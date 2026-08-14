#!/usr/bin/env python3
"""Test apps/sources.yml before the vendor workflow uses it.

vendor_apps.py finds an incorrect entry only when it operates. This script
finds the same problems in the pull request that writes the entry.

The script makes two groups of tests.

The first group reads only the file:

* Each entry has a name, a repo, a ref and an include list.
* The name and the repo of each entry are different from all the others.
* No path in `include` is absolute, and no path contains "..".
* `include` contains manifest.json.

The second group reads the source repository through the GitHub API:

* The repository has a release. An entry with no release is not an error. The
  workflow reports it as WAIT and continues.
* Every path in `include` is in the release.
* `include` copies every file that manifest.json of the release names.

The last test is the important one. Connect Cloud reads the manifest, and then
it reads every file that the manifest names. This test finds an absent file
before the workflow copies the application.

An entry can contain `allow_missing_manifest_files: true`. This value makes the
last test a warning for that entry.
"""

import base64
import fnmatch
import json
import os
import re
import sys
from collections import Counter
from pathlib import Path

import requests
from ruamel.yaml import YAML

# Both scripts are in the same directory, so this import needs no path change.
from check_secrets import FORBIDDEN, is_permitted

REPO_ROOT = Path(__file__).resolve().parents[2]
SOURCES_PATH = REPO_ROOT / "apps" / "sources.yml"
GH_API = "https://api.github.com"

KNOWN_KEYS = {"name", "repo", "ref", "include", "allow_missing_manifest_files"}
REQUIRED_KEYS = {"name", "repo", "ref", "include"}
NAME_PATTERN = re.compile(r"^[a-z][a-z0-9-]*$")
REPO_PATTERN = re.compile(r"^[A-Za-z0-9._-]+/[A-Za-z0-9._-]+$")


def gh_headers(token):
    headers = {"Accept": "application/vnd.github+json"}
    if token:
        headers["Authorization"] = f"Bearer {token}"
    return headers


def latest_release_tag(repo, token):
    """Give the tag of the latest release, or None if the repository has none."""
    resp = requests.get(
        f"{GH_API}/repos/{repo}/releases/latest", headers=gh_headers(token), timeout=30
    )
    if resp.status_code == 404:
        probe = requests.get(
            f"{GH_API}/repos/{repo}", headers=gh_headers(token), timeout=30
        )
        if probe.status_code == 404:
            raise RuntimeError(
                f"{repo} does not exist, or this token cannot read it. Make sure "
                "that the `repo` value is correct, and that the repository is "
                "public."
            )
        probe.raise_for_status()
        return None
    resp.raise_for_status()
    return resp.json()["tag_name"]


def tree_paths(repo, tag, token):
    """Give every path in the release, and whether the list is complete."""
    resp = requests.get(
        f"{GH_API}/repos/{repo}/git/trees/{tag}",
        headers=gh_headers(token),
        params={"recursive": "1"},
        timeout=60,
    )
    resp.raise_for_status()
    data = resp.json()
    return {item["path"] for item in data.get("tree", [])}, not data.get("truncated")


def manifest_of(repo, tag, token):
    """Give manifest.json of the release, or None if the release has none."""
    resp = requests.get(
        f"{GH_API}/repos/{repo}/contents/manifest.json",
        headers=gh_headers(token),
        params={"ref": tag},
        timeout=60,
    )
    if resp.status_code == 404:
        return None
    resp.raise_for_status()
    return json.loads(base64.b64decode(resp.json()["content"]))


def check_shape(entry, index, names, repos):
    """Test one entry against the rules that need no network."""
    errors = []
    label = entry.get("name") or f"entry {index}"

    absent = REQUIRED_KEYS - set(entry)
    if absent:
        errors.append(f"{label}: these keys are absent: {', '.join(sorted(absent))}.")
        return errors

    unknown = set(entry) - KNOWN_KEYS
    if unknown:
        errors.append(
            f"{label}: these keys are unknown: {', '.join(sorted(unknown))}. "
            f"Use only {', '.join(sorted(KNOWN_KEYS))}."
        )

    name = entry["name"]
    if not isinstance(name, str) or not NAME_PATTERN.match(name):
        errors.append(
            f"{label}: `name` must be lowercase letters, numbers and hyphens. "
            "This value becomes the directory apps/<name>/."
        )
    if name in names:
        errors.append(f"{label}: two entries have the name {name}.")
    names.add(name)

    repo = entry["repo"]
    if not isinstance(repo, str) or not REPO_PATTERN.match(repo):
        errors.append(f"{label}: `repo` must have the form owner/name. It is {repo!r}.")
    if repo in repos:
        errors.append(f"{label}: two entries have the repository {repo}.")
    repos.add(repo)

    if not isinstance(entry["ref"], str):
        errors.append(
            f"{label}: `ref` must be text. Use \"\" for an application that the "
            "workflow did not copy yet."
        )

    include = entry["include"]
    if not isinstance(include, list) or not include:
        errors.append(f"{label}: `include` must be a list with one path or more.")
        return errors

    seen = Counter(include)
    for path, count in seen.items():
        if not isinstance(path, str):
            errors.append(f"{label}: `include` contains {path!r}, which is not text.")
            continue
        if count > 1:
            errors.append(f"{label}: `include` contains '{path}' {count} times.")
        if path.startswith("/") or ".." in Path(path).parts:
            errors.append(
                f"{label}: the path '{path}' is not permitted. A path must be "
                "relative to the root of the source repository, and it must not "
                "contain '..'."
            )
        # Stop a secret file before the workflow copies it. check_secrets.py
        # finds the same file after the copy, and this test is earlier.
        if not is_permitted(path):
            for pattern, reason in FORBIDDEN:
                if fnmatch.fnmatch(Path(path).name, pattern):
                    errors.append(
                        f"{label}: `include` names '{path}'. {reason} Remove "
                        "this path."
                    )
                    break
    if "manifest.json" not in include:
        errors.append(
            f"{label}: `include` does not contain manifest.json. Connect Cloud "
            "needs that file, and this repository does not write it."
        )
    return errors


def check_release(entry, token):
    """Test one entry against its release. Give the errors and the warnings."""
    errors, warnings = [], []
    name, repo = entry["name"], entry["repo"]
    include = entry["include"]

    tag = latest_release_tag(repo, token)
    if tag is None:
        print(f"  WAIT  {name:20} {repo} has no release yet")
        return errors, warnings

    paths, complete = tree_paths(repo, tag, token)
    if not complete:
        warnings.append(
            f"{name}: the file list of {repo}@{tag} is too large for one "
            "request, so this script did not test the paths."
        )
        return errors, warnings

    absent = [path for path in include if path not in paths]
    if absent:
        errors.append(
            f"{name}: {repo}@{tag} does not contain {', '.join(absent)}. The "
            "workflow stops when it copies this application."
        )

    manifest = manifest_of(repo, tag, token)
    if manifest is None:
        errors.append(
            f"{name}: {repo}@{tag} contains no manifest.json. Write the manifest "
            "in the source repository, then publish a new release."
        )
        print(f"  FAIL  {name:20} {tag}  no manifest")
        return errors, warnings

    files = manifest.get("files", {})
    prefixes = tuple(f"{path}/" for path in include)
    uncovered = sorted(
        f for f in files if f not in include and not f.startswith(prefixes)
    )
    if uncovered:
        groups = Counter(f.split("/")[0] for f in uncovered)
        detail = ", ".join(f"{path} ({count})" for path, count in groups.most_common(6))
        text = (
            f"{name}: manifest.json of {tag} names {len(uncovered)} files that "
            f"`include` does not copy. First paths: {detail}."
        )
        if entry.get("allow_missing_manifest_files"):
            warnings.append(text + " This entry has a waiver.")
        else:
            errors.append(
                text + " Add these paths to `include`. If they are tests or "
                "notes, correct the source repository: put them in .rscignore, "
                "then write the manifest again."
            )

    state = "warn" if uncovered else "ok"
    print(
        f"  {state.upper():5} {name:20} {tag}  include={len(include):3}  "
        f"manifest={len(files):4}  uncovered={len(uncovered):4}"
    )
    return errors, warnings


def main():
    token = os.environ.get("GH_TOKEN") or os.environ.get("GITHUB_TOKEN")
    data = YAML().load(SOURCES_PATH)

    if not isinstance(data, dict) or not isinstance(data.get("apps"), list):
        print("ERROR    apps/sources.yml must contain a list with the key `apps`.")
        sys.exit(1)

    errors, warnings = [], []
    names, repos = set(), set()
    for index, entry in enumerate(data["apps"], start=1):
        if not isinstance(entry, dict):
            errors.append(f"entry {index}: each entry must be a mapping.")
            continue
        errors += check_shape(entry, index, names, repos)

    if errors:
        for text in errors:
            print(f"\nERROR    {text}")
        print(f"\n{len(errors)} error(s). The network tests did not operate.")
        sys.exit(1)

    print(f"Testing {len(data['apps'])} entries against their releases:\n")
    for entry in data["apps"]:
        name = entry["name"]
        try:
            entry_errors, entry_warnings = check_release(entry, token)
            errors += entry_errors
            warnings += entry_warnings
        except Exception as exc:
            errors.append(f"{name}: {exc}")

    for text in warnings:
        print(f"\nWARNING  {text}")
    for text in errors:
        print(f"\nERROR    {text}")

    if errors:
        print(f"\n{len(errors)} error(s).")
        sys.exit(1)
    print(f"\nsources.yml is correct. {len(warnings)} warning(s).")


if __name__ == "__main__":
    main()
