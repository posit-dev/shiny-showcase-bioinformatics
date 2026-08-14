#!/usr/bin/env python3
"""Vendor pinned app source into app/<name>/ from each repo's latest GitHub Release.

Reads app/sources.yml. For any entry whose pinned `ref` is behind that repo's
latest release, downloads the tarball at that release, copies the `include`
paths into app/<name>/ (replacing whatever was there), and updates `ref`.

Run by .github/workflows/vendor-apps.yml, which treats a plain `git diff` as
the source of truth for "did anything change" rather than trusting this
script's bookkeeping. This script's only job is to make app/ correct and
print a human-readable line per app it touched, for the PR body.

Uses ruamel.yaml's round-trip mode rather than plain PyYAML, specifically so
the comments in sources.yml survive being rewritten by a bot.
"""

import os
import shutil
import sys
import tarfile
import tempfile
from pathlib import Path

import requests
from ruamel.yaml import YAML

REPO_ROOT = Path(__file__).resolve().parents[2]
SOURCES_PATH = REPO_ROOT / "app" / "sources.yml"
GH_API = "https://api.github.com"

yaml = YAML()
yaml.preserve_quotes = True
# Matches the hand-written style in sources.yml (list markers indented under
# their key, not flush with it). ruamel's own default would flatten that on
# the first automated rewrite and make every PR's diff noisier than it needs
# to be.
yaml.indent(mapping=2, sequence=4, offset=2)


def gh_headers(token):
    headers = {"Accept": "application/vnd.github+json"}
    if token:
        headers["Authorization"] = f"Bearer {token}"
    return headers


def latest_release_tag(repo, token):
    """Latest release tag, or None if the repo hasn't cut one yet.

    "No release yet" is an expected state, not a failure: an app can sit in
    sources.yml before its first release and start syncing by itself the day
    one appears. Anything else (a bad token, a typo'd repo name, a network
    fault) still raises.
    """
    resp = requests.get(
        f"{GH_API}/repos/{repo}/releases/latest", headers=gh_headers(token), timeout=30
    )
    if resp.status_code == 404:
        # A 404 here is ambiguous: it means "no releases yet" and also "no such
        # repo". Probe the repo itself to tell them apart, otherwise a typo'd
        # `repo`, or one gone private, would sit in WAIT forever looking healthy.
        probe = requests.get(
            f"{GH_API}/repos/{repo}", headers=gh_headers(token), timeout=30
        )
        if probe.status_code == 404:
            raise RuntimeError(
                f"{repo} does not exist, or this token cannot see it. Check the "
                "`repo` spelling in sources.yml, and that the repo is public or "
                "the token has access."
            )
        probe.raise_for_status()
        return None
    resp.raise_for_status()
    return resp.json()["tag_name"]


def download_tarball(repo, tag, token, dest_dir):
    resp = requests.get(
        f"{GH_API}/repos/{repo}/tarball/{tag}",
        headers=gh_headers(token),
        timeout=120,
        stream=True,
    )
    resp.raise_for_status()
    tarball_path = dest_dir / "src.tar.gz"
    with open(tarball_path, "wb") as f:
        for chunk in resp.iter_content(chunk_size=1 << 20):
            f.write(chunk)
    with tarfile.open(tarball_path) as tf:
        tf.extractall(dest_dir)
    # GitHub's tarball endpoint always wraps the checkout in exactly one
    # top-level directory (named <owner>-<repo>-<short sha>).
    extracted = [p for p in dest_dir.iterdir() if p.is_dir()]
    if len(extracted) != 1:
        raise RuntimeError(
            f"expected exactly one top-level directory in {repo}@{tag}'s tarball, "
            f"found {extracted}"
        )
    return extracted[0]


def vendor_one(entry, token):
    name, repo = entry["name"], entry["repo"]
    current_ref = entry.get("ref") or None
    latest_tag = latest_release_tag(repo, token)

    if latest_tag is None:
        print(f"WAIT   {name}: {repo} has no GitHub Release yet")
        return None

    if latest_tag == current_ref:
        print(f"SKIP   {name}: already at {latest_tag}")
        return None

    with tempfile.TemporaryDirectory() as tmp:
        src_root = download_tarball(repo, latest_tag, token, Path(tmp))
        dest_root = REPO_ROOT / "app" / name

        for rel in entry["include"]:
            src = src_root / rel
            dest = dest_root / rel
            if not src.exists():
                raise RuntimeError(
                    f"{repo}@{latest_tag} has no '{rel}' (listed in sources.yml "
                    f"include for {name})"
                )
            if dest.exists():
                shutil.rmtree(dest) if dest.is_dir() else dest.unlink()
            dest.parent.mkdir(parents=True, exist_ok=True)
            if src.is_dir():
                shutil.copytree(src, dest)
            else:
                shutil.copy2(src, dest)

    old = current_ref or "(none)"
    entry["ref"] = latest_tag
    print(f"BUMPED {name}: {old} -> {latest_tag}")
    return f"- **{name}**: {old} to {latest_tag}"


def main():
    token = os.environ.get("GH_TOKEN") or os.environ.get("GITHUB_TOKEN")
    data = yaml.load(SOURCES_PATH)

    # Each app is isolated. One app failing (a renamed include path, a repo
    # gone private) must not stop the others from syncing, or a single bad
    # entry silently holds back every other app's updates.
    changes, failures = [], []
    for entry in data["apps"]:
        name = entry.get("name", "<unnamed>")
        try:
            change = vendor_one(entry, token)
            if change:
                changes.append(change)
        except Exception as exc:
            print(f"ERROR  {name}: {exc}")
            failures.append(name)

    # Only touch the file at all when something actually changed, so a no-op
    # run leaves a clean `git status`.
    if changes:
        with open(SOURCES_PATH, "w") as f:
            yaml.dump(data, f)

    # Still exit non-zero so a failure is visible, but only after every healthy
    # app has been vendored; the workflow commits whatever succeeded.
    if failures:
        print(f"\n{len(failures)} app(s) failed: {', '.join(failures)}")
        sys.exit(1)


if __name__ == "__main__":
    main()
