#!/usr/bin/env python3
"""Copy application source into app/<name>/ from the latest release of each repository.

This script reads app/sources.yml. If the release of a repository is newer than
the `ref` value of its entry, the script downloads the archive of that release.
It then copies the paths in `include` into app/<name>/, and it replaces the
files that were there. Last, it writes the new tag into `ref`.

.github/workflows/vendor-apps.yml operates this script. The workflow uses
`git diff` to find out if the files changed. It does not use the result of this
script for that decision. This script only makes app/ correct, and prints one
line for each application that it changed.

The script uses the round-trip mode of ruamel.yaml, and not PyYAML. This mode
keeps the comments in sources.yml when the script writes the file.
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
# This indentation is the same as the hand-written style of sources.yml. The
# list markers are indented below their key. The default of ruamel puts them at
# the same level as the key, which makes a large and unnecessary difference in
# the first pull request.
yaml.indent(mapping=2, sequence=4, offset=2)


def gh_headers(token):
    headers = {"Accept": "application/vnd.github+json"}
    if token:
        headers["Authorization"] = f"Bearer {token}"
    return headers


def latest_release_tag(repo, token):
    """Give the tag of the latest release, or None if the repository has none.

    A repository with no release is an expected condition, and not an error. You
    can add an application to sources.yml before its first release. The
    application starts to synchronize by itself after a release appears. All
    other conditions raise an error. Examples are an incorrect token, an
    incorrect repository name, and a network error.
    """
    resp = requests.get(
        f"{GH_API}/repos/{repo}/releases/latest", headers=gh_headers(token), timeout=30
    )
    if resp.status_code == 404:
        # Status 404 has two meanings here. It means "this repository has no
        # release" and also "this repository does not exist". Read the
        # repository to find out which one is correct. If you do not do this, an
        # incorrect `repo` value stays at WAIT and looks correct.
        probe = requests.get(
            f"{GH_API}/repos/{repo}", headers=gh_headers(token), timeout=30
        )
        if probe.status_code == 404:
            raise RuntimeError(
                f"{repo} does not exist, or this token cannot read it. Make "
                "sure that the `repo` value in sources.yml is correct. Make "
                "sure that the repository is public, or that the token has "
                "access to it."
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
    # The archive from GitHub always contains one top-level directory. Its
    # name is <owner>-<repo>-<short sha>.
    extracted = [p for p in dest_dir.iterdir() if p.is_dir()]
    if len(extracted) != 1:
        raise RuntimeError(
            f"The archive of {repo}@{tag} must contain one top-level "
            f"directory. It contains {extracted}."
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
                    f"{repo}@{latest_tag} does not contain '{rel}'. The "
                    f"`include` list of {name} in sources.yml names this path."
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

    # Each application is independent. One application can fail because a path
    # in `include` has a new name, or because its repository became private. A
    # failure of one application must not stop the others. If it did, one
    # incorrect entry would hold back the updates of every other application.
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

    # Write the file only when an application changed. Then a run that changes
    # nothing leaves `git status` clean.
    if changes:
        with open(SOURCES_PATH, "w") as f:
            yaml.dump(data, f)

    # Stop with an error code, so that the failure is visible. Do this only
    # after the script copies every application that works. The workflow commits
    # the applications that succeeded.
    if failures:
        print(f"\n{len(failures)} application(s) failed: {', '.join(failures)}")
        sys.exit(1)


if __name__ == "__main__":
    main()
