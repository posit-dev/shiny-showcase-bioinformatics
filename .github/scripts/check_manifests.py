#!/usr/bin/env python3
"""Compare each application in app/ with its own manifest.json.

Connect Cloud reads manifest.json, and then it reads every file that the
manifest names. A file that the manifest names, but that app/<name>/ does not
contain, stops the deployment. This script finds that condition before a
deployment does.

The script makes three tests for each application:

* Every file in the manifest is on disk. This test is an error.
* The checksum of each file is the same as the manifest records. This test is a
  warning, because a difference means only that the manifest is older than the
  file.
* Every package that the code loads is in the manifest. This test is an error.
  A package that is absent from the manifest is also absent from the server,
  and the application stops at run time.

An entry in app/sources.yml can contain `allow_missing_manifest_files: true`.
This value makes the first test a warning for that application. Use it only
when the source repository writes a manifest of the complete repository, and
not of the application alone. Correct the source repository, then remove the
value.

The script makes no network requests. It reads only the files in app/.
"""

import hashlib
import json
import re
import sys
from collections import Counter
from pathlib import Path

from ruamel.yaml import YAML

REPO_ROOT = Path(__file__).resolve().parents[2]
APP_DIR = REPO_ROOT / "app"
SOURCES_PATH = APP_DIR / "sources.yml"

# R gives these packages with every installation, so a manifest does not name
# them.
BASE_PACKAGES = {
    "base", "compiler", "datasets", "graphics", "grDevices", "grid", "methods",
    "parallel", "splines", "stats", "stats4", "tcltk", "tools", "utils",
}

R_STRING = re.compile(r'"(?:\\.|[^"\\])*"' r"|'(?:\\.|[^'\\])*'")
R_COMMENT = re.compile(r"#[^\n]*")
R_LOAD = re.compile(r"\b(?:library|require)\s*\(\s*([A-Za-z][A-Za-z0-9._]*)")
R_NAMESPACE = re.compile(r"\b([A-Za-z][A-Za-z0-9._]*)\s*:::?")
# A `requireNamespace()` call shows that the package is optional. The author
# writes this call to give the application a different behavior when the
# package is absent. Therefore the manifest does not need to name it.
R_OPTIONAL = re.compile(
    r"""\b(?:requireNamespace|loadNamespace)\s*\(\s*["']?([A-Za-z][A-Za-z0-9._]*)"""
)


def waivers():
    """Read the applications that permit a file of the manifest to be absent."""
    if not SOURCES_PATH.exists():
        return set()
    data = YAML().load(SOURCES_PATH) or {}
    return {
        entry["name"]
        for entry in data.get("apps", [])
        if entry.get("allow_missing_manifest_files")
    }


def packages_of(text):
    """Give the packages that one R file loads, as (necessary, optional).

    The function removes the strings and the comments before it looks for a
    necessary package. Some applications build R code inside a string, and the
    prose of a comment can contain the word "library". Both give a name that is
    not a package.

    It looks for an optional package in the complete text, and it accepts the
    name in quotation marks. A name that is optional only removes an error, so
    a name too many is safe here.
    """
    code = R_COMMENT.sub("", R_STRING.sub('""', text))
    necessary = set(R_LOAD.findall(code)) | set(R_NAMESPACE.findall(code))
    return necessary, set(R_OPTIONAL.findall(text))


def check_app(root, waived):
    """Test one application. Give the errors and the warnings as two lists."""
    errors, warnings = [], []
    manifest_path = root / "manifest.json"

    if not manifest_path.exists():
        return ([f"{root.name}: manifest.json is absent. Connect Cloud "
                 "cannot deploy an R application with no manifest."], [])

    try:
        manifest = json.loads(manifest_path.read_text())
    except json.JSONDecodeError as exc:
        return ([f"{root.name}: manifest.json is not correct JSON: {exc}"], [])

    files = manifest.get("files", {})
    appmode = manifest.get("metadata", {}).get("appmode")
    if appmode != "shiny":
        warnings.append(f"{root.name}: appmode is {appmode!r}, and not 'shiny'.")

    missing = sorted(f for f in files if not (root / f).exists())
    if missing:
        groups = Counter(f.split("/")[0] for f in missing)
        detail = ", ".join(f"{path} ({count})" for path, count in groups.most_common(6))
        text = (
            f"{root.name}: the manifest names {len(missing)} files that are "
            f"absent from app/{root.name}/. First paths: {detail}."
        )
        if root.name in waived:
            warnings.append(text + " This application has a waiver.")
        else:
            errors.append(
                text + " Add the missing paths to `include` in sources.yml. If "
                "the paths are tests or notes, correct the source repository: "
                "put them in .rscignore, then write the manifest again."
            )

    stale = []
    for name, meta in files.items():
        path = root / name
        want = meta.get("checksum")
        if want and path.exists():
            if hashlib.md5(path.read_bytes()).hexdigest() != want:
                stale.append(name)
    if stale:
        warnings.append(
            f"{root.name}: {len(stale)} files are different from the checksum "
            "in the manifest. The source repository changed these files after "
            "it wrote the manifest."
        )

    declared = set(manifest.get("packages", {}))
    necessary, optional = set(), set()
    for r_file in root.rglob("*.R"):
        found, guarded = packages_of(r_file.read_text(errors="ignore"))
        necessary |= found
        optional |= guarded
    undeclared = sorted(necessary - declared - BASE_PACKAGES - optional)
    if undeclared:
        errors.append(
            f"{root.name}: the code loads {', '.join(undeclared)}, and the "
            "manifest does not name these packages. Connect Cloud installs only "
            "the packages in the manifest."
        )

    print(
        f"  {root.name:20} files={len(files):4}  missing={len(missing):4}  "
        f"stale={len(stale):3}  R={manifest.get('platform', '?')}"
    )
    return errors, warnings


def main():
    waived = waivers()
    apps = sorted(p for p in APP_DIR.iterdir() if p.is_dir())
    if not apps:
        print("app/ contains no application. Nothing to test.")
        return

    print(f"Testing {len(apps)} applications against their manifests:\n")
    errors, warnings = [], []
    for app in apps:
        app_errors, app_warnings = check_app(app, waived)
        errors += app_errors
        warnings += app_warnings

    for text in warnings:
        print(f"\nWARNING  {text}")
    for text in errors:
        print(f"\nERROR    {text}")

    if errors:
        print(f"\n{len(errors)} error(s).")
        sys.exit(1)
    print(f"\nAll manifests are correct. {len(warnings)} warning(s).")


if __name__ == "__main__":
    main()
