"""Write the deployment matrix of deploy-apps.yml, from apps.yml.

apps.yml is the source of truth for everything about an application, so the
workflow holds no list of applications and no content id. This script reads the
tiles and prints the ones to deploy, as the JSON that a GitHub Actions matrix
takes:

    [{"app": "genescout", "content_id": "0198f3c2-...", "account": "posit"}]

A tile deploys when it has `app` and `content_id`, and `deploy` is not false.
Every other tile is either a description with no code in this repository, an
application that nobody has published yet, or one that `deploy: false` holds
back.

The script exits non-zero when apps.yml is wrong, and that exit status is the
whole assertion that `checks.yml` makes. So the tests below are the contract,
and they are strict about the *type* of each value, not only its presence. A
mistake here deploys the wrong code, or nothing at all, and a workflow that
silently deploys nothing looks exactly like a workflow with nothing to do.

  - The file is a list of categories, and each one holds a list of tiles.
  - `pcc-account` is on the category, and it is present whenever a tile below
    it deploys. It is the account that publishes, and Connect Cloud builds the
    address of the application from it, so the workflow must not carry its own
    copy.
  - `app` is a string, and it names a directory in apps/ that holds a
    manifest.json. It must also be usable in a hostname, because the address of
    the deployment is built from it.
  - `content_id` is a string, and it has the shape of a Connect Cloud id.
  - `deploy` is a boolean. Not a string: `deploy: "false"` is a *string*, which
    is true in Python, and it would deploy an application that a person meant
    to hold back.
  - No two tiles name the same `app`.

`.github/scripts/test_deploy_matrix.py` operates each of those tests. Run it
after any change here.

Operate this script on your machine with no argument to see the matrix:

    python .github/scripts/deploy_matrix.py
"""

import json
import os
import re
import sys
from pathlib import Path

from ruamel.yaml import YAML

REPO_ROOT = Path(__file__).resolve().parents[2]

# The name of a directory in apps/, and the label in the address of the
# deployment: https://posit-<app>.share.connect.posit.cloud/. Connect Cloud
# permits letters, digits and hyphens in a vanity name, and a hostname label
# cannot begin or end with a hyphen.
APP = re.compile(r"^[a-z0-9]([a-z0-9-]*[a-z0-9])?$")

# The Connect Cloud account name. It is the other half of the hostname label,
# so it takes the same characters as `app`.
ACCOUNT = APP

# A Connect Cloud content id, for example
# 01a068f2-9d80-fb53-2bd6-e200435e2c95. The shape of a UUID, in lower case.
CONTENT_ID = re.compile(r"^[0-9a-f]{8}(-[0-9a-f]{4}){3}-[0-9a-f]{12}$")


def tiles(path, problems):
    yaml = YAML(typ="safe")
    with open(path, encoding="utf-8") as f:
        categories = yaml.load(f)

    if not isinstance(categories, list):
        raise SystemExit(f"{path}: the file must hold a list of categories.")

    found = []
    for i, category in enumerate(categories):
        if not isinstance(category, dict):
            problems.append(f"category {i + 1}: is not a mapping.")
            continue
        label = category.get("category", f"category {i + 1}")
        entries = category.get("tiles")
        if entries is None:
            problems.append(f"{label}: has no `tiles`.")
            continue
        if not isinstance(entries, list):
            problems.append(f"{label}: `tiles` is not a list.")
            continue
        for j, tile in enumerate(entries):
            if not isinstance(tile, dict):
                problems.append(f"{label}: tile {j + 1} is not a mapping.")
                continue
            found.append((tile, category))
    return found


def matrix(path, root=REPO_ROOT, log=sys.stderr):
    problems = []
    entries = []
    seen = {}

    for tile, category in tiles(path, problems):
        label = tile.get("title", "<untitled>")
        account = category.get("pcc-account")
        app = tile.get("app")
        content_id = tile.get("content_id")
        deploy = tile.get("deploy", True)

        # `deploy` is tested even on a tile with no `app`, so that a typo is a
        # failure wherever it appears, and not only where it changes something.
        if not isinstance(deploy, bool):
            problems.append(
                f"{label}: `deploy` is {deploy!r}, and it must be true or false. "
                f"A quoted \"false\" is a string, and a string does not hold "
                f"the deployment back."
            )
            deploy = False

        if content_id is not None and app is None:
            problems.append(f"{label}: has `content_id` and no `app`.")
        if app is None:
            continue

        if not isinstance(app, str) or not APP.match(app):
            problems.append(
                f"{label}: `app` is {app!r}. It must be letters, digits and "
                f"hyphens, because the address of the deployment is built from it."
            )
            continue

        app_dir = root / "apps" / app
        if not app_dir.is_dir():
            problems.append(f"{label}: `app` is '{app}', and apps/{app}/ does not exist.")
        elif not (app_dir / "manifest.json").is_file():
            problems.append(f"{label}: apps/{app}/manifest.json does not exist.")

        if app in seen:
            problems.append(f"{label}: `app` is '{app}', and so is {seen[app]}.")
        seen[app] = label

        if content_id is not None and (
            not isinstance(content_id, str) or not CONTENT_ID.match(content_id)
        ):
            problems.append(
                f"{label}: `content_id` is {content_id!r}, and it does not have "
                f"the shape of a Connect Cloud content id."
            )
            continue

        if not deploy:
            print(f"HOLD  {app}: `deploy: false` in apps.yml", file=log)
            continue
        if content_id is None:
            print(f"WAIT  {app}: no `content_id` in apps.yml yet", file=log)
            continue

        # Only a tile that deploys needs the account, so a category of tiles
        # with no code needs no `pcc-account`.
        if not isinstance(account, str) or not ACCOUNT.match(account):
            problems.append(
                f"{label}: deploys, and `pcc-account` of its category is "
                f"{account!r}. Write the Connect Cloud account there; the "
                f"address of the application is built from it."
            )
            continue

        entries.append({"app": app, "content_id": content_id, "account": account})
        print(
            f"DEPLOY {app}: {content_id} at "
            f"https://{account}-{app}.share.connect.posit.cloud/",
            file=log,
        )

    if problems:
        raise SystemExit("apps.yml:\n" + "\n".join(f"  - {p}" for p in problems))

    return entries


def main():
    entries = matrix(REPO_ROOT / "apps.yml")
    payload = json.dumps(entries, separators=(",", ":"))

    output = os.environ.get("GITHUB_OUTPUT")
    if output:
        with open(output, "a", encoding="utf-8") as f:
            f.write(f"apps={payload}\n")
    print(payload)


if __name__ == "__main__":
    main()
