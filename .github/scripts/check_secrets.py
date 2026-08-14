#!/usr/bin/env python3
"""Find a file that must never be in this repository, by its name.

gitleaks reads the content of each file, and it finds a value that has the
shape of a key. This script is different: it reads the name of each file.

A file such as .Renviron holds a secret, but its content has no shape that a
scanner recognizes. `GEMINI_API_KEY=some-plain-text` is only a name and a
value. This condition is a real risk here, because the workflow copies
applications from other repositories, and an `include` list can name such a
file by accident.

The script reads the names that git records, and not the working directory. A
file that git ignores is not a problem, because it does not become part of the
repository.
"""

import fnmatch
import subprocess
import sys
from pathlib import Path

REPO_ROOT = Path(__file__).resolve().parents[2]

# Each rule is a pattern for the name of the file, and the reason. The script
# compares a pattern with the complete path and also with the name alone.
FORBIDDEN = [
    (".Renviron", "R reads this file at startup, and it holds environment secrets."),
    (".env", "This file holds environment secrets."),
    (".env.*", "This file holds environment secrets."),
    (".netrc", "This file holds a password for a remote machine."),
    (".pgpass", "This file holds a password for PostgreSQL."),
    (".htpasswd", "This file holds a password hash."),
    (".Rhistory", "This file records every command, and a command can hold a key."),
    ("*.pem", "This file holds a certificate or a private key."),
    ("*.key", "This file holds a private key."),
    ("*.p12", "This file holds a private key."),
    ("*.pfx", "This file holds a private key."),
    ("*.jks", "This file holds a private key."),
    ("*.keystore", "This file holds a private key."),
    ("id_rsa", "This file is a private SSH key."),
    ("id_dsa", "This file is a private SSH key."),
    ("id_ecdsa", "This file is a private SSH key."),
    ("id_ed25519", "This file is a private SSH key."),
    ("credentials", "This file holds credentials."),
    ("credentials.json", "This file holds credentials."),
    ("service-account*.json", "This file holds a key for a service account."),
    ("secrets.y*ml", "This file holds secrets."),
    ("secrets.json", "This file holds secrets."),
    (".posit", "Posit Publisher writes this directory. It records the server "
               "URL and the content GUID of a deployment."),
]

# A directory that must not be in the repository, at any level. The test above
# reads the name of the file, and the name of a file inside .posit/publish/
# gives no sign of the problem. Therefore this test reads each part of the path.
FORBIDDEN_DIRS = [
    (".posit", "Posit Publisher writes this directory. It records the server "
               "URL, the content GUID and the dashboard URL of a deployment."),
]

# A file that only shows the shape of a secret file, with no value in it.
PERMITTED = [
    "*.example",
    "*.sample",
    "*.template",
    "*.md",
]


def tracked_files():
    result = subprocess.run(
        ["git", "ls-files", "-z"],
        cwd=REPO_ROOT,
        capture_output=True,
        text=True,
        check=True,
    )
    return [p for p in result.stdout.split("\0") if p]


def is_permitted(path):
    return any(fnmatch.fnmatch(path, rule) for rule in PERMITTED)


def main():
    files = tracked_files()
    findings = []
    for path in files:
        if is_permitted(path):
            continue
        parts = Path(path).parts
        directory = next(
            ((d, reason) for d, reason in FORBIDDEN_DIRS if d in parts[:-1]), None
        )
        if directory:
            findings.append((path, directory[1]))
            continue
        name = Path(path).name
        for pattern, reason in FORBIDDEN:
            if fnmatch.fnmatch(name, pattern):
                findings.append((path, reason))
                break

    print(f"Read the names of {len(files)} files that git records.\n")
    for path, reason in findings:
        print(f"ERROR    {path}\n         {reason}")

    if findings:
        print(
            f"\n{len(findings)} file(s) must not be in this repository. Remove "
            "each file, and then change the secret that it holds. A secret in "
            "the history of git stays readable after you remove the file.\n"
            "If the file comes from apps/, remove its path from `include` in "
            "apps/sources.yml."
        )
        sys.exit(1)
    print("No file has the name of a secret file.")


if __name__ == "__main__":
    main()
