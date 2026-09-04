"""Tests for deploy_matrix.py, the program that decides what deploys.

    python .github/scripts/test_deploy_matrix.py

No framework: this file is asserts and a main. It exists because the exit
status of deploy_matrix.py is the only assertion that `checks.yml` makes about
apps.yml, so the tests in that script are a contract, and a contract with no
test is a comment.

Each case below writes a small apps.yml into a temporary directory, beside an
apps/ tree, and reads what the program does with it.
"""

import io
import json
import sys
import tempfile
from pathlib import Path

sys.path.insert(0, str(Path(__file__).resolve().parent))

from deploy_matrix import matrix  # noqa: E402

TILE = """\
- category: Applications
  tiles:
{tiles}
"""


def write(root, yaml_body, apps=("genescout",), manifest=True):
    """Write apps.yml and an apps/ tree, and return the path of apps.yml."""
    for app in apps:
        app_dir = root / "apps" / app
        app_dir.mkdir(parents=True, exist_ok=True)
        if manifest:
            (app_dir / "manifest.json").write_text("{}", encoding="utf-8")
    path = root / "apps.yml"
    path.write_text(yaml_body, encoding="utf-8")
    return path


def run(yaml_body, **kwargs):
    """Return the matrix, or the SystemExit message."""
    with tempfile.TemporaryDirectory() as tmp:
        root = Path(tmp)
        path = write(root, yaml_body, **kwargs)
        try:
            return matrix(path, root=root, log=io.StringIO())
        except SystemExit as e:
            return str(e)


def case(name, yaml_body, expect, **kwargs):
    got = run(yaml_body, **kwargs)
    if isinstance(expect, list):
        assert got == expect, f"{name}: expected {expect}, got {got!r}"
    else:
        assert isinstance(got, str) and expect in got, (
            f"{name}: expected a failure mentioning {expect!r}, got {got!r}"
        )
    print(f"ok   {name}")


ID = "01a068f2-9f3b-5219-e7da-701827504414"


def main():
    # The happy path.
    case(
        "a tile with app and content_id deploys",
        TILE.format(tiles=f"    - title: genescout\n      app: genescout\n      content_id: \"{ID}\"\n"),
        [{"app": "genescout", "content_id": ID}],
    )

    # The three ways a tile does not deploy, none of which is an error.
    case(
        "a tile with no app is skipped",
        TILE.format(tiles="    - title: Something Elsewhere\n"),
        [],
    )
    case(
        "a tile with no content_id waits",
        TILE.format(tiles="    - title: genescout\n      app: genescout\n"),
        [],
    )
    case(
        "deploy false holds the tile back",
        TILE.format(tiles=f"    - title: genescout\n      app: genescout\n      content_id: \"{ID}\"\n      deploy: false\n"),
        [],
    )
    case(
        "deploy true is the default, and may be written",
        TILE.format(tiles=f"    - title: genescout\n      app: genescout\n      content_id: \"{ID}\"\n      deploy: true\n"),
        [{"app": "genescout", "content_id": ID}],
    )

    # The dangerous one. A string is true in Python, so an unchecked
    # `deploy: "false"` deploys an application that a person held back.
    case(
        "deploy as a string is an error, not a deployment",
        TILE.format(tiles=f'    - title: genescout\n      app: genescout\n      content_id: "{ID}"\n      deploy: "false"\n'),
        "must be true or false",
    )
    case(
        "deploy as a number is an error",
        TILE.format(tiles=f"    - title: genescout\n      app: genescout\n      content_id: \"{ID}\"\n      deploy: 0\n"),
        "must be true or false",
    )
    case(
        "deploy is tested even with no app",
        TILE.format(tiles='    - title: Something\n      deploy: "false"\n'),
        "must be true or false",
    )

    # The shape of each value.
    case(
        "content_id must look like a content id",
        TILE.format(tiles="    - title: genescout\n      app: genescout\n      content_id: \"01a068f2\"\n"),
        "shape of a Connect Cloud content id",
    )
    case(
        "content_id must be a string",
        TILE.format(tiles="    - title: genescout\n      app: genescout\n      content_id: 12345\n"),
        "shape of a Connect Cloud content id",
    )
    case(
        "content_id needs an app",
        TILE.format(tiles=f"    - title: genescout\n      content_id: \"{ID}\"\n"),
        "and no `app`",
    )
    case(
        "app must be usable in a hostname",
        TILE.format(tiles="    - title: genescout\n      app: Gene Scout\n"),
        "letters, digits and hyphens",
    )
    case(
        "app must not begin with a hyphen",
        TILE.format(tiles="    - title: genescout\n      app: -genescout\n"),
        "letters, digits and hyphens",
    )
    case(
        "app must be a string",
        TILE.format(tiles="    - title: genescout\n      app: 42\n"),
        "letters, digits and hyphens",
    )

    # The tree on disk has to match.
    case(
        "app must name a directory in apps/",
        TILE.format(tiles="    - title: genescout\n      app: no-such-app\n"),
        "does not exist",
    )
    case(
        "the directory must hold a manifest",
        TILE.format(tiles="    - title: genescout\n      app: genescout\n"),
        "manifest.json does not exist",
        manifest=False,
    )
    case(
        "two tiles must not name the same app",
        TILE.format(
            tiles=(
                "    - title: One\n      app: genescout\n"
                "    - title: Two\n      app: genescout\n"
            )
        ),
        "and so is",
    )

    # The shape of the file itself.
    case("the file must hold a list", "category: Applications\n", "list of categories")
    case(
        "a category must hold tiles",
        "- category: Applications\n",
        "has no `tiles`",
    )
    case(
        "tiles must be a list",
        "- category: Applications\n  tiles: genescout\n",
        "is not a list",
    )
    case(
        "a tile must be a mapping",
        "- category: Applications\n  tiles:\n    - genescout\n",
        "is not a mapping",
    )

    # Every problem is reported, and not only the first one.
    got = run(
        TILE.format(
            tiles=(
                "    - title: One\n      app: no-such-app\n"
                "    - title: Two\n      app: also-missing\n"
            )
        )
    )
    assert "no-such-app" in got and "also-missing" in got, got
    print("ok   every problem is reported at once")

    # The output is what a GitHub Actions matrix takes.
    entries = run(
        TILE.format(tiles=f"    - title: genescout\n      app: genescout\n      content_id: \"{ID}\"\n")
    )
    assert json.loads(json.dumps(entries)) == entries
    assert set(entries[0]) == {"app", "content_id"}, entries
    print("ok   the output is JSON of app and content_id alone")

    print("\nall tests pass")


if __name__ == "__main__":
    main()
