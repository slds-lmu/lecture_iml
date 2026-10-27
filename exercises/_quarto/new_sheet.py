"""Create a new exercise sheet from the template _quarto/new-sheet.qmd.

Run from the exercises folder (`make new-sheet` does that):

    python3 _quarto/new_sheet.py --chapter CHAPTER --sheet SHEET [--no-code]

Creates in <chapter>/ (README.md#writing-a-new-sheet):
- <sheet>.qmd: the sheet template, with notebook links (GitHub repo and default
  branch of this lecture) and embeds filled in
- sol_<sheet>_R.ipynb, sol_<sheet>_py.ipynb: the notebooks with the code of the
  sheet (not with --no-code, the variant for pen-and-paper sheets)
- Makefile (`include ../quarto.mk`), unless the folder has one
Existing files are never overwritten.

In the template, {{name}} is replaced by its value. Lines between {{#code}} and
{{/code}} are kept for sheets with code, lines between {{^code}} and {{/code}}
for pen-and-paper sheets.
"""

import argparse
import json
import re
import subprocess
import sys
import uuid
from pathlib import Path

TEMPLATE = Path(__file__).resolve().parent / "new-sheet.qmd"
PLACEHOLDER = re.compile(r"\{\{(\w+)\}\}")
SECTION = re.compile(r"^\{\{([#^])(\w+)\}\}\n(.*?)^\{\{/\2\}\}\n", re.M | re.S)

# kernels of the notebooks (README.md#notebooks); Jupyter adds language_info when they run
KERNELSPECS = {
    "R": {"display_name": "R", "language": "R", "name": "ir"},
    "py": {"display_name": "Python 3 (ipykernel)", "language": "python", "name": "python3"},
}
# example code of the cells the template embeds, by label
CODE = {
    "R": {"import": "library(stats)", "data": "x = c(2, 4, 9)", "mean": "mean(x)"},
    "py": {"import": "import statistics", "data": "x = [2, 4, 9]", "mean": "statistics.mean(x)"},
}


def git(lecture_dir, *args):
    """Output of a git command in the lecture repo, or "" if it fails."""
    try:
        return subprocess.run(
            ["git", "-C", str(lecture_dir), *args], capture_output=True, text=True, check=True
        ).stdout.strip()
    except (OSError, subprocess.CalledProcessError):
        return ""


def fill(text, values, flags):
    """Keep or drop the {{#name}}/{{^name}} sections, then replace the {{name}}
    placeholders; unknown names are an error in the template."""
    text = SECTION.sub(lambda m: m.group(3) if flags[m.group(2)] == (m.group(1) == "#") else "", text)
    return PLACEHOLDER.sub(lambda m: values[m.group(1)], text)


def notebook(lang):
    """A sol_ notebook in Jupyter's format: an import cell, then per exercise a
    "Solution" heading and the cells the template embeds; fresh cell ids."""
    code = CODE[lang]
    cells = [
        ("code", f"#| label: import\n\n{code['import']}"),
        ("markdown", "## Solution 1: [Title]"),
        ("code", f"#| label: data\n\n{code['data']}"),
        ("code", f"#| label: mean\n\n{code['mean']}"),
        ("markdown", "## Solution 2: [Title]"),
    ]
    nb = {
        "cells": [
            {
                "cell_type": kind,
                "id": uuid.uuid4().hex[:8],
                "metadata": {},
                "source": text.splitlines(keepends=True),
                **({"execution_count": None, "outputs": []} if kind == "code" else {}),
            }
            for kind, text in cells
        ],
        "metadata": {"kernelspec": KERNELSPECS[lang]},
        "nbformat": 4,
        "nbformat_minor": 5,
    }
    return json.dumps(nb, indent=1, sort_keys=True, ensure_ascii=False) + "\n"


def main():
    parser = argparse.ArgumentParser(description="Create a new exercise sheet from the template.")
    parser.add_argument("--chapter", required=True, help="chapter folder, as in the slides, e.g. supervised-regression")
    parser.add_argument("--sheet", required=True, help="sheet name, lowercase with underscores, e.g. regression_2")
    parser.add_argument("--no-code", action="store_true", help="pen-and-paper sheet: no notebooks, no code tabs")
    args = parser.parse_args()

    if not re.fullmatch(r"[a-z0-9][a-z0-9-]*", args.chapter):
        sys.exit(f"error: chapter '{args.chapter}': use lowercase letters, digits and hyphens")
    if not re.fullmatch(r"[a-z0-9][a-z0-9_]*", args.sheet):
        sys.exit(f"error: sheet '{args.sheet}': use lowercase letters, digits and underscores")
    if not Path("_lecture.yml").exists():
        sys.exit("error: run this in the exercises folder of a lecture that uses the template (no _lecture.yml here)")

    # GitHub repo (from origin, else the folder name) and default branch, for the notebook links
    lecture_dir = Path.cwd().parent
    origin = git(lecture_dir, "remote", "get-url", "origin")
    values = {
        "repo": re.sub(r"\.git$", "", re.split(r"[/:]", origin.rstrip("/"))[-1]) or lecture_dir.name,
        "branch": git(lecture_dir, "symbolic-ref", "--short", "refs/remotes/origin/HEAD").split("/", 1)[-1] or "main",
        "chapter": args.chapter,
        "sheet": args.sheet,
    }
    flags = {"code": not args.no_code}

    chapter_dir = Path(args.chapter)
    files = {chapter_dir / f"{args.sheet}.qmd": fill(TEMPLATE.read_text(encoding="utf-8"), values, flags)}
    if not args.no_code:
        for lang in ("R", "py"):
            files[chapter_dir / f"sol_{args.sheet}_{lang}.ipynb"] = notebook(lang)

    existing = [p.as_posix() for p in files if p.exists()]
    if existing:
        sys.exit("error: these files exist already, nothing was created: " + ", ".join(existing))

    chapter_dir.mkdir(exist_ok=True)
    makefile = chapter_dir / "Makefile"
    if not makefile.exists():
        files[makefile] = "include ../quarto.mk\n"
    elif "quarto.mk" not in makefile.read_text(encoding="utf-8"):
        print(f"warning: {makefile} doesn't include ../quarto.mk, so make can't build the new sheet there")
    for path, text in files.items():
        with open(path, "w", encoding="utf-8", newline="\n") as f:
            f.write(text)
        print(f"created {path.as_posix()}")

    print(
        f"\nNext steps (README.md): write the tasks into {args.sheet}.qmd"
        + ("" if args.no_code else f", the code into sol_{args.sheet}_R.ipynb and sol_{args.sheet}_py.ipynb (then `make notebooks` in the chapter folder)")
        + f", and build with `make -C {args.chapter} all`."
    )


if __name__ == "__main__":
    main()
