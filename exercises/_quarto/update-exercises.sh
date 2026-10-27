#!/usr/bin/env bash
# Updates the exercise template in a lecture repo: copies the shared files of
# lecture_service/exercises-template/ into the lecture's exercises/.
# Run it from the lecture root or from exercises/ (`make update` does the latter).
#
#   update-exercises.sh [--target <dir>] [<branch>]
#       update from slds-lmu/lecture_service on GitHub (<branch>: default main)
#   update-exercises.sh --source <exercises-template dir> [--target <dir>]
#       update from a local copy of the template instead of GitHub
#
# <dir> defaults to exercises/ (or the current folder, if that is exercises/).
# A lecture opts in by creating <dir>/_lecture.yml (README.md#adopting-the-template);
# without it, nothing is updated.
# What an update changes (README.md#files-and-ownership):
# - _quarto.yml, quarto.mk, Makefile, .gitignore, README.md are replaced
# - _quarto/ is replaced as a whole: files that are not in the template are deleted
# - nothing else is touched (_lecture.yml, chapters, data, ...)
# Needs bash, curl and tar (only cp/rm for --source), e.g. Git Bash on Windows.

set -euo pipefail

REPO="slds-lmu/lecture_service"
SHARED_FILES=(_quarto.yml quarto.mk Makefile .gitignore README.md)

args=("$@")
source_dir=""
target=""
branch="main"
cleanup=""  # internal: temp folder to delete at the end

die() { echo "error: $*" >&2; exit 1; }

while [[ $# -gt 0 ]]; do
  case "$1" in
    --source) source_dir="${2-}"; shift ;;
    --target) target="${2-}"; shift ;;
    --cleanup) cleanup="${2-}"; shift ;;
    -h | --help) sed -n '2,18p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
    -*) die "unknown option $1 (see --help)" ;;
    *) branch="$1" ;;
  esac
  shift
done

if [[ -n "$cleanup" ]]; then
  trap 'rm -rf "$cleanup" 2> /dev/null || true' EXIT
fi

# the lecture's exercises folder
if [[ -z "$target" ]]; then
  if [[ "$(basename "$PWD")" == "exercises" ]]; then target="."; else target="exercises"; fi
fi
[[ -d "$target" ]] || die "folder '$target' not found: run this from the lecture root or its exercises/ folder, or use --target"
target="$(cd "$target" && pwd)"

[[ -f "$target/_lecture.yml" ]] || die "$target/_lecture.yml not found: this lecture doesn't use the exercise template yet; create it first (README.md#adopting-the-template)."

if [[ -z "$source_dir" ]]; then
  # Download the template, then let the downloaded (newest) version of this
  # script do the update. It runs from the temp folder, so the update doesn't
  # replace the script that is running.
  tmp="$(mktemp -d 2> /dev/null || mktemp -d -t exercises-template.XXXXXX)"
  echo "Downloading $REPO ($branch) ..."
  if ! curl -fsSL -o "$tmp/archive.tar.gz" "https://api.github.com/repos/$REPO/tarball/$branch"; then
    rm -rf "$tmp"
    die "download failed. Does the branch '$branch' exist?"
  fi
  if ! tar -xzf "$tmp/archive.tar.gz" -C "$tmp" --strip-components=1; then
    rm -rf "$tmp"
    die "could not extract the downloaded archive"
  fi
  if [[ ! -f "$tmp/exercises-template/_quarto/update-exercises.sh" ]]; then
    rm -rf "$tmp"
    die "$REPO@$branch has no exercises-template/"
  fi
  exec bash "$tmp/exercises-template/_quarto/update-exercises.sh" --source "$tmp/exercises-template" \
    --target "$target" --cleanup "$tmp"
fi

[[ -d "$source_dir" ]] || die "source folder '$source_dir' not found"
source_dir="$(cd "$source_dir" && pwd)"
[[ -f "$source_dir/_quarto.yml" && -d "$source_dir/_quarto" ]] || die "$source_dir is not an exercises-template folder"
[[ "$source_dir" != "$target" ]] || die "source and target are the same folder"

# If this script lives in the target's _quarto/, continue from a temp copy:
# the update replaces that folder.
self_dir="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
if [[ "$self_dir" == "$target/_quarto" && -z "$cleanup" ]]; then
  tmp="$(mktemp -d 2> /dev/null || mktemp -d -t exercises-template.XXXXXX)"
  cp "${BASH_SOURCE[0]}" "$tmp/update-exercises.sh"
  exec bash "$tmp/update-exercises.sh" ${args[@]+"${args[@]}"} --cleanup "$tmp"
fi

for file in "${SHARED_FILES[@]}"; do
  cp "$source_dir/$file" "$target/$file"
done
rm -rf "$target/_quarto"
cp -R "$source_dir/_quarto" "$target/_quarto"

echo "Updated $target: ${SHARED_FILES[*]}, _quarto/"
echo "Review the changes (git diff) before committing them."
