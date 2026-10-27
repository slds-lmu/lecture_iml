#!/usr/bin/env bash
# Render check of the exercise sheets, run by CI (pr-exercise-check.yaml) and
# by `make check`; works from the lecture root and from exercises/:
#
#   bash exercises/_quarto/check.sh             # all chapters
#   git diff --name-only origin/main...HEAD | bash exercises/_quarto/check.sh --changed
#                                               # chapters affected by the changed files
#
# The changed files (stdin) are paths relative to the lecture root, as git prints them.
# A change to a shared file (_quarto/, _quarto.yml, _lecture.yml, quarto.mk,
# latex-math/) affects all chapters. Chapters are the folders whose Makefile
# includes quarto.mk; a folder with .qmd sheets but without such a Makefile is
# an error (make and this check would skip it). For each chapter, the committed
# outputs are rendered (make expdf solhtml solpdf), and the check fails if a
# render fails or an _ex.pdf contains "Solution", i.e. solution text outside a
# ::: {.sol} block. The rendered files stay in place (CI uploads them); nothing
# is committed.

set -euo pipefail

changed_list=""
read_changed=false
case "${1:-}" in
  --changed) read_changed=true; changed_list="$(cat)" ;;
  "") ;;
  -h|--help) sed -n '2,17p' "$0" | sed 's/^# \{0,1\}//'; exit 0 ;;
  *) echo "error: unknown argument $1 (see --help)" >&2; exit 2 ;;
esac

cd "$(dirname "${BASH_SOURCE[0]}")/.."

if [[ ! -f _lecture.yml ]]; then
  echo "exercises/_lecture.yml not found: this lecture doesn't use the exercise template, nothing to check."
  exit 0
fi

is_chapter() { [[ -f "$1/Makefile" ]] && grep -q 'quarto\.mk' "$1/Makefile"; }

all_chapters() {
  local mk
  for mk in */Makefile; do
    if is_chapter "${mk%/Makefile}"; then echo "${mk%/Makefile}"; fi
  done
}

# folders with sheets but without the chapter Makefile: make all and this check would skip them
failures=()
for dir in */; do
  dir="${dir%/}"
  if [[ "$dir" != _* ]] && compgen -G "$dir/*.qmd" > /dev/null && ! is_chapter "$dir"; then
    failures+=("$dir: has .qmd sheets but no Makefile with 'include ../quarto.mk' (make new-sheet creates it)")
  fi
done

if $read_changed; then
  shared=false
  selected=()
  while IFS= read -r file; do
    file="${file#./}"
    case "$file" in
      "") ;;
      latex-math/* | exercises/_quarto/* | exercises/_quarto.yml | exercises/_lecture.yml | \
        exercises/_lecture/* | exercises/quarto.mk)
        shared=true ;;
      exercises/*/*)
        chapter="${file#exercises/}"
        selected+=("${chapter%%/*}") ;;
    esac
  done <<< "$changed_list"
  if $shared; then
    chapters="$(all_chapters)"
  else
    chapters=""
    for chapter in $(printf '%s\n' ${selected[@]+"${selected[@]}"} | sort -u); do
      if is_chapter "$chapter"; then chapters+="$chapter"$'\n'; fi
    done
  fi
else
  chapters="$(all_chapters)"
fi

if [[ -z "${chapters//[[:space:]]/}" && ${#failures[@]} -eq 0 ]]; then
  echo "No exercise chapter affected, nothing to check."
  exit 0
fi

group() { if [[ "${GITHUB_ACTIONS:-}" == "true" ]]; then echo "::group::$1"; else echo "== $1"; fi; }
endgroup() { if [[ "${GITHUB_ACTIONS:-}" == "true" ]]; then echo "::endgroup::"; fi; }

for chapter in $chapters; do
  group "$chapter"
  if make -C "$chapter" expdf solhtml solpdf; then
    for qmd in "$chapter"/*.qmd; do
      [[ -f "$qmd" ]] || continue
      pdf="${qmd%.qmd}_ex.pdf"
      # capture first: with pipefail, grep -q could end pdftotext early and fail the pipe
      if ! text="$(pdftotext "$pdf" - 2>&1)"; then
        failures+=("$pdf: can't be read ($text)")
      elif grep -q "Solution" <<< "$text"; then
        failures+=("$pdf contains \"Solution\": solution text outside a ::: {.sol} block?")
      fi
    done
  else
    failures+=("$chapter: rendering failed (see the log above)")
  fi
  make -C "$chapter" clean > /dev/null || true
  endgroup
done

if [[ ${#failures[@]} -gt 0 ]]; then
  printf 'FAILED: %s\n' "${failures[@]}"
  exit 1
fi
echo "All checked chapters rendered, and no _ex.pdf contains a solution: $(echo $chapters)"
