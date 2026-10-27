# Targets for all exercise sheets of the lecture; run them in exercises/.
# Synced from lecture_service (exercises-template/, see README.md); don't edit it here.
# The targets of a single chapter are in quarto.mk (e.g. make -C trees all).

.PHONY: help all new-sheet deps check update

# python3, or python where python3 is missing or only the Windows Store stub
PYTHON ?= $(shell python3 -c "" > /dev/null 2>&1 && echo python3 || echo python)
RSCRIPT ?= Rscript

# chapter folders: those whose Makefile includes quarto.mk (not data/, *-tex/, ...)
CHAPTERS := $(patsubst %/Makefile,%,$(shell grep -l 'quarto\.mk' */Makefile 2> /dev/null))

help:
	@echo "Targets for all exercise sheets (run in exercises/):"
	@echo "  all        build the committed outputs of every chapter, one after the other"
	@echo "             (in a single chapter: make -C <chapter> all, see quarto.mk)"
	@echo "  new-sheet  create a sheet and its notebooks from the template:"
	@echo "             make new-sheet chapter=<chapter> sheet=<sheet> [code=no]"
	@echo "  deps       list the R packages the sheets need; install=yes installs missing ones"
	@echo "  check      render check as in CI, for all chapters"
	@echo "  update     update the shared files from lecture_service: [branch=main]"
	@echo "See README.md for the rules and details."

# one chapter after the other: they share ../.quarto, which clean deletes
all:
	@for dir in $(CHAPTERS); do \
		echo "== $$dir"; \
		"$(MAKE)" -C "$$dir" all || exit 1; \
	done

new-sheet:
	@if [ -z "$(chapter)" ] || [ -z "$(sheet)" ]; then \
		echo "usage: make new-sheet chapter=<chapter> sheet=<sheet> [code=no]"; \
		exit 1; \
	fi
	"$(PYTHON)" _quarto/new_sheet.py --chapter "$(chapter)" --sheet "$(sheet)" $(if $(filter no,$(code)),--no-code)

deps:
	"$(RSCRIPT)" _quarto/deps.R $(if $(filter yes,$(install)),--install)

check:
	bash _quarto/check.sh

update:
	bash _quarto/update-exercises.sh --target . $(branch)
