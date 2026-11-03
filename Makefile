.PHONY: help update-service update-latex-math audit install-lese deps install-deps

help:
	@echo "Available targets:"
	@echo "  install-lese      : Install the lese R package from GitHub"
	@echo "                      Optional: ref=<name> to install a specific branch/tag (default: main)"
	@echo "  update-service    : Update service components from lecture_service repository"
	@echo "                      Optional: branch=<name> to use a specific branch (default: main)"
	@echo "  update-latex-math : Update latex-math from slds-lmu/latex-math repository"
	@echo "                      Optional: branch=<name> to use a specific branch (default: master)"
	@echo "  audit             : Render chapter audit report (requires lese R package)"
	@echo "  help              : Show this help message"

install-lese:
	@Rscript --quiet -e 'if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak"); pak::pkg_install("slds-lmu/lecture_service@$(or $(ref),main)")'

update-service:
	@bash scripts/update-service.sh $(branch)

update-latex-math:
	@bash scripts/update-latex-math.sh $(branch)

audit:
	Rscript --quiet -e 'lese::render_chapter_audit(run = FALSE, method = "$(or $(method),auto)")'

deps:
	@Rscript --quiet -e 'lese::check_lecture_deps(install = FALSE)'

install-deps:
	@Rscript --quiet -e 'lese::check_lecture_deps(install = TRUE)'
