# Used in: exercises/Makefile (make deps), .github/workflows/pr-exercise-check.yaml
#
# R packages the exercise sheets need to render: knitr and rmarkdown (Quarto's
# knitr engine) plus every package loaded in an R chunk of a sheet or in an R
# helper in _lecture/ that sheets source, found as library(), require(),
# requireNamespace() or pkg::
# (like lese::extract_script_deps()). All R chunks count, whatever their chunk
# options, since e.g. eval=knitr::is_html_output() runs in the HTML render. The
# notebooks are not scanned: rendering doesn't run them.
#
#   Rscript _quarto/deps.R              # report missing packages (make deps)
#   Rscript _quarto/deps.R --install    # install them with pak (make deps install=yes, CI)

args = commandArgs(trailingOnly = TRUE)
install = "--install" %in% args
repos = c(CRAN = "https://cloud.r-project.org")

# the exercises folder: the parent of the folder this script is in
script = sub("^--file=", "", grep("^--file=", commandArgs(FALSE), value = TRUE))
exercises_dir = normalizePath(file.path(dirname(script), ".."), winslash = "/")

# lines inside ```{r ...} chunks
r_chunks = function(path) {
  lines = readLines(path, warn = FALSE, encoding = "UTF-8")
  in_chunk = FALSE
  keep = logical(length(lines))
  for (i in seq_along(lines)) {
    if (!in_chunk && grepl("^\\s*```+\\s*\\{r[ ,}]", lines[i])) {
      in_chunk = TRUE
    } else if (in_chunk && grepl("^\\s*```+\\s*$", lines[i])) {
      in_chunk = FALSE
    } else {
      keep[i] = in_chunk
    }
  }
  lines[keep]
}

packages_in = function(code) {
  text = paste(code, collapse = "\n")
  loaded = regmatches(text, gregexpr("\\b(library|require|requireNamespace)\\s*\\(\\s*[\"']?[A-Za-z][A-Za-z0-9.]*", text, perl = TRUE))[[1]]
  namespaced = regmatches(text, gregexpr("\\b[A-Za-z][A-Za-z0-9.]*(?=:::?)", text, perl = TRUE))[[1]]
  c(sub("^.*\\(\\s*[\"']?", "", loaded), namespaced)
}

is_installed = function(pkgs) {
  vapply(pkgs, function(p) nzchar(system.file(package = p)), logical(1))
}

sheets = Sys.glob(file.path(exercises_dir, "*", "*.qmd"))
helpers = Sys.glob(file.path(exercises_dir, "_lecture", "*.R"))
code = c(unlist(lapply(sheets, r_chunks)), unlist(lapply(helpers, readLines, warn = FALSE)))
base = rownames(installed.packages(priority = "base"))
pkgs = setdiff(sort(unique(c("knitr", "rmarkdown", packages_in(code)))), base)
missing = pkgs[!is_installed(pkgs)]

cat("Packages the sheets need:", paste(pkgs, collapse = ", "), "\n")
if (length(missing) == 0) {
  cat("All of them are installed.\n")
  quit(status = 0)
}
cat("Not installed:", paste(missing, collapse = ", "), "\n")
if (!install) {
  cat("Install them with `make deps install=yes`.\n")
  quit(status = 1)
}
if (!is_installed("pak")) {
  install.packages("pak", repos = repos)
}
pak::pak(missing)
