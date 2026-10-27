# Exercise sheets (Quarto template)

This folder is the shared setup of the exercise sheets of the SLDS lectures. It is copied into a lecture's `exercises/` folder ([Adopting the template](#adopting-the-template)). This document defines the format of the sheets: what a sheet contains, how its source is written, which notebooks belong to it and which outputs are built from it. Authors and LLM agents follow it when they write a new sheet or convert an existing one from LaTeX, Rnw or Rmd.

Where to start:

- Writing a sheet: [What you get](#what-you-get) and [Writing a new sheet](#writing-a-new-sheet); then [Markup rules](#markup-rules), [Notebooks](#notebooks) and [Outputs and build](#outputs-and-build) as reference, and the [checklist](#checklist-before-committing) before committing.
- Converting a sheet: in addition, [Converting existing sheets](#converting-existing-sheets).
- Setting up a lecture: [Adopting the template](#adopting-the-template).
- Changing the template: [Files and ownership](#files-and-ownership), the [design decision](#design-decision-code-lives-in-notebooks) and the comments in the files of `_quarto/`.

Further:

- **See it live (I2ML):** CART sheet as [HTML](https://html-preview.github.io/?url=https://raw.githubusercontent.com/slds-lmu/lecture_i2ml/refs/heads/master/exercises/trees/trees.html), [exercise PDF](https://github.com/slds-lmu/lecture_i2ml/blob/master/exercises/trees/trees_ex.pdf), [PDF with solutions](https://github.com/slds-lmu/lecture_i2ml/blob/master/exercises/trees/trees_all.pdf) and [source](https://github.com/slds-lmu/lecture_i2ml/blob/master/exercises/trees/trees.qmd)
- **How to apply:** these rules are binding. Where existing sheets differ from them, the rules win; do not copy deviations. If a sheet has to deviate, give the reason in the PR description.
- **Changing the template:** only in lecture_service (`exercises-template/`), never in a lecture's copy, which `make update` overwrites ([Files and ownership](#files-and-ownership)).
- **Open points** (in-class sheets, infrastructure) are collected in the lecture_service wiki, page [Exercises](https://github.com/slds-lmu/lecture_service/wiki/Exercises#open-points).
- **Origin:** generalized from I2ML's `EXERCISE_SPEC.md` (branch `optimize_html`, commit `48c2fe43`). I2ML still uses its own copy of the setup, and its `EXERCISE_SPEC.md` stays authoritative for I2ML until it switches to the template; the differences are listed in the wiki, page [Exercises](https://github.com/slds-lmu/lecture_service/wiki/Exercises#i2ml).

## What you get

- Covers the weekly exercise sheets of a lecture. Out of scope: slides (stay LaTeX/beamer), exams. In-class sheets: migration not decided (see the open points in the wiki).
- Goals: one source file per sheet, a print-quality PDF, an interactive HTML with collapsible solutions, R and Python side by side.
- All R and Python code lives in one Jupyter notebook per language and is embedded into the sheet ([design decision](#design-decision-code-lives-in-notebooks)).

One `.qmd` produces an interactive HTML and two PDFs.

- **HTML** (on screen, for the exercise session and self-study):
  - Solutions and hints are collapsed. Open them one by one; "Show All Solutions" opens (or closes) every solution at once, including everything nested in a solution, but no hints outside solutions.
  - Code appears in R and Python tabs. Choosing a language in one tabset switches every tabset on the page, and the choice carries over to the next sheet (Quarto's tabset group `language`, remembered by the browser). "Only R" / "Only Python" do the same; pen-and-paper sheets have no language tabs, and the two buttons are hidden there.
  - "Print" prints (or saves as PDF) exactly what is visible. Students pick their language and the solutions they want opened, then print their own version; toolbar, table of contents and sidebars are left out.
  - Table of contents and a "Notebooks" list linking to the sheet's notebooks on GitHub (to run the code of the tasks and solutions).
  - A single self-contained file (`embed-resources: true`) that can be passed on as one file. Formulas need internet: MathJax loads from jsDelivr when the page opens. The text uses the reader's system font.
- **PDFs** (handout, website):
  - `_ex.pdf`: tasks and hints, no solutions.
  - `_all.pdf`: tasks, hints and all solutions, both languages one after the other.
  - Without the `solution` profile, solutions are removed from the output, not just hidden, so solution-free versions can be published before the solution release.

## Files and ownership

- Per chapter, in `exercises/<chapter>/` (`<chapter>` as in the slides, e.g. `supervised-regression`):
  - `<sheet>.qmd`: the single source (tasks, hints, solutions, embeds)
  - `sol_<sheet>_R.ipynb`, `sol_<sheet>_py.ipynb`: the code of the sheet (code handed out with the tasks, solution code), run and saved with outputs ([Notebooks](#notebooks)); pen-and-paper sheets have none
  - `figure/`: static images used by the sheet
  - `Makefile`: one line, `include ../quarto.mk`
  - committed outputs: `<sheet>.html`, `<sheet>_ex.pdf`, `<sheet>_all.pdf` ([Outputs and build](#outputs-and-build))
- Names: `<sheet>` in lowercase with underscores (e.g. `classification_2`); its notebooks are named after it. (Older sheets may use a shorter name for their notebooks, e.g. I2ML's `evaluation_1` with `sol_eval_1_R.ipynb`.)
- Shared files in `exercises/`, and who owns them:

  | File or folder | Belongs to | `make update` |
  |---|---|---|
  | `_quarto.yml` (formats, theme, filters), `quarto.mk` (targets of a chapter), `Makefile` (targets for all sheets), `.gitignore`, `README.md` (this file) | template | replaces them |
  | `_quarto/`: sheet header with math macros and toolbar (`header.qmd`), CSS (`theme.scss`), LaTeX preamble, Lua filters (`filters.lua`), helper scripts, template for new sheets (`new-sheet.qmd`) | template | replaces the folder; files that are not in the template are deleted |
  | `_lecture.yml`: course subtitle and further lecture-wide Quarto options ([Adopting the template](#adopting-the-template)) | lecture | never touches it |
  | `_lecture/` (optional): lecture-specific files referenced in `_lecture.yml` or sourced by sheets, e.g. extra CSS or R helpers | lecture | never touches it |
  | chapter folders, `data/`, environment files, legacy folders (e.g. `*-tex/`, `chapter.mk`) | lecture | never touches them |

- Nothing lecture-specific goes into `_quarto/`; it belongs in `_lecture.yml` or `_lecture/`.

## Writing a new sheet

The template of a sheet is `_quarto/new-sheet.qmd`. Start every sheet from it with `make new-sheet`, not by copying another sheet, and keep its structure:

1. In `exercises/`: `make new-sheet chapter=<chapter> sheet=<sheet>` (names as in [Files and ownership](#files-and-ownership); `code=no` for a pen-and-paper sheet). It creates `<chapter>/<sheet>.qmd` with notebook links and embeds filled in, the two notebooks with example cells, and the chapter's `Makefile`; it never overwrites a file. (The template's `{{…}}` syntax is explained in `_quarto/new_sheet.py`.)
2. In the `.qmd`, replace the placeholders, which are written as `[…]` so the new sheet renders right away, and leave out what the sheet does not need (rules in [Markup rules](#markup-rules)). The code tabset under b) is only for code handed out with a task (e.g. data generation); most code tasks have code only in the solution.
3. In the notebooks, replace the example cells with the `import` cell and the labeled cells the sheet embeds, with the same labels in R and Python ([Notebooks](#notebooks)).
4. In the chapter folder: `make notebooks` runs the notebooks and saves their outputs; `make <sheet>` renders the solution HTML for a quick look; `make all` builds the three outputs and copies the PDFs to `exercises-pdf/` ([Outputs and build](#outputs-and-build)).
5. In `exercises/`: `make check`, the render check of CI.
6. Go through the [checklist](#checklist-before-committing), then commit the `.qmd`, the notebooks, `figure/` and the outputs, including the copies in `exercises-pdf/`.

## Markup rules

### YAML header

- Only `title` and, if the sheet embeds notebooks, `notebook-view`. No `format:` block and no other per-sheet options: a sheet-level `format:` changes what `quarto render` builds.
- `title: "Exercise <N> -- <Topic>"`: `<N>` is the number of the sheet in the course; two hyphens.
- The subtitle (course name linked to the course website) is set once for all sheets in `_lecture.yml` ([Adopting the template](#adopting-the-template)).
- `notebook-view` (filled in by `make new-sheet`): one entry per notebook the sheet embeds, in the order `sol_<sheet>_R.ipynb`, `sol_<sheet>_py.ipynb`, with the titles of the template and the URL `https://github.com/slds-lmu/<repo>/blob/<branch>/exercises/<chapter>/<notebook>` (`<repo>`: the lecture repo, e.g. `lecture_i2ml`; `<branch>`: its default branch, `master` or `main`).
  - Why: `notebook-links: global` in `_quarto.yml` lists every embedded notebook in the HTML's "Notebooks" section; `notebook-view` gives each entry its title and a link to GitHub. Without it, the link points to a generated `<notebook>-preview.html`, which is not part of the self-contained HTML (and `make clean` deletes it): a dead link for students. An embedded notebook without an entry is missing from the list.
  - Quarto 1.7 ignores `url` and still links to the preview file; render with Quarto 1.10 ([Outputs and build](#outputs-and-build)).

### Sheet header

- First line after the YAML header, verbatim as in the template: `{{< include /_quarto/header.qmd >}}`. It loads the math macros for the HTML (for the PDF, Pandoc expands them) and the toolbar. The leading `/` means the project root (`exercises/`), so the line is the same at any folder depth.

### Libraries hint

- Sheets with code: directly after the sheet header, before Exercise 1, as in the template: `::: {.hint label="Hint: Useful libraries"}` with a tabset that embeds the `import` cell of both notebooks.

### Exercises

- Heading `## Exercise <N>: <Title>`, numbered 1, 2, … in order of appearance. These are the only headings; `### R` / `### Python` inside tabsets and title lines inside callouts are not headings.
- Directly after the heading: the learning goals callout of the template with 1 to 4 goals as a numbered list (`1.`, `2.`), each starting with a verb ("Compute …", "Explain …").
- Then the introduction (setting, data, notation), then the subtasks.
- Exercise for only part of the students (e.g. one of I2ML's lecture groups): directly after the heading, before the learning goals, `::: {.callout-tip icon=false title="<for whom>"}`, e.g. `title="The whole exercise is only for lecture group A!"`.
- Bonus exercise: directly after the heading, `::: {.callout-tip icon=false title="Bonus exercise -- will not be discussed in the tutorial session"}` with one sentence on who it is for.
- Other callouts, mainly inside solutions: `callout-tip` for procedures and background, `callout-warning` for pitfalls and caveats.

### Subtasks

- `a)`, `b)`, `c)` at the start of the line, one per task. Letters continue through the exercise, although solution blocks interrupt the list, and restart at `a)` in the next exercise (and in each tab of an exercise written once for R and once for Python).
- Everything that belongs to a subtask (further paragraphs, sub-items, tables, hints) is indented 3 spaces, aligned with the text after `a) `. Exception: embeds and figures go into an `.indent` block ([Code](#code), [Figures](#figures-and-tables)), and whatever of the subtask follows such a block goes into the same block, not indented.
- Sub-items: `(i)`, `(ii)`, … or `-` bullets, indented 3 spaces, i.e. a nested list of the subtask. They get no solution blocks of their own ([Solutions](#solutions)).
- Text that introduces the following subtasks (new data, a new assumption) stands at the start of the line, between two subtasks.

### Hints

- Inside the subtask, indented 3 spaces (like the subtask's other content): `::: {.hint}`, the hint, `:::`, with a blank line before the opening fence.
- Labels: "Hint" (the default) for subtasks, `::: {.hint label="Hint: Useful libraries"}` for the sheet-level hint. No other labels (`R Hint`, `Hint 1`, …).
- Language-specific hint: a tabset with both tabs (`### R` / `### Python`, as under [Code](#code)) inside the hint.
- Hints are part of the task: they appear expanded in `_ex.pdf` and must not give away the solution.

### Solutions

- Every subtask has exactly one solution block, `::: {.sol}` … `:::`, directly after the subtask (after its hint), starting at the beginning of the line (as in the template). Not `.solution`: Quarto reserves that class for proofs.
- No solution content outside this block; anything outside also ends up in `_ex.pdf`.
- Do not indent text inside the block (the block does that); 4 leading spaces turn Markdown into a code block.
- Sub-items are answered in the subtask's solution, in order and with their labels: as a list (`(i) …`, `(ii) …`) if every answer is one paragraph, otherwise with bold labels at the start of a paragraph (`**(i)** …`), because math, figures and embeds after an answer would have to be indented to the list item, and embeds cannot be.
- Text that applies to both languages (derivations, interpretation) appears once, outside the tabs; tabs hold only code and language-specific remarks.
- Optional extras inside a solution (an alternative derivation, an illustration): a nested `<details>` with a bold, descriptive summary, e.g. `<summary>**Alternative: componentwise derivation**</summary>`.

### Code

- All code comes from notebooks ([design decision](#design-decision-code-lives-in-notebooks)): `{{< embed <notebook>.ipynb#<label> echo=true >}}`. Always `echo=true`; without it only the output is shown.
- Always in a tabset `::: {.panel-tabset group="language"}` with exactly two tabs, `### R` first, then `### Python`. Every task that asks for code has both tabs; language-specific remarks without the other language go outside a tabset. The group switches all such tabsets together by their tab titles ([What you get](#what-you-get)), so the titles must be exactly `R` and `Python`; other tabsets (e.g. "Quick solution" / "Detailed solution") get no group.
- Code handed out with a task: labeled cells of the notebooks, in a tabset wrapped in `::: {.indent}`, outside the solution block ([Layout](#layout) explains why). Their output appears in `_ex.pdf` too; if it would give away part of a solution, add `#| output: false` to the cell, and the sheet shows only the code.
- Two embeds directly after each other fail; put a line of text between them that says what the next cell does.
- Nothing is executed at render; embeds show the code and output saved in the notebook.
- No `{python}` chunks. `{r}` chunks only for figures (below), never for solution code.

### Figures and tables

- Generated figure: a hidden R chunk; load packages inside it and set a seed if it draws random numbers:

  ````md
  ```{r, echo=FALSE, message=FALSE, warning=FALSE, fig.align="center", fig.height=3.5, fig.width=3.5}
  library(ggplot2)
  <plot code>
  ```
  ````

- Static image: a PNG in `figure/`, included as `![<caption, may be empty>](figure/<name>.png){width=50%}`. Convert PDF figures to PNG; browsers do not display PDF images.
- Inside a subtask, put a figure chunk into `::: {.indent}`, like an embed ([Layout](#layout) explains why). Inside a solution, nothing extra is needed.
- Tables: Markdown pipe tables; math in cells is fine.

### Math

- Inline `$…$`; display math as `$$…$$` or `\begin{align*} … \end{align*}`, with a blank line before and after.
- Only the macros of the lecture's `latex-math/`, the same as in the slides (`\xv`, `\risk`, `\Np`, …): `_quarto/header.qmd` includes `latex-math/combined.qmd`, which latex-math generates from its `.tex` files. Before using a macro, check that it is defined in `latex-math/combined.qmd`; otherwise write it out (e.g. `\mathcal{N}`). No `\newcommand` in a sheet.
- `\bm` and `\mathds` are mapped to `\boldsymbol` and `\mathbb` (in `header.qmd`): MathJax and Quarto's LaTeX template have neither package.
- Macros built on `\ifthenelse` (e.g. `\pert`) work neither in MathJax nor in the PDF; write such formulas out.
- No `\label` / `\ref`; refer to equations and figures in words ("the model from a)").

### Layout

- Block types: only `::: {.sol}` ([Solutions](#solutions)), `::: {.indent}` ([Code](#code), [Figures](#figures-and-tables)) and `::: {.hint}` ([Hints](#hints)). `.sol` and `.indent` are written at the start of the line and indent themselves by one list level in HTML and PDF. No hand-made `content-visible` / `<details>` wrappers around solutions or hints (only extras inside a solution use `<details>`).
- Why `.indent`: Quarto pastes the cells of `{{< embed >}}` into the Markdown before Pandoc reads it, without the indentation of the shortcode, so an indented embed ends the subtask's list item and breaks a tabset around it. A figure chunk would stay in the list item, but in the PDF a tall figure inside the list can push a whole exercise to the next page.
- Implementation: `_quarto/filters.lua` turns the blocks into `<details>` (HTML) and mdframed environments (PDF) and keeps labels, small tables and caption lines with what follows; `_quarto/theme.scss` (flatly's light theme only, system font) and `_quarto/pdf-preamble.tex` hold the CSS (indentation, list numbering, toolbar, printing, code font size) and the LaTeX setup. Their comments explain the details.
- No raw LaTeX for layout (`\vspace`, `\newpage`, `\hfill`, `\\`, `\noindent`); the HTML drops it. If the PDF needs a page break, use `{{< pagebreak >}}`.
- Links: `[text](url)`. Data files link to `https://github.com/slds-lmu/<repo>/blob/<branch>/exercises/data/<file>`.
- Language: English.

## Notebooks

- **Kernels:** R notebooks use `ir` (IRkernel), Python notebooks the standard kernel `python3` ("Python 3 (ipykernel)"); `make new-sheet` sets them. `make notebooks` runs each notebook with the kernel it declares, so a wrong kernel shows up there; select the right one in Jupyter or VS Code and save the notebook.
- **Labels:** every cell the sheet embeds starts with `#| label: <label>` (no space between `#` and `|`). Labels are lowercase with hyphens and unique within a notebook. Corresponding R and Python cells get the same label.
- **Content** of `sol_<sheet>_R.ipynb` and `sol_<sheet>_py.ipynb`:
  - an import cell at the top, labeled `import` (embedded by the libraries hint)
  - per exercise a Markdown cell `## Solution <N>: <Title>`, followed by the cells of the exercise in the order of the sheet: code handed out with a task and solution code
  - run top to bottom and saved with outputs before the sheet is rendered: `make notebooks` in the chapter folder runs `jupyter nbconvert --to notebook --execute --inplace --config ../_quarto/compact_png.py` on every `sol_` notebook of the chapter. The config stores the figures as 256-color PNGs: much smaller notebooks and HTML, no visible change. For a notebook saved from Jupyter or VS Code, run that command without `--execute`.
- **Further notebooks:** a lecture may hand out more notebooks, e.g. I2ML's `ex_` notebooks for students; they are embedded and listed in `notebook-view` like the `sol_` notebooks.
- **Data:** read from `../data/<file>` (relative to the notebook). R may use built-in data sets of its packages (e.g. mlr3's `tsk("bike_sharing")`). New data sets go into `exercises/data/`.
- **Randomness:** set a seed in every notebook that draws random numbers. R and Python draw different numbers even with the same seed, so both languages use the same data set and the same settings (sample sizes, grids, hyperparameters), and the solution text must not depend on exact random values. Where it does, store the data in `exercises/data/` and read it in both languages.

## Outputs and build

- Profiles (`quarto render --profile ...`; no profile files needed): none gives tasks and hints; `solution` adds the solutions and the "Show All Solutions" button.
- Committed outputs per sheet:
  - `<sheet>.html` (`make solhtml`): tasks, hints, collapsible solutions, toolbar; for the session on screen
  - `<sheet>_ex.pdf` (`make expdf`): tasks and hints; handout, website before solution release
  - `<sheet>_all.pdf` (`make solpdf`): with solutions; website after release
- Targets, in a chapter folder (`quarto.mk`) or in `exercises/` (`Makefile`, `make help` lists them):

  | Target | Where | Does |
  |---|---|---|
  | `make all` | chapter | builds the three outputs of every sheet in the folder and copies the PDFs to `exercises-pdf/`, which the course website links to; stops at the first failed render |
  | `make <sheet>` | chapter | renders one solution HTML for a quick check and cleans up (a name without `.qmd` file is an error) |
  | `make exhtml` | chapter | the exercise-only HTML `<sheet>_ex.html`, for checking (ignored by git) |
  | `make notebooks` | chapter | runs the `sol_` notebooks and saves them with their outputs ([Notebooks](#notebooks)) |
  | `make clean` | chapter | removes temporary files |
  | `make all` | `exercises/` | `make all` in every chapter, one after the other; run chapters one after another in any case, since `clean` deletes the shared `exercises/.quarto` |
  | `make new-sheet chapter=<chapter> sheet=<sheet>` | `exercises/` | creates a sheet ([Writing a new sheet](#writing-a-new-sheet)); `code=no` for a pen-and-paper sheet |
  | `make deps` | `exercises/` | lists the R packages the sheets need to render (knitr, rmarkdown, the packages of the figure chunks); `install=yes` installs the missing ones |
  | `make check` | `exercises/` | the render check of CI (`_quarto/check.sh`): renders `_ex.pdf`, HTML and `_all.pdf` of every chapter (CI: of the chapters a pull request affects); fails if a render fails, an `_ex.pdf` contains "Solution", or a folder with sheets has no chapter `Makefile` |
  | `make update` | `exercises/` | updates the template files ([Adopting the template](#adopting-the-template)) |

- Render with Quarto 1.10 (CI uses 1.10.19), after re-running every notebook that changed ([Notebooks](#notebooks)).
- `embed-resources` downloads the scripts a page links to and embeds them. If that fails (e.g. an antivirus scanning HTTPS connections), Pandoc keeps the link and the render log says "Could not fetch resource"; the HTML still works but is not fully self-contained.
- HTML must keep all features listed in [What you get](#what-you-get).
- PDF must print as a handout without manual fixes (no cut-off code, no orphaned headings). Check both PDFs by eye before committing them.
- Toolchain: Quarto; R with the packages from `make deps`; LaTeX with pdfLaTeX (with TinyTeX, Quarto installs missing packages itself; a minimal TeX Live also needs `mdframed`, `needspace` and `enumitem`); `pdftotext` (poppler) for `make check`; Jupyter with the kernels `ir` and `python3` to re-run the notebooks.

## Checklist before committing

- [ ] The sheet follows the template ([Writing a new sheet](#writing-a-new-sheet)) and the [Markup rules](#markup-rules); YAML has no `format:` block.
- [ ] Every subtask has exactly one `.sol` block (sub-items included); `_ex.pdf` contains no solution text.
- [ ] Every code task has an R and a Python tab, R first; all code comes from embeds.
- [ ] The notebooks declare `ir` / `python3`, were run top to bottom with `make notebooks` ([Notebooks](#notebooks)) and saved with their outputs.
- [ ] Every embedded cell has a `#| label:`; every embedded notebook has a `notebook-view` entry.
- [ ] Every math macro is defined in `latex-math/combined.qmd`.
- [ ] `<sheet>.html`, `<sheet>_ex.pdf` and `<sheet>_all.pdf` are re-rendered; both PDFs checked by eye; `make check` passes.
- [ ] Conversion: the PR description lists everything added or fixed, and the sheet's legacy sources are deleted ([Converting existing sheets](#converting-existing-sheets)).

## Converting existing sheets

Sheets written in LaTeX, Rnw or Rmd are converted into the format above.

### Source layouts

- I2ML, `exercises/<chapter>-tex/`: a sheet file `*.Rnw` with `\kopf{<N>}{<Topic>}` (`\kopfic` for in-class sheets) and `\aufgabe{<Title>}{…}` blocks that pull in exercises from `ex_rnw/ex_*.Rnw`; solutions are in `ex_rnw/sol_*.Rnw`; R code in knitr chunks.
- IML: `\aufgabe{<Title>}{…}` in `.tex` files under `ex_tex/`, solutions in `*_sol.tex`, figures in `figure/`, R code in separate files or `*_R_sol.Rmd`.
- AppML: one `.Rmd` per sheet (prettydoc HTML), solutions switched by a `show.solution` variable, Python in `<name>_py.ipynb`.

### Procedure

1. Collect everything that belongs to the sheet: exercise text, solutions (often in a separate file), figures, data and code.
2. Convert, don't rewrite: keep the tasks, their order, the solution steps and the numbers. Fix only clear errors and list them in the PR description.
3. Create the files with `make new-sheet` ([Writing a new sheet](#writing-a-new-sheet)), in the chapter folder named as in the slides, next to the legacy `<chapter>-tex/`. Build the `.qmd` from the template: one `## Exercise <N>` per source exercise, subtasks as `a)`, `b)`, and each solution moved directly below its subtask.
4. Move all code into the two notebooks ([Notebooks](#notebooks)), run them, embed the cells.
5. Add what the format requires but the source lacks: learning goals, and the second language for every code task (write it, run it, check the results agree). Add hints only if the source has them. List everything you added in the PR description so it gets reviewed.
6. Build and check ([Writing a new sheet](#writing-a-new-sheet), steps 4 to 6).
7. In the same PR, delete the legacy sources of the converted sheet: its sheet file, exercise and solution files (e.g. `ex_rnw/ex_*.Rnw`, `ex_rnw/sol_*.Rnw`) and the figures and code only it uses. The legacy folder stays as long as other sheets (e.g. in-class sheets) are built from it; delete it when it is empty.
8. The new PDFs `<sheet>_ex.pdf` and `<sheet>_all.pdf` replace the old ones in `exercises-pdf/`: update the links of the course website, then delete the old PDFs.

### Mapping

| Source | `.qmd` / notebooks |
|---|---|
| `\kopf{<N>}{<Topic>}`, Rmd `title`/`subtitle`/`output` | YAML header as in the template |
| `\aufgabe{<Title>}{…}`, Rmd `# 1 <Title>` | `## Exercise <N>: <Title>` plus learning goals callout |
| Rmd "Goal" section | learning goals callout |
| `\begin{enumerate}[a)] \item`, `\item[1)]`, Rmd `## 1.1 …` | subtasks `a)`, `b)`, … |
| nested `enumerate` | `(i)`, `(ii)` or `-` bullets, indented 3 spaces |
| separate solution file (`sol_*.Rnw`, `*_sol.tex`), `\begin{loesung}`, Rmd `show.solution` blocks | one `::: {.sol}` block per subtask, directly after it |
| `\textit{Hint:} …`, Rmd `<summary>**Hint 1**</summary>` | `::: {.hint}` block |
| `\textbf{x}`, `\emph{x}` / `\textit{x}` | `**x**`, `*x*` |
| `\texttt{x}`, `\code{x}`, `\verb\|x\|` | `` `x` `` |
| `\url{u}`, `\href{u}{t}` | `<u>`, `[t](u)` |
| `\footnote{t}` | `^[t]` |
| `tabular` | pipe table |
| `\includegraphics{figure/x.pdf}` | `![](figure/x.png){width=…}` (convert to PNG) |
| `$…$`, `\[…\]`, `equation`, `align` | `$…$`, `$$…$$`, `\begin{align*}…\end{align*}` (check macros, [Math](#math)) |
| `\label{…}`, `\ref{…}` | reference in words |
| knitr chunk that draws a figure (`<<…, echo=FALSE>>=`) | hidden `{r}` chunk ([Figures and tables](#figures-and-tables)) |
| knitr or Rmd chunk with task or solution code | labeled cell in `sol_<sheet>_R.ipynb`, embedded (task code outside the solution) |
| Rmd `setup.R` / package chunk | `import` cell in the notebooks |
| Python notebook with tasks and solutions | its code in `sol_<sheet>_py.ipynb`, the task text in the `.qmd` |
| `\Sexpr{expr}` | the computed value, written out |
| `\\`, `\vspace`, `\lz`, `\dlz`, `\newpage`, `\hfill`, `\noindent`, `center` | drop; `{{< pagebreak >}}` only if the PDF needs it |

## Adopting the template

- **Requirements:** bash, curl and tar (Git Bash is enough on Windows), plus the toolchain in [Outputs and build](#outputs-and-build).
- **Set up once**, from the lecture root: create `exercises/_lecture.yml` with the course subtitle, then copy the template files ([Files and ownership](#files-and-ownership)) into `exercises/`. Chapters, data and legacy folders are not touched.

  ```sh
  mkdir -p exercises
  echo 'subtitle: "[<course name>](<course website>)"' > exercises/_lecture.yml
  curl -fsSL -o update-exercises.sh https://raw.githubusercontent.com/slds-lmu/lecture_service/main/exercises-template/_quarto/update-exercises.sh
  bash update-exercises.sh
  rm update-exercises.sh
  ```

  Without a course website: `subtitle: "<course name>"`. Keep the quotes: YAML reads a value starting with `[` as a list. To use a lecture_service branch, give it as the last argument of `bash update-exercises.sh`.
- **`_lecture.yml`** holds what differs between lectures. Example (I2ML):

  ```yaml
  subtitle: "[Introduction to Machine Learning](https://slds-lmu.github.io/i2ml/)"
  ```

  Further Quarto options for all sheets of the lecture can go here as well. The file is merged into `_quarto.yml`: single values set in `_lecture.yml` win, lists are combined (e.g. `filters: [_lecture/extra.lua]` runs in addition to the shared filters). Files these options refer to belong in `exercises/_lecture/`. Without `_lecture.yml`, the build stops: Quarto would otherwise drop the subtitle without a warning.
  Quarto treats a value that matches the name of a file or folder as a path, so a subtitle without link that equals a chapter name (e.g. `Demo` next to `demo/`) comes out as `../Demo`.
- **Update** with `make update` in `exercises/` (`branch=<name>` for a lecture_service branch). It downloads lecture_service and replaces the template files ([Files and ownership](#files-and-ownership)). Review the changes with `git diff`, re-render the sheets if the layout changed, and commit.
- **CI:** the workflow `pr-exercise-check.yaml` runs the render check (`make check`, [Outputs and build](#outputs-and-build)) on pull requests that change `exercises/` or `latex-math/` and uploads the rendered files; it does nothing in lectures without `exercises/_lecture.yml`. Lectures synced with lecture_service get it with `make update-service`; others copy it from lecture_service's `service/.github/workflows/`.

## Design decision: code lives in notebooks

- **Rule:** all R and Python code of a sheet (code handed out with a task and solution code) is stored in one Jupyter notebook per language, `sol_<sheet>_R.ipynb` and `sol_<sheet>_py.ipynb`, and embedded into the `.qmd`. The `.qmd` holds text, math and tables, plus hidden `{r}` chunks that only draw figures ([Figures and tables](#figures-and-tables)).
- **Why not code chunks in the `.qmd`:**
  - *Both languages run natively.* A `.qmd` runs one engine with one kernel: under knitr (the engine of the sheets), Python runs through reticulate inside R; under the Jupyter engine with a Python kernel, R chunks are not executed but come out as plain text (tested with Quarto 1.7.27). The notebooks run in the kernels `ir` and `python3` ([Notebooks](#notebooks)), as students run them.
  - *Students get the code as notebooks* from the sheet's "Notebooks" list. With code in the `.qmd`, a generator would have to split it back into notebooks.
  - *Rendering does not run solution code:* it is fast, needs neither the R nor the Python ML environment, gives the same results every time (no newly drawn random numbers) and doesn't repeat slow computations (tuning, nested resampling, neural networks). Quarto's `freeze` doesn't help: it is skipped when single files are rendered, as the Makefile does.
- **Accepted costs** (handled by [Notebooks](#notebooks) and the [checklist](#checklist-before-committing)): embedded output can be out of date (re-run the notebooks before rendering), and embeds need cell labels and, inside a subtask, an `.indent` block ([Layout](#layout)).
