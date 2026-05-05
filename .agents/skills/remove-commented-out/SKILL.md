---
name: remove-commented-out
description: Remove redundant commented-out lecture-slide content from a specified `.tex` file, but only after explicit user permission and only when active content in the same file or sibling `.tex` files already covers the same pedagogical point. Use for pruning legacy commented-out frames, bullets, prose, or alternative phrasings.
---

# Remove Redundant Commented-Out Slide Content

## Purpose

Use this skill to delete commented-out slide content from a lecture-slide `.tex` file when the same teaching point is already present in active slide content.

This is a destructive cleanup skill. Use it only when the user explicitly asks to delete, remove, prune, trim, or clean out commented-out content.

Do not use this skill for generic refactoring, formatting, typo cleanup, or whole-file cleanup unless the user specifically mentions commented-out content.

## Required input

The task must identify a current `.tex` file to edit.

If multiple candidate `.tex` files are present and the user has not specified which one to edit, choose the file most clearly referenced by the user or the current working file. If that is not clear, stop and ask.

## Scope

Edit only the current `.tex` file.

For evidence of redundancy, compare against:

1. active content in the current `.tex` file
2. active content in sibling `.tex` files in the same directory

Do not inspect parent directories, child directories, generated files, PDFs, notebooks, or external sources unless the user explicitly asks.

Do not delete content from sibling files.

## Definitions

### Active content

Count as active content:

- visible frame titles and subtitles
- active bullet text
- active prose
- active equations with explanatory text
- active figure captions
- active diagram labels written in LaTeX/TikZ
- active table text
- active theorem, definition, example, block, alertblock, or note text

Do not count as active content:

- percent-commented lines
- content inside disabled conditionals such as `\iffalse ... \fi`
- content inside LaTeX `comment` environments
- inactive alternative frames
- source comments, TODOs, author notes, or provenance notes

### Commented-out slide content

Treat these as candidate commented-out units:

- contiguous runs of `%`-prefixed prose lines
- commented-out `\item` lines
- commented-out `itemize`, `enumerate`, `block`, or `frame` environments
- commented-out equations when they carry a teaching point
- disabled slide content inside `\iffalse ... \fi` or `comment` environments, if it is clearly legacy slide content

Do not treat these as candidate units:

- escaped percent signs such as `\%`
- inline comments after active LaTeX code
- comments used to explain source code or LaTeX mechanics
- commented package imports, macros, layout settings, bibliography commands, labels, refs, TODOs, provenance, author notes, or maintenance metadata
- content inside `verbatim`, `lstlisting`, `minted`, or similar literal-code environments

## Redundancy criterion

Remove a commented-out unit only when active content already covers the same main teaching point.

A match may be semantic rather than textual. Active content can use different wording, order, granularity, or slide structure.

A commented-out unit is redundant when the active content preserves the same pedagogical function, for example:

- same concept
- same claim or takeaway
- same example role
- same warning or limitation
- same motivation
- same contrast or distinction
- same step in an argument

Active coverage may be distributed across multiple active bullets or frames.

Prefer current-file evidence. Use sibling-file evidence only if no current-file evidence is found.

## Keep rules

Keep the commented-out unit when any of these apply:

- it contains a distinct teaching point
- it adds a materially different example, caveat, interpretation, or use case
- it gives a different level of detail that could be useful pedagogically
- it expresses a limitation, assumption, or failure mode not present actively
- it is provenance, TODO, author note, source note, or maintenance metadata
- it is unclear whether active content fully covers it
- the only matching evidence is also commented out

When unsure, keep it.

## Provenance, reference, and attribution preservation

A commented-out slide block may be semantically redundant overall but still contain non-redundant provenance, reference, or attribution information.

Before deleting any commented-out slide unit, scan it for material such as:

- URLs
- `Quelle`, `Source`, or similar provenance labels
- `citebutton`, citation commands, or citation metadata
- author names, paper titles, journal or conference references
- figure credits, image provenance, dataset provenance, or similar attribution notes

If such information is present:

- check whether the same information is already preserved near the corresponding active slide
- if it is not preserved there, move it next to the corresponding active slide as commented-out text before deleting the redundant slide content
- if there is no clear corresponding active slide, keep the provenance/reference lines commented out at the nearest safe location and do not delete them
- do not convert provenance/reference/attribution lines into active slide content unless the user explicitly asks

## Workflow

1. Identify the current `.tex` file.
2. Read the current file and identify candidate commented-out units.
3. For each candidate unit, classify it as one of:
   - slide content
   - LaTeX/source maintenance comment
   - provenance/TODO/metadata
   - ambiguous
4. Ignore all candidates except slide content.
5. Summarize each slide-content candidate in one short sentence.
6. Search active content in the current file for the same teaching point.
7. If no current-file evidence exists, search active content in sibling `.tex` files in the same directory.
8. Decide:
   - `remove` only if active semantic coverage is clear
   - `keep` if unique, ambiguous, metadata, or weakly matched
9. For each unit marked `remove`, scan it for provenance/reference/attribution material.
10. If that material is not already preserved near the corresponding active slide, move it there as commented-out text before deleting the redundant slide content.
11. If there is no clear corresponding active slide, keep the provenance/reference lines commented out at the nearest safe location.
12. Delete only the redundant commented-out slide content after preserving any non-redundant provenance/reference comments.
13. Preserve nearby whitespace and formatting where practical.
14. Do not rewrite active content.
15. At the end, write a Markdown report documenting every removal and the evidence that justified it.

## Partial redundancy

If a commented-out block contains both redundant and unique material:

- remove only the redundant lines
- keep the unique lines commented out
- preserve enough surrounding context that the remaining comment is still readable

If separating the block would make the remainder confusing, keep the whole block.

## Editing rules

- Only delete redundant commented-out slide content.
- Never delete active LaTeX.
- Never delete citations, labels, refs, macro definitions, package imports, figure includes, bibliography commands, or layout configuration unless they are clearly part of a redundant commented-out slide unit.
- A redundant commented-out slide unit may still contain non-redundant provenance/reference/attribution information.
- Before deleting a redundant commented-out unit, scan it for URLs, `Quelle`, `Source`, `citebutton`, citation metadata, author names, paper titles, figure credits, image provenance, dataset provenance, or similar attribution material.
- If that information is not already preserved near the corresponding active slide, move it there as commented-out text before deleting the redundant slide content.
- If there is no clear corresponding active slide, keep the provenance/reference lines commented out at the nearest safe location and do not delete them.
- Do not convert provenance/reference/attribution lines into active slide content unless explicitly asked.
- Never activate commented-out content.
- Never rewrite active slide content.
- Never use commented-out content as evidence for deleting other commented-out content.
- Preserve line endings, indentation style, and nearby blank lines as much as practical.

## Markdown report requirement

After editing the `.tex` file, create a Markdown report in the same directory.
By default, create one Markdown report per edited `.tex` file.

If the user explicitly asks for a single combined report across multiple files, create one Markdown report covering all edited files instead. In the combined report, group removals by edited file and preserve the same evidence requirements for every removed unit.

Name the report:

```text
<current-tex-basename>-comment-removal-report.md
```

For example, if the edited file is:

```text
interpretability-slides.tex
```

write:

```text
interpretability-slides-comment-removal-report.md
```

The report must document what was removed and why.

Prefer a compact, GitHub-friendly structure that is easy to skim in a PR comment or issue. It does not need to match the template below verbatim, but it should follow the same information architecture and stay concise.

At minimum, include:

- an `Edited files` list
- a short `Summary` with:
   - removed commented-out units
   - kept candidate units
   - restored or moved provenance/reference comment blocks, if any
   - evidence scope in one short line, e.g. `current file yes`, `sibling .tex files yes`
- a short `Provenance / attribution audit` section with bullet points for:
   - concrete repairs made
   - additional provenance restored or moved
   - provenance already preserved nearby
   - cases kept because no safe destination was clear
- for each edited file, a compact per-file section

For multi-file reports, prefer collapsible `<details>` sections per file.

Inside each per-file section, prefer:

- a short heading or `<summary>` line with the file name and counts, e.g. `<file>: 2 removed, 1 kept`
- one short evidence-scope line
- a `Removed` table with columns like:
   - `#`
   - `Removed from`
   - `Removed content`
   - `Active duplicate evidence`
   - `Justification`
   - `Provenance/reference`
- a `Kept` table with columns like:
   - `#`
   - `Location`
   - `Kept content`
   - `Reason`

Each removed row must still capture:

- the original location of the removed commented-out unit, using line numbers if available
- a short summary of the removed content
- the active duplicate evidence with file path and line numbers or stable anchors
- a short justification for redundancy
- whether provenance/reference/attribution information was present
- where that provenance was preserved as commented-out text, or whether it was already preserved nearby

Each kept row should include only notable kept cases and should stay to one short sentence per cell when practical.

Keep the report terse:

- prefer tables over repeated prose blocks
- keep each table cell to one short sentence or clause when practical
- combine multiple evidence anchors in one cell with semicolons or commas
- use bold counts in the summary when helpful

Use a structure like this:

```markdown
# Commented-Out Content Removal Report

Edited files:

- `slides/example-a.tex`
- `slides/example-b.tex`

## Summary

- Removed commented-out units: **<n>**
- Kept candidate units: **<n>**
- Restored or moved provenance/reference comment blocks: **<n>**
- Evidence scope: current file **yes/no**, sibling `.tex` files **yes/no**

## Provenance / attribution audit

- Repaired `<short case>`: moved deleted provenance comments to `slides/example-a.tex:<lines-or-anchor>`.
- Restored `<short case>` at `slides/example-b.tex:<lines-or-anchor>`.
- Other deleted provenance was already preserved near active slides: `<short list>`.
- No deleted provenance lines were left without preservation.

<details>
<summary><strong>slides/example-a.tex</strong>: 2 removed, 1 kept</summary>

Evidence scope: current file **yes**, sibling files **no**

### Removed

| # | Removed from | Removed content | Active duplicate evidence | Justification | Provenance/reference |
|---|---|---|---|---|---|
| 1 | `slides/example-a.tex:10-15` | Old commented summary bullets. | `slides/example-a.tex:30-34`; active frame restates same points. | Same teaching point, phrased more cleanly in active slide. | No separate provenance issue. |
| 2 | `slides/example-a.tex:40-44` | Standalone commented figure frame. | `slides/example-a.tex:30`; same figure already embedded in active slide. | Same figure now serves same role in active content. | Present; preserved as commented-out text at `slides/example-a.tex:35-36`. |

### Kept

| # | Location | Kept content | Reason |
|---|---|---|---|
| 1 | `slides/example-a.tex:60` | Commented caveat about edge cases. | Active slide does not cover this caveat. |

</details>
```

If no commented-out units were removed, still write the report. State that no removals were made and briefly summarize why.

Do not fabricate line numbers. If precise line numbers are not available, use stable anchors such as nearby frame title, environment name, or a short quoted snippet.

## Final response to the user

After completing the edit, report:

- how many commented-out units were removed
- how many candidate units were kept
- the path to the Markdown report
- any notable uncertain cases left in place

Do not include a long block-by-block audit in the chat unless the user asks. The detailed audit belongs in the Markdown report.

## Examples

### Remove

Commented-out block:

```tex
% \item To discover patterns in the data
% \item To debug and improve the model
```

Active content:

```tex
\item To discover and gain global insights
\item To improve, debug and audit models
```

Decision: remove. The same two teaching goals are actively covered.

Report entry:

```markdown
### Removal 1

Removed from: `slides.tex:lines 120-121`

Removed content summary:

> The removed bullets said interpretability helps discover patterns and debug/improve models.

Active duplicate evidence:

- `slides.tex:lines 75-76`
- Active content summary: Active bullets already state that interpretability supports global insight discovery and model improvement/debugging/auditing.

Justification:

The active bullets cover the same two pedagogical goals. The wording differs, but the teaching point is the same: interpretability is useful for discovering structure in data/model behavior and for improving or debugging models.
```

### Keep

Commented-out block:

```tex
% \item Interpretability can reveal feedback loops in deployed systems
```

Active content:

```tex
\item Interpretability improves trust
```

Decision: keep. Feedback loops are a distinct deployed-system failure mode, not merely a rephrasing of trust.

### Keep

Commented-out block:

```tex
% TODO: update this citation after camera-ready
```

Decision: keep. This is maintenance metadata, not redundant slide content.

### Keep

Commented-out block:

```tex
\item Accuracy improves on this benchmark % check whether this still holds for v2
```

Decision: keep. This is an inline source comment attached to active content, not commented-out slide content.

## Final check

Before finishing, verify:

- every deletion was from the current `.tex` file
- every removed unit was commented-out slide content
- every removed unit had active semantic coverage in the current file or a sibling `.tex` file
- no active LaTeX was changed
- provenance/reference/attribution information from removed units was either preserved nearby as commented-out text or was already preserved near the corresponding active slide
- no provenance, TODO, source note, macro, package, citation, label, or maintenance comment was removed without being preserved
- uncertain cases were kept
- a Markdown report was written in the same directory as the edited `.tex` file
- the report starts with a short summary and, when relevant, a provenance / attribution audit section
- multi-file reports use compact per-file sections, preferably `<details>` blocks
- every removal in the report points to exact active duplicate evidence using file paths and line ranges when available
- every removal in the report states whether provenance/reference/attribution information was present and, if so, where it was preserved
- the chat response includes the report path
