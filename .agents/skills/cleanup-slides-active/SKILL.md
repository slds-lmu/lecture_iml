---
name: cleanup-slides-active
description: Clean up active lecture slide LaTeX source while preserving content and PDF output. Use when refactoring active `.tex` slide content, normalizing frame/layout/image/math syntax, and enforcing the lecture project's helper macros.
---

# Skill: Clean Up Active Slide LaTeX

## Task contract

Treat the task as incomplete until every applicable rule below is satisfied or explicitly marked `[blocked]`.

Do not stop after a partial cleanup.
Do not silently skip a frame, raw layout, raw image inclusion, font-size cleanup, math cleanup, or path cleanup.
Unless the user explicitly names a narrower subset, all active content in the current slide file is in scope.
For this skill, commented-out code is out of scope except for preserving it unchanged while editing nearby active code.
Never remove commented-out content.
If an edit would remove any commented-out line or block, stop and mark it `[blocked: deletion requires explicit user request]` instead.

If something cannot be completed, mark it `[blocked]` and state the exact reason.
Do not claim verification, visual inspection, or compilation unless it was actually performed.

## Goal

Refactor active lecture slide `.tex` files so they are easier to read, more uniform, and more consistent with the project's helper macros.

Preserve content, notation, and rendered PDF output as closely as possible.
Only fix obvious typos or apply mechanical cleanup.

## Use this skill when

- The task is to clean or normalize active slide source in one `.tex` slide file.
- The user wants helper-macro adoption, frame normalization, or active-source cleanup.

## Do not use this skill when

- The task is to delete or prune commented-out content.
- The task is primarily to format commented-out LaTeX.
- The task spans multiple slide files and the user did not explicitly request that scope.

## Core rule

Whenever an appropriate project helper exists, use it instead of raw LaTeX.

Choose helpers from their actual definitions, not from their names alone.

## Patch transport safety

When applying edits through JSON-encoded patch payloads, escape LaTeX backslashes correctly.
Do not place raw LaTeX commands such as `\textit`, `\textbf`, `\tiny`, `\table`, or similar directly into a JSON string without preserving the leading backslash.
Remember that JSON escape handling can reinterpret prefixes such as `\t`, `\n`, `\r`, `\b`, and `\f` and silently corrupt LaTeX commands.

When a patch payload is JSON-encoded, write the payload so the resulting file content still contains the intended LaTeX command with its leading backslash.

After editing, explicitly verify that no LaTeX command lost its leading backslash due to escape handling.
At minimum, check for accidental corruption patterns such as:
- `extit`
- `extbf`
- tab-indented lines where a LaTeX command should begin

## Scope definition

Unless the user explicitly narrows the task to named frames, a line range, or a clearly specified subset, the scope is the active content of the entire current `.tex` slide file.

For this skill, whole-file active scope includes:
- the preamble
- the title slide metadata
- all active frames
- active lines before `\begin{document}` that affect the active file structure

Whole-file active scope never authorizes deleting or pruning commented-out frames, commented-out blocks, or commented-out lines.

Generic wrapper prompts such as "focus on the selected function only" do not narrow scope for slide `.tex` files.
Interpret such wording as referring to the currently open slide file, not to a subset of that file.

If a user instruction appears to conflict with whole-file cleanup but does not clearly identify the narrower subset, do not narrow scope silently.
Either clean the whole active file or ask for clarification.

## Required context gathering

1. Work on one slide file at a time unless the user requests otherwise.
2. Before editing, inspect the slide preamble and the helper definitions relevant to the content in scope.
3. If the file inputs `../../style/preamble`, inspect at least:
   - `style/framei.sty`
   - `style/framev.sty`
   - `style/splitV.sty`
   - `style/image.sty`
4. If the file uses custom itemize helpers directly or indirectly, inspect `style/customitemize.sty`.
5. Before the first edit, count commented-out frame blocks in or directly adjacent to the regions you may touch.
6. After the last edit, verify that this count is unchanged.
7. Process the file frame by frame.
8. Process and verify one active frame at a time. A local edit may also touch its immediate boundaries and, when in scope, the preamble or title metadata.
9. Do not finalize until every active frame in the file has been processed, unless the user explicitly names a narrower subset.

## Verified helper facts

Use these only after checking the actual helper definitions in the repo.

- `framei` wraps slide content in an `itemize` environment and supports `fs`, `sep`, and `align` options.
- `framei` defaults to `fs=normal`, `sep=M`, and centered vertical alignment unless overridden.
- `framev` is the general-content frame helper and supports `fs` and `align` options.
- `framev` defaults to centered vertical alignment unless overridden.
- `\splitV` is an alias of `\splitVCC`, so it is a centered two-column helper, not a generic neutral default.
- `\splitVThreeCustom` is currently an alias of the centered custom-width helper, not a top-aligned three-column helper.
- `\splitVThreeT` is the top-aligned three-column variant.
- `\image`, `\imageC`, `\imageL`, `\imageR`, and `\imageFixed` support optional width and optional attribution.
- `\titlemeta` expects four positional arguments: chunk title, deck title, figure path, and learning-goal items.

## Preamble and title slide

If a slide file uses the standard preamble, keep the preamble compact and keep only clearly needed `latex-math` files.
Remove obviously unused `latex-math` inputs only when this can be determined locally and safely.

If `\input{../../style/preamble}` is present, keep the required Beamer margin override directly below it when that override is used in this file pattern.

The title slide should use `\titlemeta` with its four positional arguments:
1. chunk title
2. slide deck title
3. a figure
4. an itemized list of learning goals

Remove redundant active `\date{}` commands from the title slide metadata when the project title-slide pattern does not use them.

## Global style rules

1. Keep edits local and content-preserving.
2. Use no indentation anywhere in active code.
3. Use one structural command per line.
4. Use one item, sentence, or thought per line where practical.
5. Remove empty lines inside active frames.
6. Allow blank lines only before and after whole active frame blocks.
7. When two active frame blocks are consecutive and no preserved commented-out block lies between them, use exactly two completely empty lines between them.
8. Do not rewrite or reformat commented-out code beyond what is unavoidably touched by adjacent edits.
9. Do not move or rewrite preserved commented-out blocks merely to enforce blank-line style.
10. Do not delete commented-out code under any circumstance unless the user explicitly requests deletion.

## Frame selection rules

1. Prefer `framei` for slides that are mainly a flat list of bullets.
2. Use `framev` for arbitrary content such as prose, images, quotes, split layouts, equations, or mixed content.
3. Use plain `frame[t,fragile]` for verbatim content.
4. Do not use `vbframe`.
5. Do not use `framebreak`; split into multiple frames with the same title.
6. Prefer explicit key-value options for custom frame environments.
7. Default to top-aligned frames.
8. For `framei`, `framev`, or any other custom frame environment that supports an `align` key, explicitly use `align=top` unless there is a concrete reason not to.
9. For plain Beamer `frame` environments that do not support an `align` key, use the top-aligned equivalent option when possible.
10. If a font-size change is needed, prefer explicit frame options such as `fs=small`.

## Layout rules

Replace raw `columns` and ad-hoc `minipage` layouts with project helpers whenever an appropriate helper exists.

Use the helpers as follows:

- `\splitV{left}{right}`: default two-column layout; alias of `\splitVCC`, so both columns are centered vertically.
- `\splitVTT[0.3]{left}{right}`: two columns with top alignment; optional argument sets left-column width.
- `\splitVCC[0.3]{left}{right}`: two columns with centered vertical alignment; optional argument sets left-column width.
- `\splitVBB[0.3]{left}{right}`: two columns with bottom alignment; optional argument sets left-column width.
- `\splitVCompact{0.4}{0.4}{left}{right}`: compact two-column layout with explicit left and right widths; useful when the content should not stretch to the full text width.
- `\splitVThree{A}{B}{C}`: three equal-width columns with centered vertical alignment.
- `\splitVThreeCustom[0.2]{0.3}{0.4}{A}{B}{C}`: three columns with explicit widths and centered vertical alignment.
- `\splitVThreeT[0.2]{0.3}{0.4}{A}{B}{C}`: three columns with explicit widths and top alignment.
- `\twobytwo{A}{B}{C}{D}`: 2x2 layout with equal-size cells and centered content in each cell.
- `\gridLayout[0.3]{A}{B}{C}{D}`: 2x2 layout like `\twobytwo`, but with configurable left-column width.

Do not keep raw `columns` or ad-hoc `minipage` layouts when an appropriate helper exists.
Do not default mechanically to `\splitV`.

Choose the helper that best matches:
1. number of columns
2. width logic
3. vertical alignment

`\splitV{left}{right}` is an alias for `\splitVCC`.
`\splitVCC[0.3]{left}{right}` gives the left column about 30% of the width and the right column the remainder.
Within `\splitV...`, `\vfill` or `\spacer` may be used to fine-tune local vertical alignment when needed.

When using `\splitVTT`, `\splitVCC`, or `\splitVBB`, account for the actual helper implementation.
If the helper is based on `minipage` alignment, verify visual alignment in the slide content, not just the helper name.
In particular, `\splitVTT` can still look vertically offset when one column starts with an image, overlay, table, or list because `minipage[t]` aligns to the first baseline rather than the visible top edge.
If that happens, fix the slide locally before changing helpers or shared style files.
Prefer a content-level top anchor at the start of each affected column.
Use `\spacer[0.25]` only when needed to preserve the intended visible separation or to correct a local alignment issue.
Do not modify shared helper definitions unless the user explicitly asks for helper changes.

## Image rules

Replace raw `\includegraphics` with the appropriate image helper whenever possible:

- `\image{file}`: full-width image with no horizontal alignment wrapper; use when maximum compatibility is needed.
- `\image[0.5]{file}`: scale image to 50% of `\textwidth`.
- `\image[1][CITEKEY-or-URL]{file}`: add a source reference below the image; if a source is supplied, also supply the scale argument.
- `\imageC[0.8]{file}`: centered image; this is usually the default choice for standalone figures.
- `\imageL[0.8]{file}`: left-aligned image.
- `\imageR[0.8]{file}`: right-aligned image.
- `\imageFixed{x}{y}[width][CITEKEY-or-URL]{file}`: place an image at absolute coordinates, for example to keep figures from jumping across animation steps.

Choose the helper from actual macro behavior, not by guessing.
Do not default mechanically to `\imageC`.

`\image{file}` inserts a full-width image without positional alignment.
`\imageC`, `\imageL`, and `\imageR` add centered, left, or right alignment.
The optional attribution argument accepts either a citekey already present in the chapter bibliography or a URL.
If a source is provided, the helper auto-detects whether it should behave as a citation-based source or a URL-based source.
Do not add bibliography entries that exist only to justify an image source.
`\imageFixed` is for absolute positioning, for example to avoid jumping images across animation steps.
When using `\imageFixed`, coordinates are measured from the top-left corner and may use absolute lengths such as `2cm` or relative lengths such as `0.1\paperwidth`.

## Itemize rules

The `itemize...` helpers exist to control item spacing and, if needed, font size, while keeping slide lists consistent.

When spacing or font size must be controlled, use:
- `itemizeS`: smaller item separation; useful for denser lists.
- `itemizeM`: medium or default item separation; effectively the standard choice and closest to ordinary `itemize`.
- `itemizeL`: larger item separation; use when items need more visual separation.
- `itemizeF`: fill layout; stretches the list vertically across the available space and should be used carefully because it may interact badly with surrounding layout helpers.

All `itemize...` helpers accept an optional font-size argument such as `[small]`, `[footnotesize]`, or `[large]`.
Always use `itemizeM` for top-level lists. For second-level lists, prefer `itemizeS` or `itemizeM` depending on spacing needs. 
Treat raw `itemize` as equivalent to the default-spacing case unless a helper is clearly more appropriate.
Do not use raw `itemize` when an appropriate helper exists outside `framei`.

## Spacing rules

1. Do not use `\bigskip`, `\smallskip`, `\medskip`, `\vspace`, or similar manual spacing commands in active content.
2. If visible vertical separation must be preserved, use `\spacer[0.25]` on its own line.
3. Ensure a line break via `\\` is present before the introduced `\spacer[0.25]` unless the preceding line already ends with a line break or the spacer is being used as a zero-height column anchor.
4. Do not mechanically replace `\\` plus vertical spacing with a paragraph break or merged prose line if that changes visible line stacking.
5. When replacing `\medskip`, `\smallskip`, or `\bigskip`, preserve any intentional adjacent `\\` that keeps short lead-in lines, example labels, or formula descriptors on separate rendered lines.
6. After introducing `\spacer[0.25]`, verify that surrounding text still breaks across lines as intended and does not collapse into a longer wrapped paragraph.

## Font-size rules

1. Do not use inline font-size switches such as `\small` or `\footnotesize` unless retaining them is required to preserve rendering and no cleaner frame- or environment-level alternative exists.
2. Do not introduce any font-size change unless there is a concrete need.
3. If the original slide does not use any font-size command or frame font-size option, keep the default size unless a size change is required to preserve layout or avoid a clear overflow problem.
4. Prefer frame options such as `fs=small` when a frame-wide font-size change is actually needed.
5. Otherwise prefer an environment form such as `\begin{small} ... \end{small}` for local content.
6. Do not add `fs=small`, `fs=tiny`, `\small`, `\footnotesize`, or similar commands merely for stylistic normalization.

## Math rules

1. Prefer `$$ ... $$` for display math.
2. Prefer `$ ... $` for inline math.
3. Do not use `\[ ... \]`.
4. Use `align` only when alignment is actually needed.
5. Do not use `eqnarray`.
6. Avoid unnecessary braces around single characters.
7. Use `^T` for transpose.
8. Prefer simple delimiters `( )`, `| |`, `|| ||`.
9. Use `\left` and `\right` only when truly necessary.
10. Avoid English punctuation inside formulas.

## Citation and path rules

1. Use `\furtherreading{CITEKEY}` for literature.
2. Use `\sourceref{CITEKEY}` or `\sourceref{URL}` for source attribution.
3. Do not add bibliography entries only for image attribution.
4. Do not convert Google Docs provenance comments into `\sourceref`s.
5. All figure and table paths must be relative to the slide file.

## Per-frame procedure

For each active frame, in order:

1. Decide whether the frame should be `framei`, `framev`, or `frame[t,fragile]`.
2. Replace any `\framebreak` with multiple frames that keep the same title.
3. Normalize frame options.
4. Add `align=top` to `framei`, `framev`, and other custom frame environments that support an `align` key unless there is a concrete reason not to.
5. For plain Beamer `frame` environments, use the top-aligned equivalent when possible.
6. Normalize line breaking and remove indentation.
7. Preserve explicit `\\` line breaks when they are part of the intended slide layout, especially in short prose blocks, lead-ins, example labels, and variable-description lines.
8. Do not merge consecutive rendered lines into one paragraph unless the visual output is unchanged.
9. Remove forbidden empty lines.
10. Replace raw layout code with the appropriate helper where applicable.
11. For `\splitV...` helpers, verify visual top, center, or bottom alignment from the actual starting content of each column.
12. If a `\splitVTT` layout is visually misaligned because of a `minipage` baseline effect, add a local top anchor at the start of the affected column content instead of editing shared helpers.
13. Replace raw `\includegraphics` with the appropriate image helper where applicable.
14. Normalize figure and table paths.
15. Replace forbidden manual spacing commands while preserving intended line stacking.
16. Normalize font size only when needed to preserve layout, existing intent, or readability.
17. If the original frame has no font-size change, prefer keeping no font-size change.
18. Normalize math style.
19. Verify that the edit is still local and content-preserving.
20. Verify that every frame environment touched in this step is properly closed before moving on.
21. Verify that no LaTeX command in the edited frame lost its leading backslash due to patch transport or escaping.
22. Normalize the boundary before and after the frame block so there are exactly two completely empty lines between consecutive active frame blocks when no preserved commented-out block lies between them.
23. Verify that no commented-out frame block disappeared as a side effect of the edit.

## Do not do

1. Do not substantially rewrite slide content.
2. Do not change notation without reason.
3. Do not remove ambiguous content.
4. Do not introduce unnecessary visible changes in the rendered slide.
5. Do not delete or rewrite commented-out slide content as part of this skill.
6. Do not modify shared helper definitions unless the user explicitly asks for helper changes.

## Final verification

Before finishing, verify every applicable item below.
If any item is false, continue working or mark it `[blocked]`.

1. Every active frame in the file was processed, unless the user explicitly named a narrower subset.
2. The active preamble and title-slide metadata were checked when in scope.
3. `vbframe` is absent from the cleaned active content.
4. `framebreak` was removed from active content.
5. Every active `framei`, `framev`, or other custom frame environment that supports an `align` key uses `align=top`, unless there is a concrete reason not to.
6. Every active plain Beamer `frame` environment uses the top-aligned equivalent when possible, unless there is a concrete reason not to.
7. Appropriate `\splitV...` or grid helpers replaced raw layout code wherever applicable.
8. Any `\splitVTT`, `\splitVCC`, or `\splitVBB` layout in active frames was checked against the actual starting content of each column.
9. Appropriate image helpers replaced raw `\includegraphics` wherever applicable.
10. Paths are relative to the slide file.
11. Forbidden manual spacing commands were removed where applicable in active frames.
12. Replacing manual spacing did not remove necessary explicit line breaks or change visible line wrapping.
13. Exactly two completely empty lines exist between consecutive active frame blocks cleaned by this skill when no preserved commented-out block lies between them.
14. Font-size changes were introduced only where actually needed.
15. If the original slide had no font-size change, no new font-size change was introduced unless required to preserve layout.
16. No avoidable inline font-size switches remain where cleanup was applied.
17. `eqnarray` is absent from active content.
18. `\[ ... \]` is absent from active content.
19. Empty lines in active code occur only at allowed boundaries.
20. Active source is visually clean, unindented, and uniform.
21. Output should remain as close as practical to the original PDF.
22. Every active `\begin{framev}`, `\begin{framei}`, and plain `\begin{frame...}` has a matching closing tag. Count opening and closing tags for each environment type and confirm they are equal before finishing.
23. No LaTeX command lost its leading backslash due to patch transport or JSON escape handling.
24. The file compiles without fatal LaTeX errors. Run `latexmk -halt-on-error -pdf` or the project equivalent from the chapter directory and confirm a PDF is produced. If a TeX installation is unavailable, mark this item `[blocked: no TeX]` and list every unverified environment-balance count explicitly instead.
25. The pre-edit and post-edit counts of commented-out frame blocks in or directly adjacent to edited regions are identical.

## Final response format

Return a compact report with:
1. `Processed frames:`
2. `Key transformations:`
3. `Blocked items:` `none` or explicit `[blocked]` items
4. `Verification:`
5. `Validation run:` actual checks performed or `not run`
