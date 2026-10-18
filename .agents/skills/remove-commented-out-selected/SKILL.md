---
name: remove-commented-out-selected
description: Verify a user's manual removal of redundant commented-out teaching content in explicitly selected lecture-slide `.tex` files from Git diff, find significant remaining candidates in that closed set, and write a report only when no manual follow-up is needed.
---

# Verify Manual Comment Cleanup in Selected TeX Files

Use this skill when the user has manually removed commented-out teaching content from explicitly supplied or selected lecture-slide `.tex` files and asks for verification, further removal suggestions, or the removal report.

This skill verifies and advises; it never edits `.tex` files. It may write the Markdown report only after the verification is clean.

## Closed input scope

The task must identify one or more `.tex` files by attachment or explicit path. These are the **selected files**.

- Read and compare only the selected files.
- Use active content only from the selected files as redundancy evidence.
- Do not discover, inspect, or rely on sibling `.tex` files, even when they are in the same directory.
- Do not inspect generated files, PDFs, parent or child directories, or external sources unless the user explicitly adds them to the task.
- If the selected files cannot be identified safely, ask the user which files to use.

When several files are selected, active content in any selected file may establish redundancy. The skill does not edit any of them.

## Candidate content

Treat these as possible commented-out teaching units:

- contiguous `%`-prefixed slide prose or bullets
- commented-out `frame`, list, block, equation, figure, or table content with a pedagogical role
- legacy presentation metadata that determines visible titles, chapter labels, title figures, or learning goals
- legacy slide content inside `\iffalse ... \fi` or `comment` environments

Legacy presentation metadata includes commented definitions or commands such as `\titlefigure`, `\learninggoals`, `\lecturechapter`, and `\lecture`. Although these look like macro or configuration code, they are candidates when their values encode visible teaching content and an active metadata command replaces that role.

Do not treat these as teaching-content candidates:

- escaped percent signs or inline comments after active code
- source explanations, TODOs, author notes, or maintenance metadata
- package imports, layout settings, labels, refs, bibliography commands, or general-purpose macro definitions
- content inside literal-code environments such as `verbatim`, `lstlisting`, or `minted`

## Redundancy decision

Treat a candidate as safely removable only when active content in the selected files clearly preserves its full pedagogical function. Wording, order, or structure may differ, but the same concept, claim, motivation, example role, warning, limitation, or argument step must remain active.

For a multi-line candidate, verify every meaningful teaching point. If only part is redundant, recommend only that bounded part when the remainder stays understandable; otherwise keep the whole unit. Keep any distinct example, caveat, interpretation, use case, assumption, or level of detail. When uncertain, keep it.

Never use commented-out or otherwise disabled content as evidence.

### Legacy title metadata versus `\titlemeta`

A commented legacy metadata block may be treated as removable as one unit when an active `\titlemeta` preserves all meaningful fields. Compare semantic values rather than command syntax:

| Legacy content | Active `\titlemeta` evidence |
|---|---|
| `\lecture{...}` | short lecture title argument |
| `\lecturechapter{...}` | chapter/title argument |
| `\titlefigure` value | figure argument |
| `\learninggoals` items | learning-goals argument |

Normalize harmless LaTeX differences such as whitespace, line wrapping, escaped underscores, and equivalent abbreviations. Require every learning goal and meaningful title/figure value to remain represented. A nearby active `\titlemeta` is not enough by itself if it omits or changes a teaching point.

For example, the commented `\titlefigure`, `\learninggoals`, `\lecturechapter`, and `\lecture` block in the user-provided example is a valid removal when the active `\titlemeta{Intro to IML}{Introduction, Motivation, and History}{figure/performance\_vs\_interpretability}{...}` retains the same lecture identity, chapter, figure, and all three learning goals.

## Provenance and attribution

Before accepting a manual deletion or suggesting one, check the unit for URLs, citations, `Quelle`/`Source` labels, authors, paper titles, figure credits, or dataset provenance.

- If the same attribution is already preserved near the active equivalent, record that fact.
- Otherwise tell the user to preserve the attribution next to the active equivalent as commented-out text before removing the redundant teaching content.
- If no safe destination is clear, tell the user to keep the attribution in place and retain enough context for it to remain understandable.
- Never activate attribution comments unless the user asks.

## Git-diff verification workflow

The user’s manual removals must be visible in a Git diff for every file being verified.

1. Resolve and record the selected file set before searching.
2. Unless the user supplies another baseline, inspect `git diff HEAD -- <selected files>` so staged and unstaged changes are both included. If a selected file is untracked, or an intended baseline is unavailable, explain that the removals cannot be verified and ask the user for an explicit baseline or diff. Do not write a report.
3. Reconstruct each deleted commented-out teaching unit from the pre-change content and its diff context. A red diff line by itself is not enough to establish the unit or its meaning.
4. Ignore unrelated edits. If the diff deletes active slide content, flag it as outside this skill’s safe cleanup scope rather than treating it as a verified comment removal.
5. Classify every removed unit as teaching content, legacy presentation metadata, maintenance/provenance, or ambiguous. Review only the teaching-content and visible legacy-metadata units.
6. For each reviewed removal, record active post-change evidence in the selected files, then recheck every teaching point and all provenance/attribution against the current file contents.
7. Inspect the current commented-out teaching content in the selected files for additional high-confidence removals that were missed. Prefer coherent blocks—frames, environments, contiguous prose, groups of bullets, and complete visible metadata blocks. Do not flood the user with isolated duplicate words, labels, or trivial rephrasings; mention a single line only when it is a complete, unambiguous teaching point.
8. Do not modify any selected `.tex` file. Use the outcome rules below.

Git diff establishes what changed; it is not semantic evidence. Semantic evidence must be active, post-change content in the selected files, never commented-out or disabled text.

## Verification outcome

### Manual follow-up is needed

Do not write or update the Markdown report if either condition exists:

- a diff-based removal is not clearly redundant, deletes active content, or loses material attribution; or
- an additional high-confidence removal candidate remains.

Instead, present actionable suggestions in chat. For every issue, state whether the user should restore/repair a prior removal or remove an additional block, and include:

- the file and line range, or a stable nearby anchor;
- the complete block or bounded portion to change;
- active evidence with file and line range/anchor;
- why the teaching point is or is not fully covered; and
- any provenance action needed.

The user makes all `.tex` edits manually. On a later request for the report, repeat the verification against the updated diff; never assume previous suggestions were applied.

### Verification is clean

Only when every manual removal has complete active coverage, required attribution is preserved, and no additional high-confidence removal remains, write the report described below. State in chat that no further manual removal suggestions were found.

## Editing invariants

- Never edit, restore, activate, or delete selected `.tex` content; the user performs all follow-up changes.
- Never use commented-out or disabled content as redundancy evidence.
- Never treat deleted active LaTeX as a valid comment cleanup.
- Never claim that provenance was preserved unless the same material attribution is present in the current selected files.
- Keep ambiguous material out of both verification passes and additional-removal suggestions.

## Report

When verification is clean, create or update one Markdown report beside each selected `.tex` file, named `<tex-basename>-comment-removal-report.md`, unless the user explicitly requests one combined report. Do not write a report while manual follow-up is needed.

Keep it compact and include:

- selected and verified files
- counts of verified manual removals and kept candidate units
- the closed evidence scope: selected files **yes**, sibling discovery **no**
- provenance repairs or notable retained attribution
- for each removal: pre-change diff location, short content summary, post-change active evidence with file and line number or stable anchor, redundancy rationale, and provenance status
- notable kept candidates and why they were kept
- a verification result stating that the Git diff was checked, every verified removal retained active coverage, and no additional high-confidence removals were found

Do not fabricate line numbers, anchors, citations, or references. Use a stable anchor when an exact line number is unavailable.

## Final response

If manual follow-up is needed, state that no report was written and present the required suggestions in chat. If verification is clean, state the verified-removal count, kept-candidate count, confirmation that no further suggestions were found, and the report path.
