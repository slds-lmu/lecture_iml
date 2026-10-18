---
name: review-commented-out-overlaps
description: Review explicitly selected lecture-slide .tex files for sizeable commented-out teaching blocks duplicated by active content in that same closed file set, and give read-only cross-file removal suggestions.
---

# Review Commented-Out Teaching Overlaps

Use this skill when the user wants a non-destructive review of commented-out teaching content across an explicitly selected set of lecture-slide .tex files. It identifies substantial obsolete blocks covered by active material, especially when the active explanation is in a different selected file.

Do not use it to edit files, apply removals, create a report, or discover sibling files. The user performs any cleanup manually.

## Closed comparison set

The comparison set is exactly the .tex files the user supplies by attachment or explicit path.

- Read active and commented content only in that set.
- Do not scan the directory for siblings or consult unselected files, generated output, PDFs, external sources, or Git history.
- If fewer than two files are supplied, still review the selected file, but explain that no cross-file comparison is available.
- If the files are unclear, ask the user to identify the set.

## What to review

Treat commented-out frames, lists, blocks, equations, figures, tables, prose groups, disabled slide content, and visible legacy metadata as possible teaching-content candidates.

Ignore inline source comments, TODOs, author and maintenance notes, imports, macros, layout settings, labels, refs, bibliography commands, escaped percent signs, and literal-code environments.

Focus on coherent pedagogical units:

- whole frames or environments;
- contiguous blocks of prose or several related bullets;
- complete metadata blocks whose visible values are replaced by active metadata; and
- one concept developed in a commented block that is actively taught elsewhere.

Do not produce a noisy inventory of overlapping words, isolated labels, or trivial alternative phrasings. Suggest a single line only when it is itself a complete teaching point and the active duplicate is unambiguous.

## Decision standard

Recommend removal only when active content in the selected set preserves every material teaching point of the candidate. Semantic matches are allowed: wording, ordering, granularity, and slide structure may differ, but the concept, takeaway, motivation, example role, warning, limitation, or argument step must remain active.

Do not recommend removal when the candidate adds a distinct example, caveat, interpretation, use case, assumption, failure mode, or useful level of detail. When in doubt, omit it from the suggestions.

For legacy metadata, require active metadata to preserve every meaningful title, chapter, title-figure, and learning-goal value; similar command syntax alone is insufficient.

Before suggesting a deletion, check for URLs, citations, source labels, authors, paper titles, figure credits, and dataset provenance. If non-redundant attribution is present, recommend preserving it as a comment near the active equivalent rather than deleting it.

## Review output

Make no file changes. Present suggestions directly in chat, grouped by source file and ordered by the largest or clearest redundant teaching blocks first.

For each suggestion, provide:

- the exact file and line range, or a stable frame/environment anchor;
- the complete block or bounded portion the user could remove;
- a short summary of the teaching point;
- active duplicate evidence with file and line range/anchor;
- why the active material fully covers the same pedagogical role; and
- any provenance that must remain and where it can be preserved.

If no high-confidence substantial overlaps are found, say so briefly. Do not create a Markdown report; this skill's deliverable is the chat review.
