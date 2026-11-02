## Slides PDFs

This directory contains the released slide PDFs, published by `slidemk --release`.

Changes in this directory are reflected on the course website, so **changes to
file names** need to be adjusted in the website repository.

The layout mirrors `slides/`:

```
slides-pdf
├── 01-intro
│   ├── 01-motivation.pdf
│   ├── 02-goals.pdf
│   ...
│   └── 99-literature.pdf
├── 02-im
│   ├── 01-motivation.pdf
│   ...
...
```

Here, `slides-pdf/<chapter>/<NN>-<topic>.pdf` is built from
`slides/<chapter>/<NN>-<topic>.tex`, and `<chapter>/99-literature.pdf` from
`slides/<chapter>/references.bib`.
