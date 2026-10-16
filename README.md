# Biologische Modellen en Statistiek — cursusboek

Source of the BMS course book. The published site (<https://bioms-uu.github.io>)
is the rendered copy in `docs/`, served by GitHub Pages from `main` → `/docs`.

## Working on the book

Clone this repo **outside Dropbox/OneDrive** (git and sync clients do not mix), then once per machine:

```bash
bash setup.sh
```

This installs a merge driver so the generated `docs/` folder never gives merge conflicts.

Edit the `.qmd` files, preview with `quarto preview`, and publish with:

```bash
bash update_book.sh
```

That script pulls the latest version, renders the book, copies `_book/` into `docs/`,
and commits and pushes everything in one go. Never edit files in `docs/` by hand.

Answer files (`Modellen/*_ans.qmd`) are git-ignored and never go online.
