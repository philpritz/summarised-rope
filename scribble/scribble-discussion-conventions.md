# Scribble Discussion Conventions

A standard for Scribble docs covering a source file's export surface: [`rope-core.scrbl`](rope-core.scrbl).

Conventions for working on the Scribble docs (`scribble/`) in these sessions. A
companion to `discussions/conventions.md`; the general rules there still hold.

## Showing a scribble document

**"Show" defaults to the rough render.** When you ask to *show* a scribble document —
a `.scrbl` doc, one of its sections, or a function's doc we're drafting — the default
is **the rough render**: a chat approximation of what the rendered HTML page looks
like (section headings, the boxed `@defproc` signature with each argument's contract
indented under it, prose paragraphs, bulleted `@itemlist`s, bold/italic runs, inline
code for `@racket[...]` / `@tt{...}`, em-dashes for `---`) — never the `.scrbl`
source. This is the default and needs no qualifier: "show me make-summary" means
render it rough, and I won't ask which form you meant.

Only these explicit phrasings get something else:

- **"the scribble source / the `.scrbl` code / the markup"** → the raw scribble markup.
- **"render it" / "open it in the browser"** → actually run the renderer and open the
  HTML (see Rendering below).

## Organisation

- **One `.scrbl` per source file** in `scribble/` (e.g. `scribble/rope-core.scrbl`),
  standalone — no master manual stitching them together. Cross-file references are
  prose for now (upgradable to linked cross-doc refs later).
- **One section per function**, bundling closely-coupled small functions into a
  single section where they read as a unit (e.g. `make-summary` with its
  `gen:summary-part` / `part->summary` extension point, since coercion is its only
  consumer).

## What goes where (the keep/offload calibration)

Offloading a source file's comments into its `.scrbl` reduces the code to **typical
codebase density**, not zero comments:

- **Stays inline:** a short module blurb, section dividers, and terse *local*
  why-notes (one line on a non-obvious line).
- **Offloads to scribble:** design narrative, justification-against-alternatives,
  cross-file architecture, and worked-example walkthroughs.

## Function doc style

How a function's section is written (complements *Organisation* above).

- **Paired boxes.** A factory that returns a function gets two boxes: the constructor
  with its result named (`(make-summary …) → smr`), then the returned function's own
  box (`(smr part ...) → any/c`). Name the returned function after its code-internal
  name (`smr`, `build`); if anonymous in code, pick a name that doesn't overload an
  existing noun (`split`, not `rope` or `cut`).
- **Concise, core voice.** Open with the result verb (*Returns*, *Folds*, *Cuts*),
  present tense, one sentence where it fits; enumerated cases become short parallel
  bullets naming the real operation (`a string? — (string-summary str)`). Keep design
  rationale out — the box states behaviour; the *why* lives in code comments.
- **Real contracts.** Prefer an honest contract (`any/c`) over an invented type name,
  but spell out a recognized union even when it collapses
  (`(or/c string? rope? summary-part? any/c)`). Use a shorthand only where the expanded
  contract is unreadable (`(-> guide guide)`); for inexpressible arity show `any` and
  describe the shape in prose.
- **Live, verified examples.** Live `@examples`; run them first and use the real
  outputs. Short but descriptive, a `code:comment` per case; prefer a concrete value
  over a factory. Demonstrate rather than assert — if a behaviour is invisible through
  the public API (e.g. fusion), expose what's needed via the internals submodule and
  flag it with a `@margin-note`.
- **See also.** A short footer to the closest companion(s) — usually what's in the
  examples — not an exhaustive list; no forward-references to unwritten sections.
- **Top of the doc.** One inline prose line naming the public functions, not a
  bulleted list or TOC.

## Rendering

`raco` is not on PATH here; use the full path:

```
& "C:\Program Files\Racket\raco.exe" scribble --dest <outdir> <file>.scrbl
```

A standalone `.scrbl` needs a `#lang scribble/manual` line, a `@title`, and
`@(require (for-label racket))` for code links. Standalone renders warn about
undefined cross-reference tags and undeclared exporting libraries — cosmetic; they
resolve once the docs are built into the collection (via an `info.rkt` `scribblings`
entry, deferred for now).
