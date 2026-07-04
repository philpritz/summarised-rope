# summarised-rope

A persistent Racket rope that caches a user-defined summary at every node, with an
s-expression zipper for structural navigation and editing.

## Orient before working — read in order, in full

A fresh session starts by orienting itself. Read the following in order, in full,
**before** responding to the first request or touching any file — skimming defeats
the purpose:

1. **`discussions/conventions.md`** — how these sessions are run (the working
   agreement). This is the authority; its "Session start" section is the canonical
   form of this list.
2. **`design-notes/nomenclature.md`** — the project's working vocabulary.
3. **The latest discussion notes** — under `discussions/`, newest date folder,
   highest-numbered file first. Recent decisions and open threads.
4. **Recent `git log`** — skim the last several commits; the messages record what
   landed (refactors, new/changed surface) and live in git, not in the notes.
5. **The source files** — the `*.rkt` at the repo root.

Steps 1–2 and 5 are imported below, so they are always in context. Steps 3–4 — the
latest discussion and the recent git log — change between sessions in a way these
imports cannot track, so read them explicitly each time.

**Maintaining the step-5 list:** the source imports are enumerated by hand — `@`
has no glob. When a root `*.rkt` is added, renamed, or removed, update the import
lines below to match; a file left off is silently never loaded.

@discussions/conventions.md
@design-notes/nomenclature.md

@rope-core.rkt
@zipper-core.rkt
@summaries/summaries.rkt
@summaries/sexp-summary.rkt
@summaries/lisp-summary.rkt
@text-edit/sexp-edit.rkt
@text-edit/lines-edit.rkt
@text-edit/sexp-split.rkt
@text-edit/lisp-edit.rkt
@text-edit/lisp-view.rkt
@text-edit/lines-zip.rkt
@summaries/summary-laws.rkt
@toolbox/main.rkt
@toolbox/algebra.rkt
@toolbox/deque.rkt
