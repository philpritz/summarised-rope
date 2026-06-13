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
4. **The source files** — the `*.rkt` at the repo root.

Steps 1–2 are imported below, so they are always in context. Steps 3–4 change
between sessions, so read them explicitly each time.

@discussions/conventions.md
@design-notes/nomenclature.md
