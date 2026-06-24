# Discussion — 2026-06-24 — guide representation: vector vs cons vs list

## Status
Accepted.

## Context
The cursor is two guides (start, end) — once a 2-vector indexed by `vref 0/1`. The edit verbs need
lenses onto it, and we want the lens vocabulary as simple and general as possible. (The landing
itself is in this session's commits — see git log.)

## Decision
Represent the cursor as a list `(list gs ge)`; storage stays two struct fields `gs`/`ge`.

## Considered options
- **vector** `#(gs ge)` — homogeneous, `vref 0/1`-indexed.
- **cons** `(gs . ge)` — needs bespoke pair lenses (`pair-car`/`pair-cdr`/`pair-diag`).
- **list** `(list gs ge)` — the shape `list-of`/`lref` already operate on.

## Consequences
- One lens vocabulary (`list-of`/`lref`) for the cursor; the cons and vector lenses (`vref`/`vdiag`)
  retire.
- Generalizes to n-ary cursors — but the navigator's `peek` still carves exactly two, so n-ary isn't
  usable yet (gated on generalizing `head`/`carve` to multiple foci).
- A wrong-arity `setter` can silently mis-set; mitigated because `lref` is length-safe.
