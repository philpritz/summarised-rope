# Discussion — 2026-05-29 (1) — with Claude

Continuing the summarised gap/seg zipper. This session **landed** a guide
refactor (unifying gap/seg under one head-dispatched guide) and **began but did
not finish** a concrete-guide layer. Landed code is in the named files; draft
code is marked and was deliberately left uncommitted.

## Landed: one head-dispatched guide (merged to master)

Replaced the old typed guide (`type make decide selector index`, with a
`prop:procedure`) with a single guide carrying both readings:

```text
(struct guide (gap-decide seg-decide selector index))
```

- `navigate` dispatches on **head shape** (gap vs seg), not a `type` field. A
  move therefore *preserves* the current shape; flipping shape is the transform
  verbs' job (`delete`, `gap->seg`, …), not navigation.
- Both decides are **curried index-first**; `move/update-index` swaps the index
  with `struct-copy`. `make` is gone — the decide closures don't close over the
  index, so no rebuild is needed.
- `as-gap`/`as-seg` collapsed into `navigate`'s internal `view`, built from the
  `on` combinator (`on h f = λ l r. (h (f l) (f r))`, Haskell's `on`).
- `offset-edge` / `left-boundary` factored into `rope-core` (split a centered
  seg value into its two ±1 cuts / project onto the left edge).

Files: `rope-core.rkt`, `zipper-core.rkt`. Verified by smoke tests (gap nav,
seg nav, delete landing at start-of-next, index surviving a delete).

## Decisions

- **Shared index; delete is index-free by construction.** gap and seg read the
  *same* index. The gap sits at the segment's left edge, so `delete` (seg→gap)
  leaves the cursor at the start of the next sexp with **no index change** —
  `delete` just flips the head and `navigate` reads the gap side. This was the
  whole motivation for the two sharing one index.
- **Offset-vs-anchor fork (2026-05-28) closed toward anchor.** The head carries
  the shape; the index stays shape-agnostic. That is what makes `make`
  unnecessary.
- **No macros / no Qi.** Surveyed Qi (a macro-to-the-core flow DSL), the
  `threading` library, and SDF combinators. Preference is value-level
  composition, so we will hand-roll small combinators (SDF-style
  `parallel-combine` / `spread` / `pipe`) in a helper file *if/when* a
  guide-decide actually fans out; `racket/function` (`compose1`, `conjoin`, …)
  covers the rest.

## Parked: concrete-guide layer (draft, NOT committed)

Started rebuilding concrete algebras/guides using `deprecated-2/` as reference
only, beginning with character count. Draft code (a `char-count` algebra,
`char-edge`, `char-at`, and a `seg-guide` combinator summing two edge guides)
**worked in seg mode** but was intentionally not committed — the separation is
not yet clean:

- **`make-guide`'s gap-decide is wrong for this layer.** Its default
  `left-boundary(seg-decide)` derives the gap *through* the seg. In the intended
  model `gap-decide` and `seg-decide` are **independent readings of the shared
  index** — the gap reads the index directly (e.g. the start edge), never via
  seg.
- **Index representation open.** A seg's index is a *list* of edges; the gap
  reads the start. Unresolved: whether the list holds edge guides (closures) or
  *positions* (positions matter for cheap movement). Current lean is "the edges
  are the index", so `seg-guide` would be index-free — `(seg-guide start [end
  start])`, one edge → both (a unit segment around the boundary).

Mapping noted for later: design-note 001's span guide (`signum(position ± 1)`
projections) is the same machinery as `offset-edge`/`left-boundary`; and
deprecated-2's `before-sexp-guide` / `after-sexp-guide` are exactly the two
boundary readings our single seg-guide unifies.

## Status

Complete and promoted (merged to master): the one-guide / head-dispatch refactor
in `rope-core.rkt` and `zipper-core.rkt`.

Not complete (draft, reverted from the working tree, uncommitted): the
concrete-guide layer + `seg-guide` combinator. Resume by (1) deciding the index
representation — edge guides vs positions — and (2) giving `gap-decide` an
independent reading of the shared index instead of `left-boundary(seg-decide)`.
