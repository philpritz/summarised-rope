# Discussion — 2026-05-27 (third) — with Claude

Very short, partial note. Capturing a paradigm-shift idea that came up
near the end of the session and was explicitly parked — not the current
direction, but worth not losing.

## The idea: always-seg head

Drop the `gap` / `seg` distinction. Every head is a seg
`(left, middle, right)`. A point cursor is the degenerate case where
`middle` is the empty rope.

```racket
(struct zipper (sys left middle right before after crumbs))
```

No head field, no discriminator.

## What unifies

- One head shape.
- One guide kind (seg-shape, `-2..2`); the old gap-guide is the
  degenerate seg-guide where the target has zero width.
- One `navigate`, no dispatch on guide kind.
- One `relative`; seg-address is always a pair of gap addresses, and
  point-relative is the equal-endpoints case.
- `gap->seg`, `seg->gap`, `left-bound-gap`, `right-bound-gap` disappear
  — shape never changes.
- `insert`, `delete`, `replace`, `settle-*` all become pure
  content-on-middle operations: same input shape, same output shape.

## Crumbs

Three variants now, one per descent direction:
`opened-from-left`, `opened-from-middle`, `opened-from-right`. Each
stashes the two non-descended pieces of the parent. `up`'s combiner is
ternary; `concat-rope` is already variadic so this is free.

## Costs

- Type-level discriminator gone: "can I delete from a point" becomes a
  runtime "is middle empty" check.
- Always carry a middle field; cheap in practice (empty leaves
  singleton-ish, concat-rope short-circuits) but always allocated.
- "Delete char left" decomposes into two ops (extend-left-by-one,
  then delete-middle) rather than one. Arguably a win — every
  delete-by-X composes from the same two primitives.

## Status

Parked. Not the present direction. Open question whether
address-pair-with-equal-endpoints is the right canonical "point"
across all summary domains; fine for text positions and sexp paths, less
obvious for exotic domains.

## Where this came from

Came up while working out how to encode seg addresses for relative
navigation. If seg-address is just a pair of gap addresses, the
machinery for gap navigation and seg navigation is already nearly the
same — the gap case is what falls out when the pair's endpoints
coincide. From there it's a small step to ask whether the head type
needs to discriminate at all.
