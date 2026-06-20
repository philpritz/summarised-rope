# Discussion — 2026-06-20 (2) — rope-core comment trim — with Claude

An **implementation session** (with a design thread on comment density): pared
`rope-core.rkt`'s comments back to typical density, moving the design narrative into the
Scribble docs.

**The offload — three files, old → scribble → new:**
- [pre-trim original](../../deprecated/deprecated-6/rope-core.rkt) — 481 lines,
  snapshotted before the work.
- [scribble/rope-core.scrbl](../../scribble/rope-core.scrbl) — where the narrative
  landed: the PART 1 / PART 2 boundary invariant and why one fusing join serves both
  cuts; why `bisect` makes the leaf non-special (a guide alone locates the gap, no
  split-string proc); the `smr/c` vs `guide/c` contract reasoning; a provisional Guides
  section; the make-summary provenance gap.
- [rope-core.rkt](../../rope-core.rkt) — the result, 353 lines (−27%), 34 tests green.

**Calibration (what stays vs offloads):** section dividers + terse load-bearing
why-notes stay inline; design narrative and justification-against-alternatives offload to
scribble; comments that merely restate the code are cut — one was also stale (the
`algebra` "eq? verify" claim describing a guard that doesn't exist).

**Fork:** scribble over a design-notes `.md`. Chose scribble — its `@examples` run on
render, so the offloaded narrative is verified, not left to rot, and it's the surface
shown to others. Losing option: a plain design note (durable, but unverified prose).
