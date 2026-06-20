# deprecated-5

**Per-file pre-change snapshots, not a whole generation.** Unlike
`deprecated-2/3/4`, which archived whole pre-rewrite *generations* (rope +
zipper + summaries together), this round touched the live files one at a time,
copying each here just before its own change:

- `rope-core.rkt` — pre the 2026-06-19 rope-core rewrite.
- `zipper-core.rkt` — pre the 2026-06-19 zipper-core pare-down.

The rest of the active tree (`summaries.rkt`, `sexp-edit.rkt`,
`summary-laws.rkt`, `helper-algebras.rkt`) is unchanged and stays live; nothing
else is copied here.

Kept as the pre-rewrite reference for the 2026-06-19 rope-core rewrite, which
draws an abstraction barrier through the file: the "dumb" structural half (nodes,
accessors, construction, the one `rope-split`) below a small boundary
(`rope-split` / `rope-info` / `combine-info` / `rope-join` / `rope-zero`), and the
"nuanced" descent (`bisect`, `multisect`) above it, speaking only that boundary.

What this snapshot still has, that the rewrite collapses:

- **Two leaf splitters** — `split-leaf` (midpoint) and `split-leaf-at` (guided
  binary search) — merged into one dumb `split`.
- **Two descent ops** — `bisect` (its own borrow/rotate balance loop) and
  `bisect-guided` (its own recurse + reconcat) — unified into a single `bisect`
  whose `decide` defaults to the rough balance (`heal-guide`) and otherwise drives
  a guided cut. `bisect-guided` survives only as a thin wrapper.

Self-contained in this folder: `rope-core.rkt` needs only `(require
racket/generic)`, and `zipper-core.rkt` builds on the sibling snapshot
(`racket/match` + this `rope-core.rkt`); both `raco test` standalone here
(`rackunit` in their test submodules).

- `rope-core.rkt` — the variadic summary rope just before the rewrite. **445
  lines** (with tests) — the size baseline to compare the rewrite against.
- `zipper-core.rkt` — the cursor machine just before the pare-down. **338
  lines** (with tests) — the size baseline to compare the pare-down against.
