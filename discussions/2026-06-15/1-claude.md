# Discussion — 2026-06-15 (1) — with Claude

An **implementation session** with a design arc behind it: bundles of summaries
land. A `bundle` is a product summary whose value is a `bundle-val` keyed by each
component's own smr; *applying a component smr to a bundle value selects that
component*. Selection rides a new `gen:summary-part` generic in `rope-core` (one
`cond` case in `make-summary`), so the core stays bundle-agnostic and the dispatch
tests a real type — chosen over an inline `hash?` clause, which lost because a
bundle value isn't reliably a hash. Projection is then just *applying the component
smr*, so a guide/reader reads through it with `on` — `Data.Function.on`,
generalized to n-ary, landed in `helper-algebras` beside `iso` (its Store-comonad /
lens kin). The three `sand-spines` call sites in `sexp-edit` wrap with
`(on sand-spines sexp-smr)`; `sand-spines` itself stays a pure frontier reader.
`sexp-summary.rkt` → `summaries.rkt`, recast as the general summaries file (the
bundle combinator + the sexp instance). Fixed in passing: `on-edges`' new contract
range `any/c` → `any` (its combiner multi-values through it).

**Parked:** a Store-comonad "bundled guides" structure (`store-seek` / `store-extract`,
keyed by smr; `guide-by` = `seek`) for *switching* the installed guide — explored
fully but dropped: switching is already re-installing guides via `zipper-guide`, and
tailoring with `on` at call sites needs no new structure. It returns only if a cursor
must *remember* each layer's position.

## Status

- **Landed:** `on` (`helper-algebras`); `gen:summary-part` + the `make-summary`
  `cond` case (`rope-core`); `bundle` / `bundle-val` (`summaries.rkt`, renamed from
  `sexp-summary.rkt`); the `on`-wrapped `sand-spines` calls (`sexp-edit`); import /
  doc updates (`CLAUDE.md`, `README`, `bench`).
- All green — **1226 tests**. Working tree, uncommitted.
