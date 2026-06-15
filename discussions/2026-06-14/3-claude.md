# Discussion — 2026-06-14 (3) — with Claude

An **implementation session**: contracts for the public surfaces of `rope-core`
and `zipper-core`, via `provide`/`contract-out` (boundary-only, so internal
recursion stays uncosted).

- **Shared helpers are private module-level `define`s** — a `let` can't span
  `contract-out` clauses, and `provide` is no expression to wrap.
- **`smr/c` is flat `procedure?`**, not `(unconstrained-domain-> any/c)`: the
  range is `any/c` (summary values are opaque), so the arrow would only
  chaperone the hot, constantly-called smr for no added check.
- **`guide/c` is higher-order where guides are *called*** (rope-core's
  `multisect`, codomain `-1/0/1`); but the zipper's stored **`guide-pair/c` is
  flat shape-only** (`vector/c procedure? procedure? #:flat?`), since a
  chaperoned cursor would re-check on every navigation `vector-ref`.
- **The two accessors need `->i`** — not an `or/c` of arrows (can't dispatch:
  every face is an arity-1 procedure at first order) nor `case->` (arity-based).
  One arrow, result chosen by the argument, collapsing to read-value vs. command.
- **`start` now requires a cursor** (no more `#f` guides) — deleted
  `zipper-show`'s now-unreachable no-guide branch, and rippled through the
  `cursor` helpers, bench, and tests.

**Status:** 1218 tests green; working tree uncommitted.

- **Open:** whether `start` should navigate to its cursor (would drop `cursor`'s
  redundant re-install).
- **Parked:** tightening the modify-face `f`; the relational write-face invariants.
