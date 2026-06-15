# Discussion — 2026-06-15 (2) — with Claude

An **implementation session** continuing the rope-core cleanup, three landings on
today's bundle work.

- **`bisect` → `match` + a sign guide.** Recast as a `match` on leaf/branch
  (binding `smr` off the inherited algebra field), with the `good-enough?` boolean
  replaced by a **guide returning a sign** — `0` good enough (stop), `+1`/`-1` the
  borrow direction — so one call carries both the stop-test and the direction in the
  navigation-guides' `-1/0/1` vocabulary; `within-ratio` now makes that guide. The
  overshoot magnitude stays a bare `(- (w r) (w l))` in each arm, the sole numeric
  residue.
- **`fixed` landed in `helper-algebras`** (beside `iso`/`on`): a fixed-point
  combinator, the loop form, multi-arity via `(compose list improve)` so a
  values-in/values-out `improve` reifies each tuple to a list and the default
  `equal?` compares successive tuples; `good-enough?` composes from a value predicate
  with `on`.
- **`size` → `leaves`.** The rope's structural scalar changes from char length to
  leaf count — the leaf count belongs to the rope proper like `height` does (both
  structural node-counts), whereas char length is content (`string-length`, the
  summary's job). The cached char field is removed outright: an audit showed char
  count is consulted only at the **seam** (concat's two boundary leaves, read O(1)
  off their text), and the small-tip guards reframed to `(leaf? …)`. Cost — O(1)
  whole-rope char length — is now only a `(make-summary string-length +)` summary,
  on-thesis and unused.
- **Nomenclature:** `seam` reworded from the node to the **boundary position**
  between a branch's children.

## Status

- Complete and landed: all four bullets above. 1230 tests green.
- Working tree only, **uncommitted**.
