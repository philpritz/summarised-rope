# Discussion — 2026-06-23 — editing algebra on a lens vocabulary — with Claude

An **implementation session** (design thread on optics): the sexp editing verbs recast as
lens expressions, and the zipper's refocus helper named as the store coalgebra it already was.

**Landed (622 tests green):**
- `helper-algebras`: `vl` → `make-lens`; new `vdiag` (diagonal vector lens — views slot 0, put
  fills every slot); `vref` now variadic — 1 index bare (unchanged), 2+ a tuple/list focus
  (product lens), + tests.
- `zipper-core`: internal refocus `lens` → `peek`, binding `focus`/`put`; consumed raw, not wrapped.
- `sexp-edit`: `at`/`move`/`each`/`both` recast as lens expressions over `vref`/`vdiag`/`index-of`;
  `lift`/`gap-at` retired (`lift` was just `(updater index-of f)`).

**Forks:**
- **`make-lens` over `lens`** for the constructor — `lens` collided with zipper-core's refocus
  helper (itself a store coalgebra). Over renaming that helper (rejected — `make-lens` fits the
  `make-*` family and overloads no used name).
- **`peek` stays the raw `(values focus put)` store coalgebra, not a `make-lens` lens** — the
  navigator needs both halves from one pass (focus to descend, put to stash as the crumb); the van
  Laarhoven encoding hides the put, forcing a re-split on every crumb replay. `make-lens` lifts a
  peek for the composable (sexp-edit) side; the navigator consumes the peek directly.
- **gap vs seg verbs** — gap (`at`/`move`) through `vdiag` (collapse to a gap); seg through `vref`
  — per-slot `compose` for `each` (a different fn per edge), tuple `(vref 0 1)` for `both`. Kept
  compose-`each` over a tuple-`each` (which needs a zip); both validated.
- **verbs act on the guide vector `gs`, not the zipper** — the multi-edge edit is computed on the
  vector, then navigated once; composing zipper-level updates would re-navigate between edges and
  can hit an invalid (crossed) intermediate cursor.

**Parked:** nomenclature entries for `peek`/`make-lens`/`vdiag`; pulling `edge`/`gap` out as names
(verbs inline the composites); the `view*`/`set*` multiple-values surface for the tuple `vref`.
