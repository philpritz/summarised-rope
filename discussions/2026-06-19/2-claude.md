# Discussion — 2026-06-19 (2) — rope-core abstraction-barrier rewrite — with Claude

An **implementation session**: `rope-core.rkt` rewritten across a PART 1 (dumb structural) / PART 2 (nuanced descent) barrier. **1484 tests** green; bench compiles. Landed but uncommitted (only the `deprecated-5/` snapshot is committed). Authored by Claude, directed at the code level by the user.

**The barrier.** PART 2 (`bisect`, `multisect`, `make-rope`, `frame`, `within-ratio`) reaches PART 1 only through `rope-split` / `rope-info` / `combine-info` / `rope-zero` / `rope-join`. PART 1 holds the invariant `(rope-join (rope-split t)) = t` (no fusable adjacent leaf pair).

**Internal surface — old → new** (`make-summary`, `leaf-rope`, `branch-rope`, `rope-write-text`, and the `rope`/`leaf`/`branch` structs unchanged):

- *Old (16 fns):* `empty-rope` · `split-leaf` `split-leaf-at` · `bisect` `bisect-guided` · `within-ratio` `heal-guide` `rebuild-guide` · `frame` · `multisect` · `pathological?` `rebalance` · `joiner` `concat-rope` `chunk-string` · `make-rope`  (+ the `empty-leaves` cache)
- *New (10 fns):* `rope-split` `rope-info` `combine-info` `rope-zero` `rope-join` (the boundary) · `within-ratio` · `bisect` · `frame` · `multisect` · `make-rope`  (+ imported `on`; `chunk`/`pathological?`/`rebalance` now local to `make-rope`)

`frame`: `((frame smr b a) g)` → `((frame combine b a) g)`. `bisect`: pared to `(bisect t [decide])`.

**Forks decided:**
- **One fusing `rope-join`, not a separate non-fusing `rope-link`.** Balance must not fuse (it collapses the leaf count the ratio reads on). Dropped on the round-trip invariant: a well-formed rope has no fusable seam, so balance's rejoins never fuse anyway; guided fragments do and get put back. *Cost:* correctness rests on the invariant; spine test rebuilt well-formed (max-leaf, not 1-char leaves).
- **`make-rope` is PART 2** (auto-balance is a balance concern) — removes the PART 1→2 forward ref.
- **`bisect` drops `bacc`/`aacc` seeds** (leaked multisect plumbing; `aacc` always empty). `multisect` bakes `bacc` via `frame` instead.
- **`frame` takes a `combine`** — a side subsumes a summary (`first` *is* the summary), so one `frame` serves both levels; the side is transient and PART-2-confined.

**Parked (drafted, not landed):**
- **`framed` struct for incremental `frame`** — the closure re-combines `bacc` per seam (O(depth)/cut; bites for sexp). `((frame before after) guide)` → a `prop:procedure` struct `bisect` unpacks to seed its descent (folds `bacc` in once); the `framed?` test lives in `frame`, so `bisect` stays `(bisect t [decide])`. Constant-factor win; the struct can't optimize without `bisect` seeding (combine has no inverse).
- **Zipper side-frame** — `zipper-core` frames head *summaries*; side-level wants side-context (weight-0 sides, or a head carrying sides).

**Follow-on — descent algebra threaded as a parameter.** The algebra now threads through the descent as a *parameter* instead of riding in every side, and a side gains a height slot. Directed at a low level by the user; **1484 tests** still green.

*Surface — old → new* (`frame`, `make-rope`, `rope-split`, `within-ratio` unchanged):
- *side:* `(summary weight . algebra)` → `(summary weight height)` — `rope-info`/`rope-zero` shed the algebra tail, gain height
- `(combine-info a b)` → `((combine-info smr) a b)` — curried `smr → cmb`, folding by `smr` · `+` · `max`
- `(bisect t [decide])` → `((bisect cmb) t [decide])`
- `(multisect [guides])` → `(multisect smr [guides])` (contract gains `smr` first)

`pathological?`/`rebalance` now read off `rope-info` (leaf test = `(zero? (third (rope-info t)))`), so PART 2 names no PART 1 struct field — retiring the last below-barrier reads.

*Forks:*
- **Thread the algebra, not carry it per side** (was `(… . algebra)` read off `(third a)`): operator-as-data, positional, unchecked. The algebra now arrives as a parameter; in exchange PART 2 names no struct field at all — a tighter barrier.
- **`smr` outer, `cmb` inner:** `bisect` takes the prebuilt `cmb` (reused across cuts; `smr` would re-derive per descent), `multisect` the `smr` its callers hold.
- **Height by `max`, not `1+max`:** `1+max` is truer but breaks identity; a folded height is never read, so the lawful `max` is free.

*Parked:* fold the kit into the factory — `make-rope` building `cmb` + a specialized `bisect`/`multisect` once (like `leaf-rope`), killing the per-call `(combine-info smr)`.

**Status:** barrier rewrite + descent-algebra threading landed, 1484 green, uncommitted; `framed` optimization, zipper side-frame, and factory-kit consolidation open.
