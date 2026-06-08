# Discussion — 2026-06-07 (4) — with Claude

Continues `2026-06-07/3` directly. That session worked the gap/seg carve machine
concrete (the `combine` transform, the halt-terminal `descend`, the two-comparator
`ascend`, the two-cut `carve`) but built nothing. This session turned the *split
primitive* over (the `rope-guide`), then reworked the descent representation — and in
doing so **reversed `…/2`'s "cursor = a pair of gap guides" direction** in favour of a
single sign-coerced guide. One built artifact: the new strip descent in
`zipper-core.rkt` (54 tests), on branch `claude/2026-06-07/seg-descent`.

Design-led by the user; Claude wrote the lower-level choices and the code, and was
corrected several times — the corrections are recorded below as durable findings so they
are not re-derived.

## Part A — `bisect` generalised: the `rope-guide` (designed, not built)

`…/3` Part E left `bisect`'s two modes (rough balance vs guided exact cut) as a
*provisional* bundling. This session made it firm.

- **The promotion.** `bisect`'s optional `good-enough?` argument changes from a **boolean**
  to a **ternary** `rope-guide` — `-1` move the boundary left, `+1` right, `0` stop. The
  `0` is a **band**, not a point. Rough-vs-exact then is just the *width* of that band:
  a rough balance has a wide band (any within-ratio cut reads `0`), a guided cut a point
  band (only the exact boundary reads `0`). One loop, one comparator type; the "two modes"
  feeling dissolves.
  - *Why a boolean couldn't do it:* the finger-tree `split p` names an **exact flip
    point** (a boolean monotone predicate has one flip). Rough balance aims at a
    **tolerance**, and a boolean can't carry slack. Promote to a ternary whose `0` is a
    band and the slack lives there. (That `0`-band is `2026-06-02/1`'s parked
    `0`-plateau / fiber, surfacing again.)

- **Where the smart split lives — chose rope-core (the finger-tree view).** Options:
  - **A — generalise in rope-core** *(chosen).* A summarised rope *affords* a
    monotone-predicate split as a first-class primitive (`…/1`'s "re-derive via a guide"
    camp); it belongs with `bisect` whether or not a zipper exists.
  - **B — keep `bisect` dumb, build the guided split in the zipper.** Rejected as the
    primary, but it is the fallback if A's coupling ever bites. Cost: the guided cut is
    descent-shaped and would duplicate machinery.
  - **C — hybrid** (rope-core exposes the dumb halving step, zipper composes). This is
    what A *is* in practice — the guided loop reuses the rough `bisect` as its halving
    sub-step.

- **The off-summary-size snag, and the fix (the bundle).** Balance reads `size`; a guided
  cut reads the **user summary**. `size` is deliberately a node field, *off* the summary
  (`2026-06-02/1`), so a comparator that consumed pre-projected *summaries* could never
  see it. **Fix: the comparator consumes the partition's extracted fields, a bundle
  `(summary, size)`, not a tree and not a bare summary.** `bisect` owns the accessors, so
  it extracts the bundle internally and hands it over — the node struct never crosses the
  wall, the 3-export API is untouched, and each comparator reads whichever field it wants.
  - *Rejected — pass the trees:* would force `tree-size`/`tree-summary` deconstructors to
    be exported, ballooning the API.
  - *Rejected — make every guide consume `(summary,size,height)` universally:* hoists the
    rope's private shape metric into the user's *addressing* vocabulary (a sexp guide
    would stare at `size`/`height` it never uses). Size-off-summary is load-bearing.
  - **Chosen — the bundle stays *local to `bisect`*.** Guides everywhere else stay
    summary-only; a wrapper lifts a summary guide into a `rope-guide`. Balance is *not* a
    guide (it addresses nothing — it keeps the tree shallow; different master), so it
    lives inside rope-core as the default `rope-guide`; a guided cut is supplied. The
    asymmetry (balance internal, guides external) **is** the size/summary module wall.
  - **`height` is dropped from the bundle** — no comparator reads it; only `pathological?`
    / `rebalance` use it, and they are not comparators. The bundle is `(summary, size)`.

- **The rope-guides** (designed): `rough measure within?` (general; `balance [a]` =
  `rough size (within-ratio a)`, the default); `gap->rope-guide g smr b a` lifts a gap
  guide (reads the summary field, ignores size); a **seg guide is two boundaries**, so it
  is *not* one `rope-guide` — `carve` composes two.

  **None of Part A is built.** `bisect` in rope-core is still single-mode. This is the
  design for the next build.

## Part B — Durable finding: the empty focus is a *splitter*, not a bisect target

A long back-and-forth (Claude wrong twice) settled this, and it matters for carve:

- A gap's empty focus is **constructed by the `edge`/`seam` splitter** — `mt` (the empty
  rope) is dropped straight in as the focus. `bisect` is *never* asked to manufacture an
  empty half.
- Therefore the **two-sequential-bisect carve does NOT degenerate cleanly** for a gap: its
  second cut would have to edge-land `(∅, t')` out of a non-empty `t'` (a refine). The
  **span-split carve does**: it finds both cut points in one pass, and when `gs = ge` the
  cuts coincide so the middle is `∅`, placed by the `seam` splitter. So **carve =
  `split-span-rope`** (`…/3` Part D, design-note 001), and the gap is the `gs = ge`
  degenerate — no refine, no special case.

## Part C — The descent representation, turned over twice

This is the session's main move. It ends by **reversing `…/2`/`…/3`'s pair model**.

1. **Quintic over `combine`.** `…/3` had descend collapse the pair via `combine` (`-1` if
   the seg is wholly left, `+1` wholly right, else `0`). That **over-halts**: `combine`
   lumps `Q ∈ {-1,0,1} → 0`, so a seg edge *flush* with a seam halts early at a
   non-minimal node (`…/3` Part B's wrinkle). Reading the quintic `Q = gs + ge` (`-2..2`)
   instead distinguishes flush (`±1`, can still narrow) from straddle (`0`, halt).

2. **…but only `sgn(Q)` is needed.** The descent routing depends only on the **sign** of
   the quintic — a ternary meaning *where the cut sits relative to the seg*: before / within
   / after. `combine` was simply the **wrong ternary collapse** (touching → `0`);
   `sgn(Q)` is the right one (touching → `±1`, narrow; strictly-inside → `0`, halt). So the
   5-valued signal is never materialised: **`sgn(Q)` for routing, the pair for carve.**
   (Equal-call-count aside: `combine` short-circuits `ge` then `gs`; a lazily-written
   `sgn(Q)` short-circuits identically — `combine` bought no cheaper probe, it only
   discarded the distinction the second call already paid for.)

3. **Abandon the pair — ONE guide, sign-coerced.** Reverses `…/2`'s "cursor = a pair
   `(gs, ge)` / point = the diagonal." A seg guide is **one** function
   `q : (left, right) -> {-2..2}` (the 5-state position: `+2` before · `+1` at start · `0`
   inside · `-1` at end · `-2` after). Everything is `q` coerced:
   - **descent** = `sgn(q)`;
   - **carve edges** = `sgn(q-1)` (start, `0` where `q=+1`) and `sgn(q+1)` (end, `0` where
     `q=-1`) — *derived views*, not a stored pair;
   - **containment** (ascend) = `q ≥ +1` (start in) / `q ≤ -1` (end in).
   - A **gap** guide `g` (`-1|0|1`) coerces to `q = 2g` (even-only). Then `sgn(q) = g`
     (descent is the native gap guide), and the carve edges `sgn(2g ∓ 1)` never read a
     literal `0` but *flip* `+1→-1` at the gap, both at the same point → coincident cuts →
     `∅`. The gap falls out.

   - **Why this escapes `…/2`'s rejection of the single 5-state guide.** That rejection was
     about *descend deriving the edges and landing a gap via `gs = ge = 0`* — unreachable,
     since `sgn(q-1)=0` needs `q=+1` and `sgn(q+1)=0` needs `q=-1` at once. We never do
     that: descend routes on `sgn(q)` and halts on `sgn(q)=0`, which a gap **reaches**
     (`q=0` at the gap). The edge-views live only in carve and land at sign *flips*, not
     simultaneous zeros. Different use of the same construction → viable.
   - *Caveat (parked):* carve's gap cuts land at a sign-*flip* with no literal `0` (the
     same machinery as a guide over a non-injective summary), and **which side** each flip
     lands — start right-of-flip, end left-of-flip — is the parked **affinity** choice; it
     must be set so the two coincide at the gap (`∅`) rather than leaving a 1-wide middle.

## Part D — `descend` strips maximally (built)

With `sgn(q)` as the probe, the descent table is keyed on the **seam**, which already
encodes which branch is unnecessary:

- `sgn(q)@seam = +1` → whole seg right of the seam → **strip the left subtree, descend R**;
- `= -1` → strip right, descend L;
- `= 0` → straddles the seam → **minimal node, halt**.

The edges are demoted to **out-of-focus guards only** (`-1` at the left = precedes, `+1`
at the right = follows → ascend further). This fixes the original table, where the
edge-`0` rows came *first* and halted before the seam was consulted — the early halt the
user objected to (`(_ _ 0)` should strip the left subtree, not stop).

- **Termination needs one non-probe bit.** A gap pinned at an extreme edge never reaches
  an interior seam, so stripping toward it would spin. And the probes can't see it: a
  strippable right-edge gap reads `(1 1 0)` and a lone-atom right-edge gap reads `(1 1 0)`
  too — identical. The distinguishing fact is that `bisect` returned an **empty** strip
  side (`lt` or `rt` = the empty rope = we've narrowed to an atom). So the strip rows carry
  an emptiness guard: *if the side I'd strip is empty, halt.* (`…/3` Part B's "zero-width
  gap at an extreme edge loops forever," handled as an emptiness check rather than a
  separate landing row.)

- **Consequence — gap carve is a real step.** The seam-`0` halt **returns the node (the
  text)**, as it should — descend now stops at the *minimal node bracketing the target*
  and no longer lands `∅`. So carving a gap is **not** a free degenerate; producing the
  `∅` (or a slice) is `carve`'s job. Accepted and deferred.

- The trade taken: edge-gaps (insert at start/end) now descend the spine to the adjacent
  atom — `O(depth)` instead of the old `O(1)` edge landing — buying a minimal node and a
  tight crumb. This is the "generally strip unnecessary branches" the user asked for.

## Built

`zipper-core.rkt` (branch `claude/2026-06-07/seg-descent`):

- new `toward` — the single-guide, `sgn`-coerced `match*` table (seam-keyed strip; edge
  guards; empty-strip-side termination);
- new `descend` — iterates `toward` to a fixpoint, halting at the minimal bracketing node;
- `edge-l`/`edge-r`/`seam` splitters kept (unused now) for `carve`.

**54 tests** (144 across the repo). The descent contract is tested on gap guides directly
(`sgn(g) = g`): for every offset, single-leaf and multi-leaf, the text **round-trips** and
the focus span `[before, before+size]` **brackets** the offset; plus the empty document
and a sequential case that exercises `ascend` (the first settle leaves the focus at
`[5,11]`, away from offset 2, so re-navigating must rise).

## Status

- **Built:** the strip descent (gap-tested, seg-capable in routing).
- **Designed, not built:** the `rope-guide` bundle in `bisect` (rope-core is unchanged);
  `carve` as `split-span-rope` with the `gs = ge` degenerate and `sgn(q ∓ 1)` cut-views;
  `ascend` for real seg guides (non-strict containment — `…/3` Part C's *select*, which the
  single-guide `q ≥ +1 / q ≤ -1` test replaces); the `q = 2g` gap coercion end to end.
- **Removed pending carve:** insert-at-gap and the gap-empty (`gap?`) tests — they need the
  `∅` landing, which is carve.

## Open / parked

- **carve** — span-split; the gap's coincident-cut **affinity** (which side of each
  sign-flip) so `gs = ge → ∅`; how carve tells gap from seg (plateau vs carried bit,
  `…/2` Fork 5).
- **The rope-guide build** — land Part A into `bisect` (the `(summary,size)` bundle, the
  `balance`/`rough` default, `gap->rope-guide`). Option B (dumb bisect) remains the
  fallback if rope-core coupling bites.
- **ascend** for segs on the single guide (the `q ≥ ±1` thresholds; non-strict touching).
- Carried over: the fine pin (atom-ids / within-fiber offset / structural-only), fiber
  affinity / cursor-shift, the `0`-plateau, seg-at-seam touching.
