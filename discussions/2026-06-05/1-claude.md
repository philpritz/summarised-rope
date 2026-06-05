# Discussion — 2026-06-05 — with Claude

Two arcs. First a **design pass** on the nature of guides and indexes that reversed
the `2026-06-01` editing model — designed, not built. Then a long
**implementation pass** that cleaned up and finished `rope-core.rkt`'s balancing and
shrank its API to three names — built and tested (67 checks pass). Design-led by the
user; Claude wrote the code and the lower-level choices.

## Part A — Indexes & guides (designed, not built)

### The reversal: carry, don't derive

`2026-06-01` chose **A** (pin each edge to the side its edit spares; never patch)
over **B** (store a from-the-left coordinate, patch per edit). A couldn't be made to
work. The essential problem: turning a summary into an index (`read : summary ->
address`) is **lossy** — an index holds strictly less information than the summary,
which holds less than the text (cf. `2026-05-28/1`: the seg *head* is more expressive
than the sexp *index*). Two distinct positions can share a summary projection, so
re-resolving a derived index **drifts the cursor**. No cleverer `read` fixes it; the
information isn't there.

So we flip to **B**: the index *carries* its extent and edits *patch* it. The key
reframe — a low-information index is safe **precisely because we never reconstruct it
from the summary**. This isn't contradicting `2026-06-01`'s reasoning (it preferred A
to avoid patching); it completes it — A's hidden cost (the mirror-monoid /
`sx-chars-to-close` inversion) is worse than B's patching.

Underlying principle — **two channels**: the summary monoid drives **guides**
(decisions only, `(left-total right-total) -> sign`; non-invertible is fine, a guide
never asks "what offset"); an additive **measure** carries **extent** (patchable,
lives *outside* the summary). This mirrors the rope already keeping `size` off the
summary.

### The index: `(path start measure)`

A frame path, a start within it, and an extent. `measure 0` = gap, `> 0` = a
selection. Chosen over `(path offset)` (frame fused into the path) because the
explicit frame *contains* the measure (it can't overrun a structural boundary —
tames the `2026-05-28` overrun), makes re-homing clean (a structural edit touches
only `path`; the local span rides along), and is measure-agnostic. It is
`2026-06-01`'s `frame-path + local span` with `offset` swapped for `both-ends`; the
anchor->offset reversal is orthogonal to the frame-path part, which stands.

- **Single cursor.** A zipper is one-hole by construction (the datatype's
  derivative); genuine multi-cursor = one structure + marks as offset-indices that
  rebase on edits — deferred. Single-cursor is also what makes patching trivial (the
  edit is always *at* the focus).

### The measure and its reader

Renamed from "offset" -> **measure** (freed by renaming rope-core's leaf measure to
`string-summary`). It's produced by a **reader** `read : content -> measure` — the
*forward* projection. Contrast the `read` we dropped (`summary -> index`, an
inversion): same word, opposite arrow, and the forward one is safe. The reader can
**reject** unbalanced content — a context-free fragment (e.g. a lone `(`) has no
context-free count; that's `2026-05-28`'s "ill-defined for partial fragments" with a
clean cause. Rejection policy (block / restructure / hold-raw) **parked**.

### Merge the measurer into the guide

The guide and measurer are two readings of one algebra (guide reads *context* ->
direction; measurer reads *content* -> extent), meeting on one quantity. So the
**guide is a callable struct** (`prop:procedure` = the comparator, like crumbs)
carrying its `index` and `measurer`. `compare` takes the index as an *argument* -> it's
algebra-level, not per-index, so moving/editing is `struct-copy` on `index` (no
rebuild) and `carve` reuses one `compare` for a seg's two edges. Index-editing has two
families: **by-number** (`start +/- 1`, path push/pop, `measure +/- 1`, `:= 0`) and
**by-text** (`measure := read C`) — and insert-and-cover *is* the by-text edit.
`measurer : string -> measure` is a pure read; the `(string, index) -> index` shape is
the *edit* built on it.

### Two address sorts; the op set; module layout

- **node-addr** (bare path, the `measure = 1` slice, extent intrinsic) ⊂ **span-addr**
  (`(path start measure)`). node-addr is the clean coordinate for structural
  navigation (path arithmetic); span-addr is for gaps/selections/editing.
  Translations = expand/compress + the bound-gap choices.
- Index operations group by which field moves: horizontal (`start`/`measure`),
  vertical (`path`), and the lone **cross-frame** "next position in document order" —
  which is a primitive *only* if the cursor may sit between atoms (char measure) vs
  only between forms (structural). Parked sub-fork.
- Patching on edits touches **only `measure`** (insert -> `read` content; delete ->
  0); `start`/`path` are invariant under the cursor's own edits. The three
  `2026-06-01` guarantees (cover / hole / round-trip) fall out.
- **Layout (sketched, deferred):** `rope-core` <- `zipper-core` (the machine,
  guide-agnostic — operates on callables) <- **`index-core`** (NEW: rich guide struct
  + address types + index-edit algebra + orchestration) <- `summaries` (concrete
  sexp/char). Keeps `zipper-core` minimal and dodges the import cycle `2026-06-01`
  flagged.

## Part B — Rope-core cleanup & balancing (built)

### Names

`summariser` -> **`summary`** (factory), its minted fn -> **`smr`**; the leaf-measure
param -> **`string-summary`**; `roper` -> **`rope`** (all `-er` suffixes dropped). The
arity guard `(when (null? parts) ...)` is gone — `summary`'s fold base is now
`(string-summary "")`, so `(smr)` with no args = the empty summary, exactly analogous
to `((rope smr))` = the empty rope.

### Balancing — the hybrid policy, now implemented

Per `2026-05-28/2` and the parked scheme in `2026-06-02/1`:

- `tree` gained a **`height`** field (leaf 0, branch `1 + max`); **weight = size =
  `tree-size`**.
- **`bisect`** is a **rough-borrow** taking a `good-enough?` predicate (default
  `within-ratio 3`): rotate the boundary child to the lighter side,
  `[L [A B]] -> [[L A] B]`, until `max <= a*min + 1`, descending only while it improves
  (no overshoot). The lazy heal, run on every `descend`. Cost ~ local imbalance
  (≈O(log ratio) when the heavy side is reasonable, O(n) on a dirty spine — inherent;
  the spine rebuild is Ω(n) for anyone).
- **`rebalance`** = recursively `bisect` with a stricter `within-ratio 2`, reusing
  leaves. The pathology rebuild. *Chosen over* the bottom-up O(n) leaf-collect:
  recursive-bisect is O(n log n) on a spine but reuses the one primitive. Deliberate.
- **`pathological?`** = `height > 3*log2(weight+1) + 2` — the scapegoat trigger
  (literature: partial rebuilding / scapegoat trees / Boehm ropes). The local ratio a
  and the global C are the same bound seen twice; C set just above `1/log2(1+1/a)` so
  an a-balanced tree never trips it.
- **`rope`** factory: dumb concat fold, *then* rebalance if pathological. *Chosen
  over* a from-scratch Θ(n) balanced build (reuse over optimal). *Eager balancing on
  concat was rejected* (the `2026-05-28` rejection), as was strict weight-balance (no
  trigger but eager).
- **`concat-rope`** stays dumb about *shape* but **fuses leaves** — the inverse of
  `split-leaf`: descent splits a leaf, the rises' joins fuse it back. Two boundary
  leaves fuse if `<= max-leaf`, reached by a **size-gated seam descent** (walk a
  boundary edge only through children `< max-leaf` — the bounded leaf tip splitting
  leaves — stop at any real subtree). So it's O(1)-ish, never O(depth). `max-leaf`
  (1024) is the fuse limit *and* the default chunk.

### Illegal states made unconstructable

The bad state — an empty *branch* (a degenerate empty tree) — is now forbidden by a
`branch` **`#:guard`** that errors on an empty child. So size-0 <=> the canonical
empty leaf, structurally. Racket has no static refinement type for "non-empty branch"
(Typed Racket included); the constructor guard is the idiomatic "make the illegal
state unconstructable." Consequences, all tested:

- **`bisect` is total** — empty -> two empties, atom -> empty + rest (it's not a
  forbidden case; `descend` just never *needs* it, since its edge reads stop at an
  atom — which is also what makes descent terminate).
- **`concat` reabsorbs empties** — `join`'s first two clauses drop empties *before*
  any `branch-rope`, so an empty from any source (e.g. bisecting an atom) is cleaned
  up, never branched.
- The guard is the backstop: it fired **0x** across all 67 tests, including ones that
  deliberately push empties through `concat` — empirical proof the invariant holds,
  rather than "this won't happen."

### API: three names

`(provide summary rope bisect)`. Removed from the surface: `rebalance`,
`pathological?`, `tree-height`, `concat-rope`, `atom?`, `tree-size`.

- `concat-rope` -> internal; the zipper's `arrange`/`empty` rejoins now route through
  `rope`. A consequence: rises also run the pathology check, so the parked
  "pure-zipper editing never triggers a rebuild" gap is effectively closed
  (rebuild-on-exposure on both descent and rise).
- `tree-size` -> internal; `gap?` is now `(equal? (head-rope h) ((rope smr)))` —
  emptiness through the public API alone, safe *because* the guard makes the empty
  canonical. (`gap?` now takes `smr`.)
- `atom?` removed entirely — no caller but its own tests.

## Status

`rope-core.rkt` + `zipper-core.rkt`: **67 tests pass.** The rope core is done —
three-function algebra (`summary`, `rope`, `bisect`), the full hybrid balancing
policy, leaf fusing, and a structurally-guaranteed canonical empty.

## Open / parked

- **Part A is all unbuilt** — the seg/edit layer, the rich guide-with-measurer, the
  address translations, `index-core`/`summaries`. Next session.
- **Sub-forks parked:** char vs structural measure (the cross-frame-next op decides
  it); node-addr as a stored cursor state vs a lens over the span-index; the
  unbalanced-insert rejection policy (block / restructure / hold-raw).
- **Deliberate non-optimal choices** to revisit only if they bite: `rebalance` is
  recursive-bisect (O(n log n)) not bottom-up (O(n)); fresh builds are
  fold-then-rebalance not from-scratch Θ(n); no hysteresis / borrow-budget (the
  pathology tier backstops).
