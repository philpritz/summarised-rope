# Discussion — 2026-06-02 (2) — with Claude

Continues `2026-06-02/1`, which left the navigation core as a stack machine —
cursor = `(head, crumb-stack)`, machine ops threading `(head stack) → (values
head stack)` — with `smr` threaded alongside the guide. This session redesigned
the **descent** as an edge-first carry binary search — removing the long-standing
atom special case — and rebuilt and pushed the **gap navigation** on it. Seg
navigation is fully designed but not built. The user set the directions; Claude
wrote the code and the lower-level choices.

## The descent — read the edges first

A guide is a callable `(left-total right-total) -> sign`; a gap guide returns
`-1 | 0 | 1` — the target boundary is left of the cursor, at it, or right of it
(`2026-05-27/1`).

**The pathology it removes.** A gap always sits *on a boundary*. Interior
boundaries coincide with bisection seams, but the two **document edges** (before
the first character, after the last) never do — so a descent that only ever reads
the seam between the two halves has nothing to halt it there. Worked example,
text `abc`: recursive bisection puts seams at offset 1 (`a|bc`) and offset 2
(`b|c`); a gap at offset 3 (after `c`) matches neither, so a seam-only descent
walks `abc → bc → c`, bottoms out at the atom `c`, and must *guess* which side of
it to leave the empty cursor — the old `atom->gap` patch.

**The fix.** At each node, *before* bisecting, read the focus's two outer edges:

```
L = guide(before , rope·after)     ; the  before│rope  boundary
R = guide(before·rope , after)     ; the  rope│after   boundary
```

If `L = 0` the gap is at the focus's left edge; if `R = 0`, its right edge — stop
and plant the gap there. Only when the target is strictly *inside* (both nonzero)
do you bisect and read the seam. On `abc`/offset-3 the root's `R` is already `0`
(offset 3 is the whole document's right edge), so the gap is planted at the root
with no descent at all. Consequences:

- **No atom case.** An atom's only two gap positions *are* its edges, so `L`/`R`
  catch it; the "can't bisect an atom" branch never arises and `atom->gap` is
  deleted outright.
- **Document-edge gaps cost nothing** — caught at the root, where the old descent
  walked to a leaf.
- Every interior gap still aligns with a seam, so it stops at `seam = 0` exactly
  as before.

**The carry.** This makes the descent a binary search over the gap, the two edge
reads its `lo`/`hi`. Each bisection probes the midpoint — the new seam `s` — and
you keep the outer bound on the side you stay on while the seam becomes the new
inner bound:

```
descend left  (s < 0):  (L, R) → (L, s)     ; carry L, the seam is the new R
descend right (s > 0):  (L, R) → (s, R)     ; carry R, the seam is the new L
```

The carried bound is *the same read*, never recomputed: a node's two children
recombine to the node, so `guide(before, leftChild·(rightChild·after)) =
guide(before, rope·after) = L`. So the descent costs **one fresh guide read per
level** (the seam) while always holding both edges. (The alternative —
recomputing both edges from scratch each level rather than threading them — is
correct and slightly shorter to write, but pays two extra reads per level; the
threaded carry was kept.) One honest subtlety to record: the *carried* edge can
never re-hit `0` mid-descent — it would have stopped the moment it was set — so
below the root it is always the fresh seam that fires. The carry earns its place
by catching the two document edges at the root *in the same loop* (no separate
special case for them) and by keeping both bounds live for the seg, where the
span genuinely has two edges in play all the way down.

## `descend` is a machine op; `navigate = descend ∘ ascend`

`descend` takes and returns `(head, stack)`, building the cursor as it goes: one
crumb pushed per step via the three-slot `arrange` — focus a child and stash its
sibling on the way down; at the stop, focus the empty gap with both sides
stashed. Being a machine op of the same shape as `ascend`, the whole move
composes:

```
navigate = (compose (descend g smr) (ascend g smr))
```

— `ascend` rises (popping crumbs) until the focus `contains?` the target;
`descend` then walks down to the gap.

- **Considered — `descend` as a pure rope op** `(before rope after) → (values
  left right)`, returning the *split* instead of moving the cursor. Shorter, and
  exactly the shape `carve` wants (a seg is two such splits chained). But a rope
  op isn't a machine op, so `navigate` would need an adapter — call it `focus` —
  to unpack the head, run the split, `arrange`, and push the crumb, purely to line
  the types up for `compose`. The **machine-op** form was kept so `navigate` is a
  clean two-op composition with no adapter. The cost, paid later: the seg's
  `carve` will need its own rope-level split rather than reusing `descend`.
  Deferred with the seg.
- **Rejected — replacing the rope's `bisect` with a `trisect`** (motivated by
  wanting the single-atom case `"" | "a" | ""` to fall out structurally). The rope
  is a **binary** tree, so `bisect` is a node's natural eliminator — a branch hands
  back its two children in O(1); a leaf splits at its midpoint. A node has no third
  piece to give, so a `trisect` would have to crack one child open: not O(1), and
  with no canonical choice of which child. The genuine three-piece split is
  `carve` / `seg-split` — *two* `bisect`-based cuts — paid for only when a span is
  actually selected. A true ternary *primitive* would mean ternary **nodes** — a
  2-3 tree — a separate, self-balancing direction (it would even subsume the parked
  balancing scheme), not what the gap/atom question called for. Single-atom
  *selection* (`"" | "a" | ""`) is the seg `carve` doing its job; single-atom
  *gaps* are the two edges, handled by the edge reads above.

## Status

- **Gap navigation: built and pushed** (`zipper-core.rkt`). `descend` (edge-first
  carry binary search, machine op), `ascend` / `contains?`, and `navigate =
  descend ∘ ascend`; the old `lens` / `search` descent and its `atom->gap` case
  are gone. **35 char gap-guide checks pass**: navigation lands in a gap and
  preserves the text at every offset of `hello world`; insert via `over` covers
  exactly the typed text; a chunked multi-leaf rope behaves identically; a
  sequential move (settle deep, then navigate elsewhere) exercises `ascend`.
  `start` dropped its unused guide argument — the guide rides in per-op, not in
  the cursor.
- **Seg navigation: designed, not built.** `navigate` will dispatch on guide
  kind: gap → `descend`; seg → `carve`, two boundary `descend`s at the seg guide's
  `signum` edges (`signum(seg∓1)`, `2026-05-27/1`) that isolate the span and focus
  it. The open implementation question is `carve`'s rope-level split sitting next
  to the machine-op `descend` (the trade above).
- **Editing** (insert / delete against the `2026-06-01` index) rides on `over`
  plus re-navigation; that plan is unchanged.
