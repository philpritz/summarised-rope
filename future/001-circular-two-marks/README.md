# 001 — Circular rope with two marks

Exploratory project. **Not the current direction.** Each `future/NNN-*`
folder is a sibling-to-deprecated workspace: it starts as a snapshot of
the present core and develops in a direction we're not yet committed to.

This project explores the circular-rope / always-two-marks model that
came out of the 2026-05-27 sessions. The `.rkt` files here are a copy
of the top-level `rope-core.rkt`, `zipper-core.rkt`, and
`zipper-core-draft.rkt` at the time of branching; from here we modify
in place.

## The shift, in one sentence

The cursor is always two marks. The rope is conceptually circular. The
two marks divide the loop into two arcs; operations act on one
designated arc.

## Why this version (not the linear-oriented-pair version)

We considered modelling the seg as an ordered pair `(m1, m2)` with a
genuine orientation. That gave clean mathematics but earned its weirdness
— "insert at a reverse seg" wanted to write content backwards to stay
consistent with the orientation Z/2. The circular model gets the same
Z/2 structure for free, with cleaner semantics:

```text
two marks on a loop = two arcs
swap = pick the other arc
```

Swap doesn't reverse content. It changes *what region the operation
acts on.*

## Arcs and origin

A normal text file isn't truly circular — it has a beginning and an end.
We model that as a fixed third mark:

```text
origin   — pinned at the document boundary
m1, m2   — the user's two marks
```

Three marks divide the loop into (up to) three arcs. The arc *not*
containing origin is the "inner" / "selection" arc. The arc *containing*
origin is "everything outside" — `after ⊕ before` walked in document
direction.

For genuinely circular documents (ring buffers, calendars, programmer's
ring of buffers), the origin is movable or absent. Linear text pins it.

## What this gives the editor

Operations on the inner arc are the usual ones. Operations on the
complement (via swap) recover classical editor verbs as compositions:

| Composed op | Effect | Conventional name |
|---|---|---|
| `swap; delete` | drop everything except seg | **extract** |
| `swap; insert(c); swap` | replace surroundings, keep seg | **wrap** |
| `inner=∅` ↔ `outer=∅` (via swap) | full document ↔ point cursor | **select-all duality** |

Selection state is no longer modal. The two marks are always there.
"Point cursor" is the degenerate case where the marks coincide; "full
selection" is the degenerate case where the inner arc is the entire
non-origin part of the loop.

## Summary algebras: nothing changes

The algebra interface stays `(empty, leaf, append)`. The circle is a
*positional* concept, not an algebraic one. The zipper already
maintains `before-summary` and `after-summary` separately; the outer
arc's summary is just their concatenation in document direction:

```text
inner-arc summary = middle-summary
outer-arc summary = after-summary ⊕ before-summary
total summary     = before ⊕ middle ⊕ after        ; linear, unchanged
```

Non-commutative algebras (sexp frontier, row/column) compose in the
correct order because the origin's position fixes the walk direction.
No inverse / subtraction needed.

## Address shape

```text
seg-address = ((m1, m2), inside?)
```

`m1 ≤ m2` always (normalize the pair). `inside?` is the Z/2 bit
indicating which arc is the selection. `swap` toggles `inside?`. The
arc-bit travels with the address through every transform.

For point-relative work, the degenerate case `m1 = m2` makes the
arc-bit irrelevant (both arcs degenerate to the same point).

## Implementation sketch (very rough)

- Head shape stays `(left, middle, right)`. Add a `inside?` flag on the
  zipper or fold the swap into operations.
- Or: head shape becomes `(content, complement-summary)` with a
  primitive `swap` that exchanges the two. This is more symmetric but
  may cost more in summary recomputation.
- Crumbs: same three variants as the always-seg sketch. Each ascent
  reconstructs the parent's three-piece head.
- `navigate`: seg-only. Gap-guides are degenerate seg-guides.
- `relative`: one helper, address is always a pair-plus-bit.

## Open questions to develop here

1. **Where does the `inside?` bit live?** On the zipper, on the address,
   or implicit in operation choice? Each has tradeoffs.
2. **Does origin ever move?** For files, no. For exotic documents, yes.
   Does the algebra need to support a movable origin, or do we ship
   pinned-origin-only?
3. **Navigation across the origin.** Should `navigate` ever cross the
   origin (i.e. wrap around)? Probably not for text. But the
   *mathematical* answer is yes; the *UX* answer is no.
4. **Inverse / complement summaries.** Some algebras have natural
   subtraction (character count). Should the system *use* it when
   available (for fast swap), or always walk the actual rope (uniform
   but slower)?
5. **Generalization to k-marks.** Origin + m1 + m2 is three marks. Is
   there a natural generalization to k marks on the loop (k-1 arcs
   tile the non-origin part)? Multi-cursor falls out naturally if so.
6. **Editor verbs.** Which classical editor operations decompose as
   `swap; primitive`? Which need their own primitives? Sketch a
   complete editor verb table.

## Status

Sketch only. No code yet. Develop here without disturbing the present
core.
