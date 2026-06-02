# Discussion — 2026-06-02 (1) — with Claude

The cleanup pass flagged in `2026-06-01/1-claude.md` ("a mock-up, to be cleaned up
and rewritten"). Three threads: the **rope** was combed through and rewritten
(done — the headline); a size-based **balancing** scheme was designed and parked;
and the **zipper** was reframed as a stack machine — design only, the live
direction this note hands off. Design-led by the user; Claude wrote the code.

## Rope rewrite (done)

Rewrote `rope-core.rkt` top to bottom; the pre-rewrite version is snapshotted in
`deprecated-4/` for a size comparison. **243 → 184 lines, 16 tests pass.**

Decisions, each with what it beat:

- **Pruned the public surface to `{summariser, roper, bisect}`** (plus `atom?`,
  kept pending a total `bisect`). Dropped `rope-algebra`, `empty-rope`,
  `empty-rope?`.
  - `rope-algebra` goes because the zipper will **thread `smr`** (carry it, seed at
    `start`) instead of recovering it from a focus rope — the caller already holds
    it. *Over* the sys-free recovery (the 2026-05-29 design), which is unnecessary
    once `smr` is threaded. It survives as an *internal* accessor (the summariser's
    `eq?` guard still needs it).
  - `empty-rope` → `((roper smr))` (the no-arg builder already yields the canonical
    empty); `empty-rope?` → a canonical-empty check (`(equal? r (rope))` or
    `(zero? (tree-size …))`).

- **Nodes as a struct hierarchy (route 1).** A `tree` parent holds the shared
  fields; `leaf`/`branch` inherit:
  ```racket
  (struct tree (summary algebra size) #:transparent #:property prop:custom-write …)
  (struct leaf tree (text))  (struct branch tree (left right))
  ```
  Gives uniform `tree-summary`/`tree-algebra`/`tree-size`, inherits the write hook,
  and turns the old `rope?` `or` into a free `tree?`. *Over* the union predicate and
  `racket/generic` — inheritance wins because the node types share real fields, so
  it DRYs fields, accessors, and custom-write at once.

- **Dropped `leaf-range`** (the no-copy backing view). The argument: splitting a
  leaf must re-measure each half (`combine` has no inverse), and re-measuring
  **substrings anyway** — so `leaf-range` never saved the split copy, it only
  avoided *storing* a second copy while pinning the whole backing string alive (a
  substring leak). Plain copy-on-split is the same substring, now kept, and frees
  the backing. *Parked, not rejected:* a slice-aware measure (`measure text start
  end`) would let it measure without copying and earn its place — not pursued.

- **Added a `size` field** (char length) to every node — a **dedicated field, off
  the summary**. Summaries are bracketing-invariant (content quotiented by
  associativity), so a tree-shape quantity can't live in one; `size` is fixed
  (`string-length`), never user-supplied — plumbing, not the user's summary.
  Groundwork for balancing. *Over* folding size into the summary, which would
  pollute the user's value and couple balancing to it.

- The **variadic coerce-and-fold** summariser is unchanged (the 2026-05-29 trial,
  retained).

**Consequence:** `zipper-core.rkt` and `summaries.rkt` no longer build — they
import the dropped exports. The fix is the `smr`-threading migration below; the
rope stands alone (its own tests pass).

## Balancing (designed, parked)

Not implemented; only the `size` field landed (as groundwork).

- **Scheme: size-based (weight-balanced), not red-black/AVL.** Height and colour
  are bracketing-*dependent* (tree properties, not content) so cannot be summaries;
  size can. Then layered on: **also store `height`**, pathology = `height` vs
  `log₂(size)`.
- **Where:** local **rough-borrow in `bisect`** (shuffle whole subtrees across the
  split until the halves are even — by *size*; the user prefers *by height* later,
  flagged as not a clean swap since height is `max+1`, not additive); **total
  rebuild** when a node trips the pathology check; **balanced construction** in
  `roper`.
- **Pathology measures weighed:** height-vs-log(size) (chosen, needs the stored
  height); weight-ratio / BB[α] (size-only, local); scapegoat depth-vs-log(size)
  (size-only, nothing stored); potential/debt counters. A *strict* weight-balance
  needs no rebuild trigger at all — the trigger exists only because the local
  repair is lazy.
- **Sketched but parked** (`balanced?`, `rough-split`, `rebuild`, the new
  `bisect`). Known gaps: `roper` isn't balanced yet (fresh loads are spines until
  first-bisected); healing is **descent-only** (rises/edits and off-path subtrees
  wait until a `bisect` reaches them — intended laziness).

## Zipper — the live direction (design only)

The handoff. Nothing here is implemented; `zipper-core.rkt` is untouched (and
non-building, per above).

**A zipper is a stack machine.** The `head` is the working register (the focus
sub-rope plus the cached `before`/`after` summaries); the crumbs are the stack,
each crumb a repair closure `head → head`; `rise` is pop-and-apply, descent is
push; to-root folds the stack over the focus.

**Essential vs not** (the trim this buys): the irreducible state is `(focus,
stack)`. The head's `before`/`after` are a *memoized fold of the stack* — a cache
for O(1) guide reads, recoverable, not core. The `guide` is an op *parameter*; the
`span` (move/seg) is a cursor *label*.

**The algebra** — ops thread `(head stack) → (values head stack)`; guide-curried
ops are `((f* g) head stack)` (two values, no wrapper struct):

```racket
(define (rise h k)                      ; pop + reconstruct the parent
  (values ((car k) h) (cdr k)))

(define ((descend lens) h k)            ; push the lens's crumb, focus its child
  (define-values (h* c) (lens h))
  (values h* (cons c k)))

;; lens : guide -> (head -> values head crumb)  — the descent step; absorbs the
;; old `pick` + `atom->gap` (bisect & read the guide at the seam, else at an atom
;; drop to a gap beside it).

(define ((search g) h k)                ; navigation = guided descent to the gap
  (if (gap? h) (values h k)
      (let-values ([(h* k*) ((descend (lens g)) h k)]) ((search g) h* k*))))
```

So `search = descend ∘ lens`, looped — "searching for a position" *is* descending,
the guide's sign picking the side at each seam. `ascend g` is the dual (`rise`
until `contains? g`); `navigate g` = `ascend` then `search`; `over` edits the
focus, and an `over` between a `descend` and its `rise` is what propagates an edit
upward.

**Noted, not adopted:** the crumb stack is a *defunctionalized continuation*
(`rise` is its apply), and the whole is the zipper **comonad** / the type
derivative `∂T`; Racket's delimited control could carry the stack as a real
continuation. Kept as the explicit two-value machine. The two-value threading was
chosen *over* a packaged `machine` struct — it matches how the old
`rise`/`descender` already passed `(h crumbs)`.

## Status

- **Rope:** rewritten, 184 lines, 16 tests pass standalone — ready to promote.
- **Balancing:** designed, parked; only the `size` field landed.
- **Zipper:** design only; `zipper-core.rkt` / `summaries.rkt` need the
  `smr`-threading migration to build again. **Next session:** rewrite the zipper as
  the stack machine above (`descend`/`rise`/`over` + `lens` + `search`/`ascend`/
  `navigate`), threading `smr`, then fold the balancing work back in.
