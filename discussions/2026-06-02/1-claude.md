# Discussion — 2026-06-02 (1) — with Claude

The cleanup pass flagged in `2026-06-01/1-claude.md` ("a mock-up, to be cleaned up
and rewritten"). Three threads: the **rope** was combed through and rewritten
(done — the headline); a size-based **balancing** scheme was designed and parked;
and the **zipper** was reframed as a stack machine, then carried forward — its
`smr`-threading settled, the navigation core written and verified, the seg
(selection) layer designed. Design-led by the user; Claude wrote the code.

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

**Consequence:** the old `zipper-core.rkt` / `summaries.rkt` / `examples.rkt`
(which import the dropped exports) are **deprecated to `deprecated-4/`** — the
active tree is the new rope alone, a blank slate. They are rewritten from scratch
next session, not migrated; the rope stands alone (its own tests pass).

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

## Zipper — the stack machine (navigation built; segs designed)

Now the live `zipper-core.rkt`. The navigation core below is **written and
verified** against the new rope; the seg (selection) layer is **designed, not yet
built**.

**A zipper is a stack machine.** The `head` is the working register (the focus
sub-rope plus the cached `before`/`after` summaries); the crumbs are the stack,
each crumb a repair closure `head → head`; `rise` is pop-and-apply, descent is
push; to-root folds the stack over the focus.

**Essential vs not** (the trim this buys): the irreducible state is `(focus,
stack)`. The head's `before`/`after` are a *memoized fold of the stack* — a cache
for O(1) guide reads, recoverable, not core. The `guide` is an op *parameter*; the
`span` (move/seg) is a cursor *label*.

**The algebra** — ops thread `(head stack) → (values head stack)`; guide-curried
ops are `((f* g) head stack)` (two values, no wrapper struct). Composition is by
Racket's multiple-value `compose`, which feeds the two values straight through, so
the manual `let-values` plumbing drops out:

```racket
(define (rise h k)                       ; pop + reconstruct the parent
  (values ((car k) h) (cdr k)))

(define ((descend lens) h k)             ; push the lens's crumb, focus its child
  (define-values (h* c) (lens h)) (values h* (cons c k)))

;; lens : guide -> (head -> values head crumb)  — the descent step; absorbs the
;; old `pick` + `atom->gap` (bisect & read the guide at the seam, else at an atom
;; drop to a gap beside it).

(define ((search g) h k)                 ; guided descent to the gap
  (if (gap? h) (values h k)
      ((compose (search g) (descend (lens g))) h k)))

(define ((ascend g) h k)                 ; rise until the focus contains the target
  (if (or (null? k) (contains? g h)) (values h k)
      ((compose (ascend g) rise) h k)))

(define (navigate g) (compose (search g) (ascend g)))   ; point-free: search ∘ ascend
```

So `search = descend ∘ lens`, looped, and `navigate = search ∘ ascend` — point-free
over the two-value stream; `over` edits the focus, and an `over` between a `descend`
and its `rise` propagates an edit upward. The recursion sits on the *left* of each
`compose` (the consumer / last-applied slot), so it stays a proper tail call —
checked: such a loop runs 300M iterations in constant memory.

**Noted, not adopted:** the crumb stack is a *defunctionalized continuation*
(`rise` is its apply), and the whole is the zipper **comonad** / the type
derivative `∂T`; Racket's delimited control could carry the stack as a real
continuation. Kept as the explicit two-value machine. The two-value threading was
chosen *over* a packaged `machine` struct — it matches how the old
`rise`/`descender` already passed `(h crumbs)`.

### smr threaded alongside the guide

`rope-algebra` is gone, so ops can't recover the summary fn from a rope; it is
**threaded as a fixed parameter riding next to the guide** — the realized ops are
`(lens g smr)`, `(search g smr)`, `(contains? g smr)`, `(navigate g smr)`, and
`start` is *given* smr (the caller built the rope with `(roper smr)`) rather than
recovering it. The surface is small: only `lens` and `contains?` read smr;
`search`/`ascend`/`navigate` merely route it; `rise`/`descend`/`over`/`gap?` never
touch it (crumbs close over smr via `arrange` at descend time).

- Chosen — **alongside g**: the machine stays a clean two-value `(head stack)`
  stream (smr never enters the state) and `compose` keeps composing. *Cost:* `g`
  and `smr` travel as a pair at every call site, and `start` shifts from recovering
  to receiving.
- *Over* **smr as a head field**: makes it ambient (the carriers needn't route it)
  but widens the head and threads a constant through `rise`/`over` that never use it.
- *Over* **smr bundled into the guide** (`g` carries its own smr): also frees the
  carriers and is arguably where it belongs — **parked** as the natural follow-up if
  the g/smr pairing grates.
- *Over* **a third machine value** `(head stack smr)`: threads a never-changing
  value through every op and muddies `compose`. Rejected.

### Navigation: built and verified

`gap?` is `(zero? (tree-size focus))` — O(1) and smr-free — so `rope-core` now also
exports `tree-size`. Verified against the new rope with an inline char guide (the
char-count summary *is* the offset): navigation lands in a gap and preserves the
text at every offset; `insert` via `over` covers exactly the typed text; a chunked
multi-leaf rope behaves identically; and a sequential move (settle deep, then
navigate elsewhere) exercises `ascend`/`contains?` and lands exactly on target.
**35 checks pass.** Concrete structural guides aren't ported yet, so this is the
flat/char case only.

### Segs: the polymorphic plan (designed, not built)

A seg move should fall out of the *same* machine: a **gap is the zero-width
degenerate of a seg**, and the old `navigate = carve ∘ narrow ∘ rise` re-emerges as
`ascend → search → carve` over guides made polymorphic in arity — a gap is one
boundary guide, a seg is two (`gs`/`ge`).

Every descent level is a **3-way decision read off one bisect** — `descend-L` /
`descend-R` / **terminate** — and gap vs seg differ only in the terminal:

```text
(bisect t) -> (L R); summarise the seam once
  target strictly left of seam   -> descend L   (focus L, stash R)
  target strictly right of seam  -> descend R   (focus R, stash L)
  else (straddles)               -> terminate
       gap : empty middle  (today's `0` case)
       seg : carve the node by (gs, ge) into (l, m, r), focus m  — another `lens`
```

- The two **descend** branches are identical to the gap `lens`. The **carve** is
  `arrange smr b l m r a` (the 3-way arrange `lens` already uses) cutting at two
  boundaries (`carve2` = two `split-at`s) instead of one bisect — so it drops into
  `descend` like any lens. With `gs = ge` the two seam queries collapse to one and
  the middle is empty: exactly the gap. No special-casing.
- **`contains?` polymorphises to "fully contains"** — a gap: the point is inside; a
  seg: *both* edges inside. `ascend` runs it on the **focus vs its own anchors** (no
  bisect); the descent runs the analogous test on the **children**, which is *why*
  it bisects. The bisect lives only in descent.
- **One bisect per level**, decided *from the bisect's products* (the seam
  queries) — never query-then-bisect-again. The seg reads two seam queries off the
  one bisection where the gap reads one. (The 2026-05-30 "bundle the bisect with the
  decision" resolution, now also covering segs.)
- **Open knob — how much to narrow before carving:** from root (the old
  `carve-span`, one fat crumb); from the containing ancestor after `ascend` (one
  crumb); or `ascend + narrow` to the minimal node (incremental crumbs, and the
  gap-`search` is literally its zero-width case). Leaning to the last, for
  uniformity.
- **To nail — the `0` / touching cases.** The strict-sign convention (descend only
  when the target is *strictly* one side) is what makes the gap degenerate clean,
  but it is exactly where the old "dead zone" and the `runs-end` monotonicity bug
  lived. The "fully contains" predicate is where to pin open-vs-closed at the seam;
  it wants the same test-it-hard treatment.

## Status

- **Rope:** rewritten, 184 lines, 16 tests pass standalone — ready to promote.
- **Balancing:** designed, parked; only the `size` field landed.
- **Zipper:** the **navigation core is the new `zipper-core.rkt`** — the stack
  machine above, `smr` threaded alongside the guide; **35 checks pass** against the
  new rope (char guide). `rope-core` now also exports `tree-size` (for the smr-free
  `gap?`). The old zipper/summaries remain in `deprecated-4/`. **Segs designed, not
  built.** **Next:** build the seg layer (the polymorphic gap/seg `lens`) and port
  the concrete guides into a new `summaries.rkt`, then fold balancing in.
