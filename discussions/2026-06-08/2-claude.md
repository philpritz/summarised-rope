# Discussion — 2026-06-08 (2) — with Claude

Continues `2026-06-08/1` directly. That session designed `trisect`/`carve` over a **set-valued**
gap state (`⋆ = {-1,1}`, `resolve`/straddle, `match/sets*`), built on the single sign-coerced
guide. This session **pinpointed that the set was a symptom** of a deeper choice — collapsing a
comparator *pair* into one value — and reversed it: the guide becomes the **pair itself** (the
point-interval relation), the gap is its diagonal, and the entire set layer dissolves. The move
is backed by the temporal-reasoning literature (point-interval relations / the pointized
endpoint representation). All **design and diagnosis**; nothing built. The one unbuilt primitive
(`bisect`'s rope-guide) is unchanged.

## Part A — The diagnosis: the `-2…2` guide is a lossy *sum*

The single seg-state value `q ∈ {-2,-1,0,1,2}` is the **sum of two boundary comparators**:
`s = cmp(start, cut)`, `e = cmp(end, cut)`, each `∈ {-1,0,1}` (`+1` boundary right of the cut,
`-1` left, `0` at). With `start ≤ end` the pair is ordered `s ≤ e`, and:

```
              (s , e)   s+e   = original q
before        ( 1, 1)    2
at-start      ( 0, 1)    1
inside        (-1, 1)    0
at-end        (-1, 0)   -1
after         (-1,-1)   -2
gap           ( 0, 0)    0
```

The sum is a **bijection on five of the six cells** — the only collision is at `0`, where
`inside (-1,1)` and `gap (0,0)` both land. So the `-2…2` value is *the pair minus one bit* — the
gap/inside bit — and that bit exists only at `sum = 0`. It is exactly the bit **`carve` needs**
(gap → `m = ∅`; inside → a real slice) and the one **`descend` doesn't** (both halt). The
`⋆`-set / `resolve` machinery from `/1` was an attempt to carry that lost bit *inside a single
value*; but `resolve-at` only ever reads the set's `[min,max]`, so the set was a clumsy interval.
The honest object that carries the bit is the **pair**.

## Part B — Decision: the guide is a relation **pair**

`q : (L,R) → rel`, where `rel` is the ordered pair `(s . e)`. The gap is the **diagonal**
`(0 . 0)`; an inverted index is `s > e` (unrepresentable / an error); the old `-2…2` is
`(+ s e)`, recoverable any time.

```racket
(struct rel (start end) #:transparent)              ; start <= end, gap = (rel 0 0)
(define ((index->guide x y) L R) (rel (cmp x L) (cmp y L)))   ; cmp = sgn(boundary - position)
```

**The deciding reason — an index maps straight into it, with no special-casing.** `[x y] → (rel
x y)` directly, and `[n n]` lands on the diagonal `(rel 0 0)` — the gap needs no separate
encoding; it is just the case where the two endpoints coincide. This was the chosen-first basis
(per *Record the reason as a dependency*: it is what this decision hangs on — if a future index
scheme made the mapping no longer direct, the choice would be back open). The two reasons below
are **later support, not the basis**:

- *the invariant is structural.* `start ≤ end` is enforceable on the single ordered object, so an
  inverted index is unrepresentable rather than a precondition you hope holds. Two separate guide
  functions `gs`,`ge` *can't* enforce `gs ≤ ge`, which is why the single pair is better-typed.
- *the literature agrees* (Part F): the pair is the standard pointized / point-interval relation.

**Representation — chose the `rel` struct** over `cons` and `values`:
- `values` is wrong: it models a transient multi-return, but the relation is a value that *flows*
  (`ascend` holds it, `carve` compares it to the gap).
- `cons` works but is untyped — `car`/`cdr` don't say start/end and can't carry the invariant.
- the **struct** gives named accessors and makes `s ≤ e` structural.
- *Split convention:* the relation flows → struct; the four reads `toward` needs are transient
  (consumed straight into a `match*`) → returned as `values`. The arity picks the convention.

*Set approach (`/1`) kept as the recorded fallback* if the pair ever bites.

## Part C — The three projections (one pair, read three ways)

This is 3-claude Part A, now literal — the same `rel` feeds all three ops:

- **descend** *sums*: `sgn(s + e)` — the quintic; halts at `s+e = 0` (inside **or** gap).
- **carve** *uses both*: cut at `s = 0` (start), then `e = 0` (end); the gap is the diagonal,
  detected as `(zero? (rel-end …))` at the start landing.
- **ascend** *selects*: `s` within the left edge, `e` within the right edge.

The reads collapse to **"start watches left, end watches right"**: `s@left`, `s@seam`, `e@seam`,
`e@right`. `s` is never read on the right, `e` never on the left.

## Part D — `toward`, the four-column form

`descend` halts at the minimal node; the step is a `match*` over the four reads, each row a single
component (no `or`), halts pulled first:

```racket
(match* (sL sM eM eR)                       ; s@left │ s@seam │ e@seam │ e@right
  [(-1 _ _ _) (error 'toward "precedes")]    ; start escaped left   (= ascend's left guard)
  [(_ _ _ 1)  (error 'toward "follows")]     ; end escaped right    (= ascend's right guard)
  [(_ -1 1 _) (values h k)]                  ; seam strictly inside -> halt
  [(_ 0 0 _)  (values h k)]                  ; seam on the gap      -> halt
  [(_ _ 1 _)  (if (equal? lt mt) (values h k) (into (half-r lt rt mt)))]   ; end right of seam -> R
  [(_ -1 _ _) (if (equal? rt mt) (values h k) (into (half-l lt rt mt)))])  ; start left of seam -> L
```

Properties: every valid case lands in exactly one row; an **inverted** guide produces a tuple
(e.g. `s_seam=1, e_seam=-1`) matching **no** clause → `match*` errors rather than mis-routing, so
`s ≤ e` is enforced structurally here too. The two guards *are* `ascend`'s containment test.
*Variations weighed* (all equivalent routing): collapse-every-column (smallest, fuses inside/gap),
pairs-in-all-columns (uniform, edges carry unused detail), pair-only-at-the-seam (keeps the
gap/inside bit), four-column (chosen — no `or`, each case caught, invariant structural).

## Part E — `ascend`, `carve`, `navigate`

```racket
(define ((contains? q smr) h)                ; ascend's select: gs@left, ge@right
  (match-define (head b t a) h)
  (and (not (negative? (rel-start (q b (smr t a)))))
       (not (positive? (rel-end   (q (smr b t) a))))))

(define ((trisect q smr b a) t)              ; carve: cut under start, then end
  (define ((cut pick lo hi) lb rb) (pick (q (smr lo (car lb)) (smr (car rb) hi))))
  (define-values (l t1) (bisect t (cut rel-start b a)))            ; cut #1: start boundary
  (if (zero? (rel-end (q (smr b l) (smr t1 a))))                   ; end also here -> GAP
      (values l (empty smr) t1)                                    ;   m = ∅
      (let-values ([(m r) (bisect t1 (cut rel-end (smr b l) a))])
        (values l m r))))
```

`carve` wraps `trisect` in `(lens smr)`; `navigate = carve ∘ descend ∘ ascend`. This **restores
the gap-as-empty-focus** that `/1` deferred (an empty `m` becomes the focus → `gap?` true → insert
works through `over`).

*Interface fork (open):* the guide is fundamentally **binary** (`(L,R) → rel`, pure, no `smr`);
`toward`'s **four reads** come from a `probe` that combines the focus's four pieces (`b lt rt a`)
and so needs `smr`. Keep them split (pure relation + `smr`-aware `probe`), or fuse via
`case-lambda`. Leaning split for purity. (Same shape generalizes: a relation swept over the cuts a
structure exposes — one cut → the pair, a bisected focus → the four reads.)

## Part F — Literature backing

Why the pair is the right object, in others' terms:
- The five seg-states **are** the **point-interval relations** (Vilain). The pair `(s,e)` is the
  **pointized / endpoint representation** (Vilain–Kautz, van Beek, Meiri) — the tractable point
  layer; whole-interval relations are the harder one.
- **Ligozat's geometry:** an interval is a point above the diagonal `s ≤ e`; relations are regions;
  the **gap is the degenerate vertex on the diagonal**, flush edges are the lower-dimensional
  boundary cells (a `0` in a coordinate = on a partition line), and the well-formed region is the
  **convex** (preconvex) triangle. Our `s ≤ e` invariant = that region.
- **Freksa's semi-intervals:** reasoning with the two boundaries separately = our two components;
  conceptual-neighborhood coarsening = `descend`'s `sgn(s+e)`.
- **ORD-Horn** (Nebel & Bürckert): endpoint order-literals (`≤,=,≠`) with a Horn restriction is the
  maximal tractable class. The **non-Horn / non-convex boundary** (e.g. "outside on either side" =
  `before ∨ after`) is exactly where **one ordered pair stops sufficing** and you'd need a
  disjunction/set — fine for a single cursor or selection (always convex), the thing to extend for
  disjunctive/multi-range targets.
- **Interval-primitivism** (Allen–Hayes `meets`, Whitehead, Russell): the rope's
  `head (before · focus · after)` is already an interval ontology — a **gap is the `meets`** of
  `before` and `after`, atoms are **moments**. The design lands at the synthesis: interval-primitive
  *data*, point-reductionist *addressing* (the pair, gap = the constructed diagonal). The `⋆`-set was
  the failed attempt to make the *meeting* a first-class value with content — which interval
  primitivism says you can't cleanly do.

## Status

- **Nothing built** (diagnosis + design). The set machinery in `/1` (`⋆`, `resolve`/`resolve-at`,
  `match/sets*`, `gap->seg`) is **superseded** by the pair.
- **Settled:** guide = ordered relation pair `(rel start end)`, gap = the diagonal; `-2…2 = s+e`;
  the three projections (descend sums, carve splits, ascend selects); the four-column `toward`;
  `carve` restores the gap-as-empty-focus.
- **Files this commits to changing (next build):** `zipper-core.rkt` (`rel`, `index->guide`,
  four-column `toward`, `ascend`, `trisect`/`carve`, `navigate`); `rope-core.rkt` (`bisect` →
  rope-guide + guided descent — *the one real remaining build*).

## Open / parked

- **`bisect`'s guided-descent loop** — the unbuilt core (borrow-vs-descend + `(summary . size)`
  accumulator threading); everything above bottoms out here.
- **Guide interface** — binary relation + `smr`-aware `probe` (leaning) vs a fused `case-lambda`.
- **Well-formedness check placement** — `x ≤ y` once at `index->guide`, or a per-`rel` guard.
- **Half-open index framing** (Dijkstra EWD831): gap `= [n,n)`, char `= [n,n+1)` — an actionable
  reframing that may remove the remaining special-casing; parked.
- **Disjunctive / multi-range targets** would exceed one ordered pair (the ORD-Horn/convex
  boundary) — would need a set/disjunction.
- **Fallback:** the `/1` set/single-value approach, if the pair ever bites.
- Carried over: the fine pin (atom-ids / within-fiber offset / structural-only); fiber affinity —
  now **subsumed** for the gap (the diagonal is unambiguous); seg-at-seam touching.
