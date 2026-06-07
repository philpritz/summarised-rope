# Discussion — 2026-06-07 (2) — with Claude

Continues `2026-06-07/1-claude.md`. That session settled the *direction* (anchor +
swappable measure, index encoding A, both frontiers innermost-first) and re-affirmed the
gap/seg-unified carve machine in the abstract (its Parts F/G). This session makes that
machine **concrete**: the ternary descent, the `ascend`/`carve` bookends, and a
generalisation of `bisect`. All **design**; nothing written into the project files —
`zipper-core.rkt` still carries the gap-only descent. Design-led by the user; Claude wrote
the sketches and the lower-level choices.

## Part A — The seg→descent transform: ternary, with the quintic kept for carve

A seg cursor is a **pair of boundary comparators** `(gs, ge)` — start edge, end edge — each
a guide returning `-1 | 0 | 1` against a cut (`-1` target left of the cut, `+1` right, `0`
at it). Descent only ever needs a **ternary** routing signal, so the two comparators
collapse to one:

```racket
(define ((combine gs ge) l r)
  (cond [(negative? (ge l r)) -1]    ; even the END is left of the cut  -> whole seg left  -> descend L
        [(positive? (gs l r))  1]    ; even the START is right of the cut -> whole seg right -> descend R
        [else                  0]))  ; the cut touches/lies in [gs,ge]   -> straddle -> terminate
```

On the diagonal `gs = ge`, `combine` *is* the gap guide, so a gap flows through the same
descent unchanged.

The structural reading: the seg signal is the **sum** of the two comparators, in `-2..2`
(the "quintic"). Descent **clamps the extremes** (`±2 → ±1`, descend) and the middle three
`{-1, 0, 1}` are the terminal (the cut touches or lies inside the seg). For a **gap**
`gs = ge`, the sum is even-only (`-2/0/2`), so the terminal collapses to the single point
`0`; for a **seg** the `±1` values appear (one edge touching the cut, the other not), so the
terminal fattens to an interval.

**Decision — ternary descent, quintic only at carve.** *Over* a native 5-valued (`-2..2`)
descent machine: the ternary keeps the descent loop single-signal and makes the gap *literally*
the diagonal of the seg, no special case. The quintic isn't discarded — it is the data the
**carve** reads to place the two cuts; descent just doesn't need it.

The same `(gs, ge)` pair is **projected three ways**, and the gap is the diagonal of all three:

- **ascend** *selects* — `gs` at the left edge, `ge` at the right (Part C);
- **descend** *collapses* — `combine` at the seam (Part B);
- **carve** *uses both* — `gs` and `ge` as two cut-finders (Part D).

## Part B — descend, concretely (settled)

Keep the existing three-probe `toward` table **verbatim** (left edge | seam | right edge),
feed it `combine`, and change only the **terminal**: the three former gap-landing rows now
**halt** instead of landing or carving. `toward` becomes a full machine step
`(head stack) -> (values head stack)` (fed the crumbs too, so the halt is the *identity
step* — no `#f` sentinel), and `descend` iterates it to a fixpoint:

```racket
(match* ((probe mt t) (probe lt rt) (probe t mt))   ; left edge | seam | right edge
  [(0 _  _) (values h k)]                           ; halt (was edge-l)
  [(_ _  0) (values h k)]                           ; halt (was edge-r)
  [(_ 0  _) (values h k)]                           ; halt (was seam)
  [(1 -1 _) (into (half-l lt rt mt))]               ; descend L  (unchanged)
  [(_ 1 -1) (into (half-r lt rt mt))]               ; descend R  (unchanged)
  [(-1 _ _) (error 'toward "seg precedes the focus -- ascend further")]
  [(_ _  1) (error 'toward "seg follows the focus -- ascend further")])
```

- **Why keep all three probes** (not just the seam). The two edge reads catch a boundary
  **flush with an atom or document edge** — a position that is *never* an interior seam — so
  there is no atom special case. A *proper* seg (positive width) always halts on an interior
  seam, so it would survive on the seam read alone; but a **zero-width gap at an extreme edge
  loops forever** without the edge read (worked: `point@3` over `"abc"` keeps routing right
  into the last atom, whose seam can never reach offset 3). The gap is the hard case, and it
  is exactly why the three points stay.
- **Gap reduction is exact.** `combine` on the diagonal = the gap guide; same probes, same
  rows; the empty-focus *landing* the old descent did inline now moves into the **degenerate
  carve** (Part D). Nothing about the gap's behaviour is lost.
- **Seg-only wrinkle (not a bug).** A seg edge flush with a focus edge fires a halt row early,
  returning a **non-minimal** node. Correctness is fine (carve cuts any containing node); it
  just isn't the smallest. This is the "how much to narrow before carving" knob from
  `2026-06-02/1`.

**Settled:** `descend` is the plain `define-values` loop (see Part F for why the fancier forms
were explored and dropped).

## Part C — ascend (designed): select, don't collapse

Ascent rises until the focus **fully contains** the seg — both edges inside. It does *not*
use `combine`; it **selects** one comparator per edge:

```racket
(define ((contains? gs ge smr) h)
  (match-define (head b t a) h)
  (and (not (negative? (gs b (smr t a))))    ; start not left of the focus's LEFT edge
       (not (positive? (ge (smr b t) a)))))   ; end   not right of the focus's RIGHT edge
```

`ascend` is otherwise the current shape (rise until `contains?` or root). Reduces to today's
gap `contains?` when `gs = ge`.

**Why select, not collapse.** Containment asks "is *each* seg-edge inside the focus," which
needs one comparator at *each* edge. `combine` is non-strict-blind at a boundary: its `0`
bucket merges "edge strictly outside" with "edge exactly touching," and containment is
**non-strict** (a touching edge still counts as contained), so collapsing would over-climb
whenever an edge sits on a focus boundary. So descent collapses (it only cares strictly-left /
strictly-right / not), ascent selects (it must keep strict-vs-touching). *Forcing ascent
through `combine` is possible only if you accept strict-only containment* — parked, since that
changes the touching semantics (one of the `2026-06-02/1` `0`-plateau cases).

## Part D — carve (designed)

After descend halts at the minimal straddling node `(b, t, a)`, carve cuts `t` at the two
boundaries into `(l, m, r)`, focuses the slice `m`, and folds `l`/`r` away — a `lens` whose
splitter does **two sequential guided cuts**:

```racket
(define (carve gs ge smr h)
  (match-define (head b t a) h)
  (define-values (l t') (bisect t gs b a))            ; cut at gs:        l | t'
  (define-values (m r)  (bisect t' ge (smr b l) a))   ; cut remainder ge: m | r
  (values (head (smr b l) m (smr r a))                 ; focus the slice m
          (lambda (h*) (head b ((make-rope smr) l (head-rope h*) r) a))))  ; put rebuilds l·m·r
```

The new anchors get the **summaries** of `l`/`r` (for later guide reads); the put crumb closes
over the **ropes** (for editing) — the dual the existing `lens` already provides.

- **Gap = degenerate carve.** With `gs = ge` the second cut lands at the very left edge of
  `t'`, so `m = ∅`, `r = t'` — an empty focus at the boundary. No special case; "forgoing
  carving for gaps" is just the short-circuit that skips the redundant second cut.
- **Role split — why descend halts at the node first.** Descend builds the crumb stack down to
  the *minimal* straddling node, so the carve is **one** lens (one crumb) on a *small* node:
  the put rebuilds only the node, not the document, and navigating away stays incremental.
- **Parked optimisation — the quintic-shared descent.** The two cuts re-descend independently
  from `t`. The quintic terminal (`{-1,0,1}`: both cuts in `lt` / straddle / both in `rt`)
  could route a *single* descent that shares the two cuts' common prefix and forks at the
  straddle. Chosen the simple two-cut version (the node is minimal, so the shared prefix is
  short); the fused version is parked.

## Part E — generalising bisect (designed; provisional, recut-able)

One entry, two modes:

- `(bisect t)` — today's **rough size-balanced** split.
- `(bisect t good-enough)` — a **summary-dependent exact cut** (the guided split returning two
  ropes, i.e. the old `bisect-at` folded in). It uses the rough `(bisect t)` as its *halving
  step* — clean layering, no mutual recursion. For carve, `good-enough` curries the node's
  context `(b, a)` and reads the guide at the candidate boundary.

**The size/summary tension, and why it resolves to two modes.** Balance reads `size` *and*
`height` (node fields, off the summary); the exact cut reads the **user summary** (threaded
context). Different sources. The decisive fact: **height is non-additive** (`1 + max`), so it
cannot live in a summary *at all* — and balance uses it. So balancing is *constitutionally*
off-summary, not by preference.

- **Option 1 — put `size` in the summary. Rejected.** Doesn't help the part that needs it
  (height still can't go there), duplicates `size` into two sources that `rebalance` /
  `pathological?` would have to keep consistent, and re-couples balancing into the user's
  summary algebra — undoing the `size`-as-a-field separation from `2026-06-02/1` (the sexp
  summary tracks no chars at all).
- **Option 2 — no-arg default reads the node fields; the guide-shaped arg is the summary path.
  Chosen, but flagged provisional.** It is an honest *pragmatic bundling* — two genuinely
  different operations sharing a `bisect` name and a recursion skeleton — **not** a firm
  separation. The only firm thing is height's non-additivity forcing balance off-summary;
  sharing the name is convenience. **Parked for a possible recut.**

**Open detail the recut hinges on:** how `good-enough` sees the right context as an exact cut
**descends into a straddling child**. At the node's top level the context is fixed `(b, a)` and
the candidate `(l, r)` is the node's full partition, so currying `(b, a)` in is correct. But
descending into a child, the child's outer context is no longer `(b, a)` (the siblings fold
in), so `good-enough` must be **re-curried per level** (the "make functions currying the
context" idea) rather than curried once — *or* phrased as the bare comparator + threaded
context. Unsettled; this is what the recut turns on.

## Part F — descend as a fixpoint (explored, parked on the loop)

We explored expressing `descend` as `(fix toward)` — the fixpoint of a multiple-values step —
and **kept the plain loop**. Durable findings (so the cleanup isn't re-derived):

- `call-with-values` is **unavoidable** for *arity-generic* multiple-value capture; it's the
  only primitive that reifies an unknown count. `let-values` / `define-values` are its sugar
  and, at the fixed `(head, stack)` arity, are clean and allocation-free.
- **Composition** threads multiple values without reifying, but can't express the fixpoint
  *test* without observing the stepped result (= capture). `ascend`'s `compose` form avoids
  this only because its stop reads the **input** (`contains?`, cheap); `descend`'s stop is "the
  step was a no-op" over an **expensive** `bisect`, so do-then-check (capture) beats
  check-then-do (re-bisect). That asymmetry is *why* `descend` wants the loop and `ascend`
  doesn't.
- **Streams carry multiple values natively** (correcting an over-claim made mid-session):
  `stream-cons` / `stream-first` preserve `(values …)`; the library does the `call-with-values`
  internally. The fixpoint then reads as "first `n` with `sₙ = sₙ₊₁`," and stream **memoisation**
  makes `s` vs `(stream-rest s)` single-compute. But `stream-filter` is single-stream (filter
  is single-source; `map` is the variadic-over-many one), so "test the pair, keep the first" is
  a small custom combinator, not a built-in. All parked.

## Status

- **Designed, not built (this whole session):** the `combine` transform; the halt-terminal
  `descend` (three-probe, fixpoint loop); the two-comparator `ascend`; the two-cut `carve`; the
  `(bisect t [good-enough])` generalisation.
- **Nothing written into the project.** `zipper-core.rkt` still has the gap-only landing
  descent; `rope-core.rkt`'s `bisect` is still single-mode.
- Builds on `2026-06-07/1` (Part F gap/seg unification, Part G the flip) and `2026-06-02/1`
  (the polymorphic seg plan).

## Open / parked

- **Generalised `bisect`** — Option 2 is provisional; the context-delivery to `good-enough`
  on descent (re-curry per level vs thread bare comparator + context) is the open detail the
  recut hinges on.
- **Quintic-shared descent** — fuse carve's two cuts into one prefix-sharing pass. Parked.
- **Fixpoint-combinator cleanup of `descend`** (streams / compose) — parked; the loop kept.
- **Flush-edge early-halt** — descend can return a non-minimal node when a seg edge is flush;
  harmless, the narrow-knob.
- **navigate / set-guide assembly** — wiring `ascend → descend → carve` into one move, and
  where the `(gs, ge)` pair + `combine` get assembled, is not yet done.
- Still parked from before: the fine pin (atom-ids / within-fiber offset / structural-only),
  fiber affinity, the `0`-plateau / seg-at-seam touching cases.
