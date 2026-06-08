# Discussion — 2026-06-08 — with Claude

Continues `2026-06-07/4` directly. That session built the strip `descend` and left the
extraction half — `carve` / the `rope-guide` bundle in `bisect` — **designed, not built**,
with the gap's coincident-cut **affinity** parked (cuts that land at a sign-flip with no
literal `0`). This session works that extraction concrete and, in the process:

- renames `carve`'s splitting primitive **`trisect`** (the rope-level analog of `bisect`);
- settles the gap by giving the seg guide a **set-valued "at-both-boundaries" state**, with a
  **0-dominance** resolve and a `match/sets*` form — which *dissolves* the affinity choice;
- lands `trisect` as **two `bisect` calls with specialised rope-guides** (the `…/4` Part A
  promotion), **overriding** `…/4` Part B's "carve = a single span-split, not two bisects."

All **design**; nothing written into the project — `rope-core.rkt`'s `bisect` is still
single-mode, `zipper-core.rkt` has no `trisect`/`carve`. Design-led by the user; Claude wrote
the lower-level choices and the sketches.

## Part A — `trisect`, not `carve` (the name and the shape)

The splitting primitive is named **`trisect`**: it is to `bisect` what a slice is to a cut.

- `bisect : t [guide] -> (values l r)` — two pieces, `l ++ r = t`.
- `trisect : … -> (values l m r)` — three pieces, `l ++ m ++ r = t`, the middle `m` the slice
  (and `∅` for a gap).

`trisect`'s output is already the **splitter** contract `zipper-core` defines (`rope ->
(values ls m rs)`), the same shape `(lens smr)` consumes — so `carve` is just `trisect` poured
into the existing `lens`, with `m` becoming the focus and the put-crumb falling out. *Over* a
bespoke head-rewriting `carve` (`…/3` Part D's `(values focus put)` shape), which re-implements
what `lens` already does.

## Part B — the guide question: one seg guide vs two gap guides

A `trisect` cuts at two boundaries, so the question is how those two are named. Forks weighed:

- **single seg guide `q`** `(l r) -> {-2..2}`, with the two cut-edges *derived* as
  `gs = sgn(q-1)` (start) and `ge = sgn(q+1)` (end). The user's preferred ergonomics.
- **two gap guides `(gs, ge)`** carried explicitly (`…/2`/`…/3`'s pair). More direct to cut
  with, but reintroduces the pair the `…/4` descent abandoned, and admits a start/end misorder.
- **separate `navigate-gap` / `navigate-seg`** doors — quarantines all seg-only machinery, at
  the cost of two entry points and re-opening "the core names the kind" (`…/2` Fork 1, parked).

The deciding analysis — *the seg case is clean, the gap case is not*:

```
proper seg, cut moving across [start..end]:
  q          :  +2   +1    0   -1   -2
  gs=sgn(q-1):  +1    0   -1   -1   -1     <- reads 0 only AT start
  ge=sgn(q+1):  +1   +1   +1    0   -1     <- reads 0 only AT end
```

For a real seg the two derived edges each read a clean `0` at their boundary — one guide gives
two good cut-finders. But coerce a **gap** guide `g` as `q = 2g` (so `q ∈ {-2,0,2}`, the `±1`
"at-a-boundary" states never occur) and the edges **never read 0** — they sign-*flip*:

```
gap (q = 2g), cut across the point:
  q          :  +2   0   -2
  gs=sgn(q-1):  +1  -1   -1     <- flips +1->-1 AT the point, no 0
  ge=sgn(q+1):  +1  +1   -1     <- flips +1->-1 just AFTER, no 0
```

That missing `0` *is* `…/4`'s parked affinity (which side of each flip the cut lands so the two
coincide → `m = ∅`). So a single coerced guide is clean for segs and **not a clean landing for
the gap** — exactly the user's worry. Keeping two *native* gap guides dodges it (each keeps its
own `0`), but at the cost of the pair. The asymmetry behind the choice: **pair → combined is
free** (`sgn(gs+ge)`, all the descent needs); **combined → pair is lossy** (recovering the edges
needs the `±1` shift, which is what kills the gap's `0`). The resolution (Part C–E) keeps the
single guide *and* recovers the clean gap.

## Part C — the missing state, and why it cannot be a number

The gap wants a guide value that the **start** view reads as `+1` and the **end** view reads as
`-1` *at once* — i.e. `sgn(v) = sgn(v-1) = sgn(v+1) = 0` simultaneously. `sgn(v)=0` forces
`v=0`, but then `sgn(v-1) = -1`. **No number works** — this is `…/2`'s "a gap is not a
degenerate seg" obstruction, in arithmetic form. So the gap state must be a **distinct object
off the integer line**; there is no clever numeric encoding hiding from us. Ruled out along the
way:

- **a number** — impossible, as above.
- **an object `equal?` to several numbers** — Racket won't consult a struct's `prop:equal+hash`
  when the other side is a plain number (different types short-circuit `#f`), so you can't make
  it `equal?` to `0`; and making one value equal to `-1,0,1` would collapse those into one
  equivalence class everywhere `equal?` (hence `match` literals) is used. Poisonous; rejected.

The honest model: the object is **two-faced / per-observer** — the seam view reads it `0`, the
start view `0`-as-if-`+1`, the end view `0`-as-if-`-1`. Its value depends on who is looking,
which is precisely why one number (read the same by everyone) cannot be it.

## Part D — representation: the set `{-1,0,1}` with **0-dominance** resolve

Represent the state as the **set `⋆ = {-1,0,1}`**, make *every* probe a set (an exact signal is
a singleton `{n}`; the gap-set is the only multi-element one that ever arises, since a proper
seg's `sgn` is always a singleton), and route through one resolve:

```racket
(define (resolve s)            ; probe -> one signal
  (cond [(not (set? s)) s]     ; bare number passes through (exact probes need no allocation)
        [(set-member? s 0) 0]  ; 0-DOMINANCE: ambiguous-at-a-boundary = "at the cut" = contained/halt
        [else (set-first s)])) ; otherwise the unique +/-1
```

Each consumer then just looks for *its* element: the **seam** halts on `0`, the **start** cut on
`+1`, the **end** cut on `-1`. For the gap-set all three find their element. Matching is a
membership pattern, auto-applied:

```racket
(define-match-expander in                          ; "this signal is k": equals k, or a set with k
  (syntax-rules () [(_ k) (app resolve k)]))
(define-syntax (match/sets* …)                     ; like match*, but every integer literal -> (in n);
  …)                                               ;   _ and identifiers pass through unchanged
```

The payoff: wrapped in `match/sets*`, the strip-descent `toward` table is **byte-for-byte
today's numeric table** — the gap-set resolves to `0` and flows through the existing `0` row.
And the earlier "strict guards" requirement **dissolves**: a gap at a focus edge makes the edge
probe `{-1,0,1}`, and `resolve = 0 ≠ -1`, so the bare `[(-1 _ _) error]` "precedes" guard cannot
misfire — while a *genuine* precede is `{-1}` → `-1` → fires. 0-dominance separates the two for
free, so all patterns can be uniform.

**0-dominance is the load-bearing decision.** "Ambiguous-at-a-boundary resolves to contained"
is also a quiet ruling on the parked **touching-semantics** (a boundary-flush edge counts as
*in*), and it is what removes the affinity choice — the gap's two cuts both resolve `0` at the
same point, so `m = ∅` necessarily, with no side to pick. *Alternative (parked):* make `resolve`
a parameter if a future case wants a touching edge treated as *out*.

*Alternatives weighed for representation:* a plain **sentinel + per-projection collapse**
functions (lighter, guard-safety becomes *structural* since the collapse hands the table a
number — but it's a one-off that doesn't generalise); the set is heavier (`racket/set`,
membership) **but** it is the true denotation and unifies with the parked **0-plateau / fiber**
(itself an interval/set), which is the reason it was chosen — if the fiber lands, `(in k)`
already covers it.

## Part E — the coercion: `gap-guide -> -2 · {-1,0,1} · 2`

```racket
(define ((gap->seg g) l r)
  (case (g l r) [(1) 2] [(-1) -2] [(0) (set -1 0 1)]))   ; before / after / AT-the-point = ⋆
(define (shift* x d) (if (set? x) (for/set ([v x]) (+ v d)) (+ x d)))
(define ((start-edge q) l r) (sgn* (shift* (q l r) -1)))  ; resolve 0 at START
(define ((end-edge   q) l r) (sgn* (shift* (q l r) +1)))  ; resolve 0 at END
```

The check the whole thing turns on — both edges land at the gap, by 0-dominance:

```
cut vs the point:   before |        at         | after
  q (coerced):        +2   |     {-1,0,1}       |  -2
  start-edge:        {+1}  | {-1,0} resolve 0   | {-1}
  end-edge:          {+1}  | {0,1}  resolve 0   | {-1}
```

Both resolve `0` at the **same** point — and note each column is just `-1|0|1`: *for a gap, both
derived edges collapse back to the original gap guide `g`*. So the single coerced guide
reproduces the clean "two equal gap guides" behaviour, with no affinity. (A positive-width seg
never emits `⋆`, so `⋆ ⟺ gap` — the set *is* the gap/seg distinction, placed in the guide value;
this answers `…/2` Fork 5's "plateau vs carried bit": neither, it's `set?`.)

## Part F — `trisect` = two `bisect` calls with specialised rope-guides

The split loop does **not** live in the zipper. It is the `…/4` **Part A `bisect` promotion**:
`bisect`'s optional arg becomes a ternary **rope-guide** (`(l-bundle r-bundle) -> {-1,0,1}`;
`0` is a *band*, not a point), `bisect` itself does the guided descent, and `trisect` is just
two calls.

```racket
;; rope-core: bisect t [guide] -> (values l r). guide reads a (summary . size) bundle of each
;; full side; bisect threads the running accumulation as it descends, splitting a straddling
;; leaf via split-leaf so the cut can land at an EXACT summary boundary. Default = rough balance.
(define ((balance ok?) lb rb)                      ; reads SIZE, ignores summary
  (cond [(ok? (cdr lb) (cdr rb)) 0] [(> (cdr lb) (cdr rb)) -1] [else 1]))

;; zipper/summary layer: lift a summary boundary into a rope-guide (reads summary, ignores size)
(define ((edge->rope-guide edge smr lo hi) lb rb)
  (edge (smr lo (car lb)) (smr (car rb) hi)))      ; closes over the constant outer context lo/hi

(define (trisect q smr h)
  (match-define (head b t a) h)
  (define-values (l t1) (bisect t (edge->rope-guide (start-edge q) smr b a)))      ; cut #1 (start)
  (if (set? (q (smr b l) (smr t1 a)))                                              ; ⋆ -> it's a gap
      (values l (empty smr) t1)                                                    ;   m = ∅, no cut #2
      (let-values ([(m r) (bisect t1 (edge->rope-guide (end-edge q) smr (smr b l) a))])
        (values l m r))))

(define ((carve q smr) h) ((lens smr) (lambda (_) (trisect q smr h)) h))           ; lens unchanged
```

**Overrides `…/4` Part B.** That note reversed *away* from two-sequential-`bisect` to a single
span-split, because the second cut would have to edge-land `∅` out of a non-empty `t1` —
something `bisect` cannot do. The **`⋆` short-circuit** removes that exact objection: on a gap,
`q` at the first boundary *is* the set, we detect it, and **the second `bisect` is never
called** — so Part B's principle ("`bisect` is never asked to manufacture an empty half") still
holds, yet the architecture stays two-calls. The cost is only that the gap/seg branch is the
explicit `set?` test in `trisect` rather than implicit in one span pass — which is the same
`set?` that serves as the gap/seg detector.

**Candidate resolution of `…/3` Part E** (how the exact-cut guide sees context as it descends
into a straddling child): `bisect` threads the **within-node accumulated** `(summary . size)`
bundles, and the rope-guide closes over only the *constant* outer `(lo, hi)` — so no per-level
re-currying is needed. `balance` reads the size field, a boundary guide reads the summary field;
that is what the `(summary, size)` bundle buys (`…/4` Part A).

## Status

- **Nothing built.** `rope-core.rkt`'s `bisect` is still single-mode (boolean `good-enough?`);
  `zipper-core.rkt` has no `trisect`/`carve`/`gap->seg`/edges/`match/sets*`. This note is the
  **design for the next build**.
- **Settled this session:** `trisect` is the `(l m r)` analog of `bisect` and a `lens` splitter;
  no number can be the gap state (so it's a distinct object); the state is the set `{-1,0,1}`
  with **0-dominance** resolve and a `match/sets*` form (table unchanged, guards safe); the
  `-2 · {-1,0,1} · 2` coercion lands both edges at the gap; `trisect` = two rope-guided `bisect`
  calls + the `⋆` short-circuit; `carve = (lens) ∘ trisect`.
- **Files this commits to changing (next build):** `rope-core.rkt` (`bisect` → rope-guide +
  guided descent + `(summary . size)` bundle, `balance` as the default rope-guide);
  `zipper-core.rkt` (`gap->seg`, `start-edge`/`end-edge`, `sgn*`/`resolve`/`in`/`match/sets*`,
  `edge->rope-guide`, `trisect`, `carve`; retrofit `toward` under `match/sets*`).

## Open / parked

- **`bisect`'s guided-descent loop** — the real remaining work: borrow whole children when they
  don't straddle vs. descend + `split-leaf` into the one that does, threading the
  `(summary . size)` accumulators. The part to test hardest (mid-leaf boundaries, a cut exactly
  at the document edge).
- **0-dominance** — load-bearing; it also settles touching-semantics (boundary-flush = *in*).
  Parked alternative: make `resolve` a parameter if a case ever wants touching = *out*.
- **The `(summary, size)` bundle scope** — balance wants *local-to-`t`* sizes, a boundary guide
  wants *global* (outer context folded in); reconciled by threading within-node accumulators and
  closing over the constant outer context, but unbuilt.
- **Subsumed for the gap:** the `…/4` coincident-cut **affinity** — 0-dominance makes the two
  cuts resolve `0` at the same point, so there is no side to choose. (Still open as a general
  knob only if `resolve` is later parameterised.)
- **Carried over:** the fine pin (atom-ids / within-fiber offset / structural-only); the
  **0-plateau / fiber** (the set representation is the candidate home for it via `(in k)`);
  seg-at-seam touching; cursor-shift / affinity for non-gap fibers.
