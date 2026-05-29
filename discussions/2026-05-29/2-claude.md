# Discussion — 2026-05-29 (2) — with Claude

Pure design session on the **rope library's public surface** — recasting
summaries, rope construction, and the split primitive as a small family of
variadic polymorphic functions, and re-cutting the file boundaries between
rope / summary / guide / zipper. The sketches below were **subsequently
implemented** — the rope library was rewritten in this style and merged to
master (see *Status*); the zipper is left for a later session. Started from the
original goal "replace
crumbs using lenses," which reframed quickly (see *Crumbs-as-lens*) and then the
session pivoted to settling the rope foundation the crumb/lens work sits on.

## Framing (important)

This explores a **heavier style of polymorphism than the user has typically
used**: single functions taking *interleaved arguments of mixed types at
variable arity*. It was treated as a trial — lock in only if it proves clean.
**Outcome:** it proved out for the rope, which was rewritten this way and merged;
the zipper is still to come.

## Summary as one variadic function

The rope's entire knowledge of summaries reduces to a **monoid + a measure**,
packaged as one callable. The rope only ever *builds, combines, caches/reads,
and forwards* summaries — it never looks inside one (that is the guide's job).

```racket
;; constructor: measure : string -> S,  combine : S S -> S   (author writes only these)
(define ((summary-algebra measure combine) . parts)
  (define (->s x) (cond [(string? x) (measure x)]
                        [(rope? x)   (rope-summary x)]   ; cached, see guard below
                        [else        x]))                ; already a summary
  (foldl (lambda (x acc) (combine acc (->s x))) (->s (car parts)) (cdr parts)))
```

- Variadic, **coerce-then-fold**, left-to-right (monoid is associative but
  **not** commutative — order is preserved).
- **No identity field.** `(summary "")` is the identity, relying on the measure
  being a monoid homomorphism (`measure "" = empty`) — the natural law anyway.
- Author supplies only unary `measure` + binary `combine`; the variadic lifting,
  string coercion, and rope coercion are added by the wrapper (which lives in
  the rope layer, so it is rope-aware while the author stays rope-blind).
- **Interleaving** (`(summary s1 "x" r2 s3)`) falls out for free.

## summary eats ropes (cached) with a same-algebra guard

Passing a **rope** uses its cached `rope-summary` — O(1), already paid for.
Valid only if the rope was summarised under the *same* algebra, so:

- Every rope **node is tagged with its algebra** (= the `summary` fn itself),
  compared by `eq?`.
- Cross-algebra -> **error now**; **reconstruction** (re-measure the rope's text
  under the new algebra, O(text)) is deferred (expensive).
- Consequence: `sys` is recoverable from any rope via `(rope-algebra r)`, so most
  rope ops **don't need a `sys` parameter** — they pull it from the rope. Only
  construction *from strings alone* still needs it passed.

## rope: the same shape, string/rope carrier

```racket
(define ((rope sys #:chunk-size [chunk 1024]) . parts)
  (define (->rope x) (if (string? x)
                         (apply (concat-rope sys) (map (leaf-rope sys) (chunk-string x chunk)))
                         x))
  (apply (concat-rope sys) (map ->rope parts)))
```

- Strings are **chunked into leaves** (decided: yes, split strings up); ropes
  pass through; folded by concat.
- Subsumes `string->rope` (chunk+assemble) and `concat-rope` (all-ropes case).
- Homomorphism: `(rope-summary (rope ... p ...)) === (summary ... p ...)` —
  build and measure commute.
- **Balancing deferred** (decided: want it, stay dumb for now). The fold is a
  right-leaning spine, so fresh loads are temporarily **O(n)** until the
  2026-05-28 balanced batch-build slots into the fold seam. A real (temporary)
  regression from today's balanced `string->rope`.

## The three carriers, and why `string` stays plain

summary / rope / string form a family of variadic coerce-and-fold builders:

```text
summary : (string | rope | summary)* -> summary
rope    : (string | rope)*           -> rope
string  : (char | string | rope)*    -> string
```

We **considered** making the third carrier the *builtin* `string` (so
`(string aRope)` = its text). **Rejected:**

- The only mechanism is **shadowing** (rename-in the primitive, redefine,
  provide). Racket has no way to retrofit polymorphism onto a primitive
  (`racket/generic` only defines *new* interfaces; `string` isn't one).
- Shadowing **doesn't compose**: two libraries each shadowing `string` collide;
  hygiene also stops the shadow reaching imported macros. It's a hack, not a
  general polymorphic mechanism.
- **Decision: export a plain `rope->string`** (direct leaf-walk). Conceptually
  it is summarisation under the trivial text algebra (`measure = identity`,
  `combine = string-append`) and the simplest instance of deferred
  reconstruction.

## split: the one-level eliminator (+ atom?)

- `split` is the **definite** descent primitive: one structural level, threads
  children's contexts, reads the guide, dispatches one way. No base case of its
  own.
- `split` is **total** — splitting an *atom* (leaf len <= 1) yields no-progress
  halves, so unguarded recursion would loop; `atom?` is therefore a
  **termination guard**, kept **internal**.
- Form: **guided, three-args, no struct**, `before mr after` order, **sys-free**
  (recovers the algebra from the rope). Draft `split-rope`:

```racket
(define ((split-rope guide) before mr after)
  (define s   (rope-algebra mr))
  (define (cat . rs) (apply (rope s) rs))
  (define empty (empty-rope s))
  (let walk ([before before] [mr mr] [after after])
    (define (decide L R)
      (case (guide (s before L) (s R after))
        [(0)  (values L R)]
        [(-1) (define-values (ll lr) (walk before L (s R after))) (values ll (cat lr R))]
        [(1)  (define-values (rl rr) (walk (s before L) R after)) (values (cat L rl) rr)]))
    (cond [(branch? mr) (decide (branch-left mr) (branch-right mr))]
          [(atom? mr)   (if (positive? (guide before (s mr after)))
                            (values mr empty) (values empty mr))]
          [else (define-values (L R) (split-leaf-piece mr)) (decide L R)])))
```

Currying kept: **guide first**, then the rope group. A `split` polymorphic over
guide type (gap-guide / seg-guide / none -> plain split) was considered then
**parked: keep conservative for now**.

## Layering insight (resolves the file-split confusion)

`summary` and `guide` are **duals over the same `S`**:

- the **algebra** (`measure`/`combine`) is the monoid structure *on* `S` — blind
  to its contents.
- the **guide** is a map *out* of `S` — the **only** thing that reads inside a
  summary (projects/compares) to a decision sign.

So **both algebra and guides are `S`-specific**, while **rope and zipper are
`S`-agnostic** (rope threads the summary fn and knows guides only by their sign;
zipper carries opaque summaries and calls guides). The right cut is
**generic-vs-specific**, not rope-vs-zipper:

- generic **rope**, generic **zipper**,
- one **`S`-specific module** holding the concrete `S` + its `measure`/`combine`
  **and** its guides together.

Today's split — algebra in `rope-core`, guides in `zipper-core` — tears the
`S`-specific code in half across two generic files.

## Proposed minimal rope exports

```racket
(provide summary-algebra rope rope->string split split-rope seg-split)
```

Internal (no longer public): node structs `leaf`/`leaf-range`/`branch`, `atom?`,
`concat-rope` (= `rope` on ropes), `rope-summary` (= `(summary r)`),
`empty-rope`, `summary+`/`empty-summary` (folded into `summary`), and all
leaf-piece / leaf-join / chunk / build-balanced machinery.

## Crumbs-as-lens (the original goal, reframed)

Established early, then set aside pending the rope foundation: a crumb =
**`(side . sibling)`**; **rise = join** (= `concat`/`rope`), **get = split +
pick a side**, **put = join with the sibling in its slot**. So the located
`split`/`join` algebra *is* the lens; the summaries the old
`opened-left`/`opened-right` stored explicitly are reproduced by join. Not
implemented.

## Status

**Implemented and merged to master:** the rope library was rewritten in this
style, replacing the old `rope-core.rkt`.

### Implemented (`rope-core.rkt`)

- `summary-algebra` -> one variadic `summary` fn (doubles as `sys`); nodes tagged
  with their algebra; same-algebra `eq?` guard (cross-algebra -> error).
- `rope` builder (chunks strings into leaves, dumb concat fold, re-fuses adjacent
  ranges, drops empties); subsumes the old `string->rope` / `concat-rope`.
- sys-free `split` (one-level eliminator), `split-rope`, `seg-split` — all
  `before mr after`, guide curried first; `rope->string` as a leaf-walk.
- Exports: `summary-algebra rope rope->string split split-rope seg-split`.
- Inline rackunit suite — **26 tests passing** (`raco test rope-core.rkt`).
- Old `rope-core.rkt` + `zipper-core.rkt` moved to `deprecated-3/` as reference.

### Not done (later sessions)

- **zipper** — untouched (in `deprecated-3/`); to be rewritten on the new rope
  (crumb = `(side . sibling)`, rise = join). The new `S`-specific module
  (concrete algebra + guides) also still to be carved out of both files.
- `README.md` is now **stale** (documents the old zipper API / `string->rope`).
- **Balancing** still deferred — `rope` folds into a right-leaning spine, so
  fresh loads are O(n) until the 2026-05-28 batch-build lands in the fold seam.
- **Empty selection (a = b)** is not a `seg-split` case — the ±1-offset machinery
  has a 2-wide dead zone, so a zero-width window can't be expressed; a point
  cursor is `split-rope`'s job.

Open / parked:

- guide-in vs guide-out for the eliminator (leaned **guide-in**, from "three
  args, no struct").
- local-rise vs root-based navigation (mostly moot now that summary composition
  is one fn).
- balancing implementation — deferred; seam = the fold inside `rope`.
- reconstruction — deferred; cross-algebra errors for now.
- `system` seam: collapsing `sys` into the summary fn closes the place to hang
  per-system config (chunk-size, balancing thresholds) — revisit.
- polymorphic-over-guide-type `split` — parked, keep conservative.

This whole direction is **provisional** — a trial of heavy interleaved/variadic
polymorphism the user hasn't leaned on before; adopt only if it stays clean in
real code.
