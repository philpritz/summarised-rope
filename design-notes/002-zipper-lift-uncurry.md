# 002 — Uncurrying the zipper machine ops (`smr`/`guides` at one level)

Status: **design draft, not applied.** Discussion 2026-06-14. Target file: `zipper-core.rkt`.

## The observation

The machine ops are triple-curried — `guides → smr → ((h k) → (values h k))`:

```racket
(define (((toward guides) smr) h k) ...)   ; and contains? ascend descend carve, navigate (double)
```

But both outer layers are **ambient context constant for an entire lift run**: `gs` and
`smr` come straight off the same `zipper` struct (`(match-define (zipper h k smr gs) z)`),
destructured side by side in `zipper-lift`, and never change while the ops execute.

Traced every path: `guides` enters the op pipeline at **exactly one point** — `(navigate gs)`
inside `zipper-lift` — and there `gs` is *always* `(zipper-guides z)`. Never caller-supplied,
never computed on the fly. (Install/modify build a *new* zipper with the new vector, then lift;
by the time the ops run, that vector IS the zipper's `guides` field.) So the two outer curry
layers reconstruct, by hand, a pair the zipper already holds as two adjacent fields.

## Decision

- **Not** a `ctx` struct. Mount the two as **two args at one level**: `(op smr guides) → ((h k) → …)`.
- **Reorder the struct** so the fixed pair leads: `(struct zipper (smr guides head stack) …)`.
  Then the reseal is plain `curry`, no `cut`.
- Thread each op the pair with a local **thrush** `pass` (function-position hole).
- Result: `srfi/26` (`cut`/`<>`) is no longer used anywhere → **drop it from `require`**.
  (Grep confirmed the only real `cut` uses were the two lines in old `zipper-lift`;
  every other "cut" is the word in comments / the `carve` op prose.)

`smr` before `guides` — matches the struct field order and the `zipper-lift` destructure.

## Naming aside

`((pass . args) f) → (apply f args)` is the **thrush** combinator `T x f = f x`. In Haskell:
`flip ($)`, exported as `(&)` from `Data.Function` (single arg). The n-ary "apply to a fixed
arglist" form has no canonical Haskell name (currying makes it moot); sometimes `applyTo`.
In Racket it's exactly `srfi/26`'s `(cut <> a b)` — `pass` just names that without the macro.

Considered `fancy-app` (`(_ smr gs)`): terser but rebinds the module's `#%app` so every `_` in
any application position becomes a hole, plus a third-party dep. Rejected — `pass` reads as well
without the global reinterpretation.

## The draft (apply to `zipper-core.rkt`)

```racket
;; ---------- thrush ----------
(define ((pass . args) f) (apply f args))      ; (pass a b) = (λ (f) (f a b))

;; ---------- machine ops : (op smr guides) -> ((h k) -> (values h k)) ----------
(define ((contains? smr guides) h)
  (match-let* ([(head b t a)   h]
               [(vector gs ge) guides])
    (and (not (negative? (gs b (smr t a))))
         (not (positive? (ge (smr b t) a))))))

(define ((ascend smr guides) h k)
  (if (or (null? k) ((contains? smr guides) h))
      (values h k)
      ((compose (ascend smr guides) rise) h k)))

(define ((toward smr guides) h k)
  (match-let*-values ([((head b t a))   h]
                      [((vector gs ge)) (vector-map (frame smr b a) guides)]
                      [(mt)             (empty smr)]
                      [(lt rt)          ((multisect) t)]
                      [(atom?)          (or (equal? lt mt) (equal? rt mt))]
                      [(into)           (lambda (split)
                                          (let-values ([(h* c) ((lens smr) split h)])
                                            (values h* (cons c k))))])
    (if atom?
        (values h k)
        (match* ((gs mt t) (gs lt rt) (ge lt rt) (ge t mt))
          [(-1 _ _ _) (error 'toward "start precedes the focus -- ascend further")]
          [(_ _ _ 1)  (error 'toward "end follows the focus -- ascend further")]
          [(_ 1 _ _)  (into (lambda (_) (values lt rt mt)))]
          [(_ _ -1 _) (into (lambda (_) (values mt lt rt)))]
          [(_ _ _ _)  (values h k)]))))

(define ((descend smr guides) h k)
  (let loop ([h h] [k k])
    (let-values ([(h* k*) ((toward smr guides) h k)])
      (if (eq? h* h) (values h k) (loop h* k*)))))

(define ((carve smr guides) h k)
  (match-define (head b _ a) h)
  (let-values ([(h* c) ((lens smr) (multisect (vector-map (frame smr b a) guides)) h)])
    (values h* (cons c k))))

(define (navigate smr guides)
  (compose (carve smr guides) (descend smr guides) (ascend smr guides)))

;; ---------- public zipper ----------
(struct zipper (smr guides head stack) #:transparent      ; fixed pair leads
  #:property prop:custom-write (lambda (z port mode) (zipper-show z port)))

(define (start smr rope) (zipper smr #f (head (smr "") rope (smr "")) '()))

;; curry reseals (smr/gs lead), pass threads each op the pair, navigate is permanent last op
(define ((zipper-lift . ops) z)
  (match-define (zipper smr gs h k) z)
  ((apply compose (curry zipper smr gs)
          (map (pass smr gs) (cons navigate ops)))
   h k))

;; --- goes THROUGH the lift ---

(define guide              ; install/modify call (zipper-lift) with NO extra ops
  (letrec ([install (curry (lambda (gs z)
                             (match-define (zipper smr _ h k) z)
                             ((zipper-lift) (zipper smr gs h k))))]
           [modify  (curry (lambda (f z) ((install (f (zipper-guides z))) z)))])
    (match-lambda
      [(? zipper? z)         (zipper-guides z)]
      [(? procedure? f)      (modify f)]
      [(and gs (vector _ _)) (install gs)])))

(define focus              ; set composes the lift with ONE extra op (the swap)
  (letrec ([read   (lambda (z) (head-rope (zipper-head z)))]
           [set    (compose zipper-lift
                            (curry (lambda (content smr guides h k)   ; guides unused
                                     (match-let ([(head b _ a) h])
                                       (values (head b ((make-rope smr) content) a) k)))))]
           [modify (curry (lambda (f z) ((set (f (read z))) z)))])
    (match-lambda
      [(? zipper? z)    (read z)]
      [(? procedure? f) (modify f)]
      [content          (set content)])))

;; --- stays OUTSIDE the lift (homing must not re-navigate; on-edges is a pure read) ---

(define (to-root z)
  (match-define (zipper smr gs h k) z)
  (zipper smr gs (foldl (lambda (crumb h) (crumb h)) h k) '()))

(define ((on-edges c f g) z)
  (match-define (zipper smr _ (head b m a) _) z)
  (c (f b (smr m a)) (g (smr b m) a)))
```

`lens` is **unchanged** — genuinely `smr`-only (`((lens smr) split h)`), doesn't want guides.

## Watch-outs when applying

1. `contains?` reads **raw** guides (edge reads); `toward`/`carve` go through
   `(vector-map (frame smr b a) guides)`. That asymmetry is intentional — don't "unify" it.
   (If factoring, a `framed` helper could absorb the two framed call sites, but that's a
   *separate* axis from this uncurry and was left out of this draft.)
2. The `focus` `set` op carries an unused `guides` arg purely to keep the uniform
   `(smr guides …)` op shape so `zipper-lift` can map a single `(pass smr gs)`. Alternative is
   special-casing `set` out of the shape — rejected, breaks the single map.
3. `gs` is `#f` until first install (`start`). `navigate` would choke on `#f` guides, but no
   path reaches it pre-install — same precondition as today, not a regression. `zipper-show`
   already handles `#f` → bare document.
4. Field reorder touches every `zipper` constructor + `match-define` site: `start`, `zipper-lift`,
   `guide` install, `to-root`, `on-edges`. Accessor *names* (`zipper-head`, `zipper-smr`, …)
   are unchanged.

## Next step

Apply to `zipper-core.rkt`, drop `srfi/26` from `require`, run `(module+ test)` to confirm
behavior-preserving. Tests live in the same file and exercise gap/seg/install/modify/focus/
on-edges/printing — they should pass unchanged.
```
