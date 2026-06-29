#lang racket

;; Small algebraic helpers -- isos, variadic van Laarhoven lenses, and a few
;; combinators -- each documented at its definition (the canonical home other files
;; point to; the provide list is the surface map). Intended as a reusable helper
;; library, so some surface is built out past what this project strictly needs.
;; Narrative -- the iso group law, the lens store-coalgebra theory, the inlining
;; rationale -- in scribble/helper-algebras.scrbl.

(provide (struct-out iso)        ; (iso to from); callable = applies `to`
         compose-iso             ; compose any number of isos; inverses reversed
         expt-iso                ; integer powers of an iso (scmutils function arithmetic)
         iso-law? check-iso-laws ; round-trip predicate; the inputs that fail it
         make-lens               ; (make-lens peek): a coalgebra -> a variadic lens
         iso->lens               ; view an iso as a lens (its put ignores the original -- that absence IS the iso)
         viewer setter updater   ; the lens ops, curried (viewer gets, optional k folds the view; setter/updater command)
         list-of                 ; map an element lens over a list -- ONE focus
         lref                    ; index a list, fanning to N foci; length-safe
         ldiag                   ; the list diagonal -- view i, put broadcasts to all
         varg                    ; rearrange the value stream by position
         vdiag                   ; the value-stream diagonal -- view i, put broadcasts to all (ldiag on values)
         pure                    ; (pure v ...): the constant fn, ignoring its args and returning the v ... as values (K)
         on                      ; (on op f) a ... = (op (f a) ...) -- Haskell's `on`
         arg                     ; project args by 0-based position
         pass                    ; apply each f to the fixed args, as values
         fork                    ; apply each f to the same arg(s), as values -- pass, functions-first
         spread                  ; apply each fn to its own arg, combine with h
         variadic                ; lift a binary op + seed to a variadic left fold
         fixed                   ; iterate to a fixed point
         lexicographic)          ; first-difference 3-way order on sequences

;; A focused (to, from) pair; prop:procedure runs `to`, so an iso is callable as its
;; forward function -- only its own combinators see the other half.
(struct iso (to from)
  #:property prop:procedure (struct-field-index to))

(define (inverse i) (iso (iso-from i) (iso-to i)))

;; Compose isos; the composite inverts the halves in reverse. (compose-iso) = identity.
(define (compose-iso . is)
  (iso (apply compose (map iso-to is))
       (apply compose (map iso-from (reverse is)))))

;; Integer powers of an iso, in the style of scmutils function arithmetic: n<0 uses
;; the inverse, so (expt-iso i -1) = (inverse i). Closed on isos.
(define (expt-iso i n)
  (cond [(negative? n) (expt-iso (inverse i) (- n))]
        [else (for/fold ([acc (iso values values)]) ([_ (in-range n)]) (compose-iso i acc))]))

(define (iso-law? i x) (equal? ((compose-iso (inverse i) i) x) x))   ; x round-trips unchanged
(define (check-iso-laws i xs) (filter (lambda (x) (not (iso-law? i x))) xs))   ; '() = genuine iso

;; make-lens: a store coalgebra `peek` -> a variadic lens over the value stream (one or
;; more foci inside a structure that is itself one or more values).
;;   peek : structvals ... -> (values put focus ...)   ; put-back FIRST, then foci
;;   put  : newfocus ...   -> structvals ...
;; One body serves view and set; the foci handler `k` picks the functor, view tagged
;; const-box. Composition is plain `compose`. Why put-first / Const vs Identity: scribble.
(struct const-box (vs))                                        ; private view tag; foci as a list
(define ((make-lens peek) k)
  (compose
   (lambda (put . foci)
     (let ([r (apply (compose list k) foci)])
       (if (and (pair? r) (null? (cdr r)) (const-box? (car r)))
           (car r)                                             ; view -> lone const-box: skip the put
           (apply put r))))                                    ; set/over -> rebuild from new foci
   peek))
(define ((viewer  l [k values]) . s) (apply k (const-box-vs (apply (l (lambda foci (const-box foci))) s))))  ; optional k folds the view (optics `views`)
(define ((setter  l . xs) . s) (apply (l (lambda _    (apply values xs))) s))
(define ((updater l . fs) . s) (apply (l (lambda foci (apply values (map (lambda (f x) (f x)) fs foci)))) s))

;; iso->lens: an iso worn as a lens -- view is its forward map, put is its backward map. The put
;; ignores the original structure (only the new focus matters), which is exactly what makes it an
;; iso rather than a general lens; so (compose some-lens (iso->lens i)) composes with no fuss.
(define (iso->lens i) (make-lens (lambda (s) (values (iso-from i) (i s)))))

;; list-of: a single-focus element lens lifted over a list -- ONE focus (the list of
;; views); the put rebuilds element-wise. Stays single-value until `lref` fans out.
(define (list-of el)
  (make-lens (lambda (xs)
    (values (lambda (ys) (map (lambda (x y) ((setter el y) x)) xs ys))
            (map (viewer el) xs)))))

;; lref: index a list at positions `is`, fanning into N foci; the put writes them back
;; into a copy. Length-safe -- overwrites slots, never reshapes.
(define (lref . is)
  (make-lens (lambda (xs)
    (define v (list->vector xs))
    (apply values
           (lambda nf (define w (vector-copy v))
                      (for ([i (in-list is)] [x (in-list nf)]) (vector-set! w i x))
                      (vector->list w))
           (map (lambda (i) (vector-ref v i)) is)))))

;; ldiag: the list diagonal -- view position `i`, put broadcasts one value to every slot.
;; The collapsing twin of `(lref i)`; lawful only when the slots are already equal.
(define (ldiag i)
  (make-lens (lambda (xs) (values (lambda (x) (make-list (length xs) x)) (list-ref xs i)))))

;; varg: the lens twin of `arg` -- focus the values at positions `is`, in order; the put
;; writes them back. Lawful for distinct positions; a repeated position is a lossy
;; diagonal (put-get fails).
(define (varg . is)
  (make-lens (lambda structvals
    (define v (list->vector structvals))
    (apply values
           (lambda nf (define w (vector-copy v))
                      (for ([i (in-list is)] [x (in-list nf)]) (vector-set! w i x))
                      (apply values (vector->list w)))
           (map (lambda (i) (vector-ref v i)) is)))))

;; vdiag: the value-stream diagonal -- view value `i`, the put broadcasts one value to every
;; position. The value-stream twin of `ldiag` (and the collapsing twin of `varg`); lawful only
;; when the positions are already equal.
(define (vdiag i)
  (make-lens (lambda structvals
    (values (lambda (x) (apply values (make-list (length structvals) x)))
            (list-ref structvals i)))))

;; pure: the constant function, variadic in its result -- (pure v ...) ignores its
;; arguments and returns the v ... as values. The K combinator, lifting plain values
;; into a transform that disregards its input (e.g. a re-edge that just installs v).
(define ((pure . vs) . _) (apply values vs))

;; on: (on op f) a b ... = (op (f a) (f b) ...) -- the n-ary Haskell `on`. E.g.
;; (on guide smr) reads each side of a guide through a summary.
(define ((on op f) . args) (apply op (map f args)))

;; arg: ((arg i j ...) . xs) returns the i-th, j-th, ... arguments as multiple values.
(define ((arg . is) . xs)
  (let ([v (list->vector (take xs (add1 (apply max is))))])
    (apply values (map (lambda (i) (vector-ref v i)) is))))

;; pass: hold a tuple of args, then apply each function to them, as values --
;; ((pass . args) f g ...) = (values (apply f args) (apply g args) ...).
(define ((pass . args) . fs)
  (apply values (map (lambda (f) (apply f args)) fs)))

;; fork: the function-first twin of `pass` -- hold the functions, then apply each to the same
;; args, as values: ((fork f g ...) . args) = (values (apply f args) (apply g args) ...). A fanout;
;; ((fork f g) x) = (values (f x) (g x)), e.g. (fork read values) reads and passes its arg through.
(define ((fork . fs) . args)
  (apply values (map (lambda (f) (apply f args)) fs)))

;; spread: each function to its corresponding argument, results combined by `h` --
;; ((spread h f g ...) a b ...) = (h (f a) (g b) ...). case-lambda inlines 1..4 fns
;; positionally; 5+ falls to a map/apply tail. Used as (spread combine values coerce).
(define spread
  (case-lambda
    [(h f)       (lambda (a)       (h (f a)))]
    [(h f g)     (lambda (a b)     (h (f a) (g b)))]
    [(h f g k)   (lambda (a b c)   (h (f a) (g b) (k c)))]
    [(h f g k l) (lambda (a b c d) (h (f a) (g b) (k c) (l d)))]
    [(h . fs)    (lambda xs (apply h (map (lambda (f x) (f x)) fs xs)))]))

;; variadic: lift a binary `op` (acc-first, (op acc x)) + seed `id` to any arity, left-
;; folding from id. The inlined 0/1/2-ary cases fold `id` in too, so they match foldl
;; for ANY op/id (no identity-law assumption).
(define (variadic op id)
  (case-lambda
    [(a b) (op (op id a) b)]
    [(a)   (op id a)]
    [()    id]
    [xs    (foldl (lambda (x acc) (op acc x)) id xs)]))

;; fixed: iterate `improve` from a seed to a fixed point; seed and improve may carry
;; several values. Halt is `same?` over `(key v ...)` applied to the tuple as args
;; (default key=list -> whole-tuple equal?), mirroring remove-duplicates' [same?] #:key.
;; An improve that no-ops at the fixpoint needs no stop test. Arities 1..4 inlined; 5+
;; falls to `rest-loop`. Inlining rationale: scribble.
(define (fixed improve [same? equal?] [key list])
  ;; macro so its template captures improve/same?/key from this scope
  (define-syntax (fixed-case stx)
    (syntax-case stx ()
      [(_ v ...)
       (with-syntax ([(v* ...) (generate-temporaries #'(v ...))])
         #'(let loop ([v v] ... [kp (key v ...)])
             (define-values (v* ...) (improve v ...))
             (define kp* (key v* ...))
             (if (same? kp kp*) (values v* ...) (loop v* ... kp*))))]))
  ;; the generic tail: tuple held as a list, for any arity past the inlined ones
  (define (rest-loop xs)
    (let ([step (compose list improve)])
      (let loop ([xs xs] [kp (apply key xs)])
        (define ys (apply step xs))
        (define kp* (apply key ys))
        (if (same? kp kp*) (apply values ys) (loop ys kp*)))))
  (case-lambda
    [(a)       (fixed-case a)]
    [(a b)     (fixed-case a b)]
    [(a b c)   (fixed-case a b c)]
    [(a b c d) (fixed-case a b c d)]
    [xs        (rest-loop xs)]))

;; lexicographic: lift an element comparison `cmp` (-> {-1,0,1}) to a 3-way order on
;; sequences -- first non-zero verdict decides; a prefix precedes its extension.
(define ((lexicographic cmp) xs ys)
  (let loop ([xs xs] [ys ys])
    (cond [(null? xs) (if (null? ys) 0 -1)]
          [(null? ys) 1]
          [else (let ([v (cmp (car xs) (car ys))])
                  (if (zero? v) (loop (cdr xs) (cdr ys)) v))])))

;; ============================================================================
;; WIP -- not yet load-bearing; surface and semantics may still move.
;; ============================================================================

(provide lockstep             ; (lockstep f g ...): N equivalent fns worn as one self-checking procedure
         lockstep?            ; recognizes one
         lockstep-on          ; (lockstep-on x): checking-on sibling; non-locksteps pass through
         lockstep-off         ; (lockstep-off x): run only the trusted impl, raw; non-locksteps pass through
         lockstep-mode)       ; 'on | 'off

;; lockstep: bundle N functions that should compute the same thing, worn as one procedure.
;; Born ON: every call runs all impls and checks they agree before returning the common
;; result. Each impl's return is captured as a value tuple (so multiple-values impls work),
;; and agreement is checked per value-position: a value column must be equal? across impls;
;; a column where every impl returns a procedure isn't comparable yet, so its check rides
;; down to the next application (a re-bundled lockstep in that slot); a procedure-vs-value
;; split in a column, or differing tuple arities, is a disagreement. `lockstep-off` flips an
;; instance to OFF: run only the trusted impl, raw -- one run, results unwrapped (a returned
;; procedure comes back plain, no deferral). Trusted defaults to the LAST impl (we list the
;; ordinary form first, the one to run when off last); `#:trusted i` overrides. Sound only
;; for pure, deterministic fns -- N runs per call. The struct `steps` is private.
(struct steps (fs mode trusted)
  #:property prop:procedure
  (lambda (self . args)
    (case (steps-mode self)
      [(off) (apply (list-ref (steps-fs self) (steps-trusted self)) args)]   ; one run, raw
      [else
       (define rss (map (lambda (f) (call-with-values (lambda () (apply f args)) list))
                        (steps-fs self)))             ; one value tuple per impl
       (define n (length (car rss)))
       (unless (andmap (lambda (vs) (= (length vs) n)) rss)
         (error 'lockstep "arity mismatch: ~e" rss))
       (apply values
        (for/list ([j (in-range n)])                  ; resolve column by column
          (define col (map (lambda (vs) (list-ref vs j)) rss))
          (cond
            [(andmap procedure? col) (steps col 'on (steps-trusted self))]   ; defer to next apply
            [(ormap procedure? col) (error 'lockstep "shape mismatch at value ~a: ~e" j col)]
            [else
             (for ([r (in-list (cdr col))] [i (in-naturals 1)])
               (unless (equal? r (car col))
                 (error 'lockstep "impl ~a fell out of step: ~e vs ~e" i r (car col))))
             (car col)])))])))

(define (lockstep #:trusted [t #f] . fs) (steps fs 'on (or t (sub1 (length fs)))))
(define lockstep? steps?)
(define lockstep-mode steps-mode)

;; lockstep-on / -off are universal: flip a lockstep's mode, but pass any other value
;; through untouched -- so arbitrary functions can be wrapped and simply no-op. This also
;; lets you silence one deferred sub-stage: in ON mode each stage is itself a lockstep.
(define (lockstep-on  x) (if (steps? x) (steps (steps-fs x) 'on  (steps-trusted x)) x))
(define (lockstep-off x) (if (steps? x) (steps (steps-fs x) 'off (steps-trusted x)) x))

;; ============================================================================
(module+ test
  (require rackunit)

  (define inc (iso add1 sub1))

  ;; --- applying an iso runs its forward side; `inverse` runs the other ---
  (check-equal? (inc 10) 11)
  (check-equal? ((inverse inc) 11) 10)

  ;; --- integer powers, closed on isos ---
  (check-equal? ((expt-iso inc 3) 10) 13)         ; forward thrice
  (check-equal? ((expt-iso inc -3) 13) 10)        ; negative = inverse's power
  (check-equal? ((expt-iso inc 0) 99) 99)         ; n = 0 is the identity iso

  ;; --- the result is still an iso: invert it, re-exponentiate it ---
  (check-equal? ((inverse (expt-iso inc 3)) 13) 10)

  ;; --- compose-iso is variadic: any number of isos, inverses reversed ---
  (check-equal? ((compose-iso inc inc inc) 10) 13)          ; three composed, forward
  (check-equal? ((inverse (compose-iso inc inc inc)) 13) 10)
  (check-equal? ((compose-iso) 42) 42)                      ; no isos = the identity iso

  ;; --- the identities that closure buys ---
  (define i (iso (lambda (x) (* 2 x)) (lambda (x) (/ x 2))))
  ;; inverse and power commute
  (check-equal? ((inverse (expt-iso i 4)) 48)
                ((expt-iso i -4) 48))
  ;; expt -1 = inverse  (the generic-arithmetic identity, inside the type)
  (check-equal? ((expt-iso i -1) 6) ((inverse i) 6))
  ;; (i^m)^n = i^(m*n)
  (check-equal? ((expt-iso (expt-iso i 2) 3) 5)
                ((expt-iso i 6) 5))

  ;; --- the iso law: a genuine iso round-trips, a non-iso is caught ---
  (check-true  (iso-law? inc 10))
  (check-true  (iso-law? (expt-iso inc 3) 10))
  (check-equal? (check-iso-laws inc '(0 5 -3 99)) '())
  (define bad (iso add1 add1))             ; from doesn't undo to
  (check-false (iso-law? bad 10))
  (check-equal? (check-iso-laws bad '(1 2 3)) '(1 2 3))

  ;; --- pure: the constant fn -- ignores its args, returns the v ... as values ---
  (check-equal? ((pure 5) 'a 'b) 5)                                            ; any args ignored
  (check-equal? (call-with-values (lambda () ((pure 1 2 3) 'x)) list) '(1 2 3)) ; variadic -> values
  (check-equal? (call-with-values (lambda () ((pure))) list) '())              ; no values

  ;; --- on: every argument projected through f, then op (any arity) ---
  (check-equal? ((on + abs) -3 4) 7)               ; abs each, then +
  (check-equal? ((on + abs) -1 2 -3) 6)            ; n-ary, not just binary
  (check-equal? ((on cons add1) 1 2) '(2 . 3))

  ;; --- pass: hold the args, apply several functions to them, as values ---
  (check-equal? ((pass 5) add1) 6)                 ; one function, one value
  (check-equal? (call-with-values
                 (lambda () ((pass 3 4) + * -)) list)
                '(7 12 -1))                         ; each f applied to (3 4), as values

  ;; --- fork: the function-first twin -- each fn to the same arg(s), as values ---
  (check-equal? ((fork add1) 5) 6)                 ; one function, one arg
  (check-equal? (call-with-values
                 (lambda () ((fork + * -) 3 4)) list)
                '(7 12 -1))                         ; each f applied to (3 4)
  (check-equal? (call-with-values
                 (lambda () ((fork add1 values) 5)) list)
                '(6 5))                             ; fanout: read + pass-through (values = identity)

  ;; --- spread: spread-combine -- each function to its own argument, results combined
  ;;     by h.  (spread h f g) a b = (h (f a) (g b)).  Small arities inlined, 5+ tail. ---
  (check-equal? ((spread list add1 sub1) 10 20) '(11 19))               ; (list (add1 10) (sub1 20))
  (check-equal? ((spread + values string-length) 10 "abc") 13)          ; the make-summary shape: (+ 10 3)
  (check-equal? ((spread list add1 sub1 -) 1 2 3) '(2 1 -3))            ; arity 3 (macro case)
  (check-equal? ((spread list add1 sub1 - add1) 1 2 3 4) '(2 1 -3 5))   ; arity 4 (macro case)
  (check-equal? ((spread list add1 sub1 - add1 sub1) 1 2 3 4 5) '(2 1 -3 5 4)) ; arity 5 (tail)
  ;; folds coerced args inside variadic, the make-summary shape:
  (check-equal? ((variadic (spread + values string-length) 0) "ab" "cde") 5)   ; (+ (+ 0 2) 3)

  ;; --- variadic: a binary op + seed lifted to any arity; id folded in every case,
  ;;     so the inlined small-arity paths match a plain left fold for ANY op/id ---
  (check-equal? ((variadic + 0))         0)         ; nullary = the seed
  (check-equal? ((variadic + 0) 5)       5)         ; (+ 0 5)
  (check-equal? ((variadic + 0) 1 2)     3)         ; (+ (+ 0 1) 2)
  (check-equal? ((variadic + 0) 1 2 3 4) 10)
  ;; non-monoidal op: seed and order matter, the inline paths still agree with foldl
  (check-equal? ((variadic - 0) 5 3) (foldl (lambda (x acc) (- acc x)) 0 '(5 3)))
  (check-equal? ((variadic cons '()) 1 2 3) '(((() . 1) . 2) . 3))

  ;; --- fixed: single value, multiple values, and a key projection ---
  (check-equal? ((fixed (lambda (n) (quotient n 2))) 100) 0)        ; halve to the fixpoint 0
  (check-equal? (call-with-values                                   ; multi-value: (a b) -> (b min)
                 (lambda () ((fixed (lambda (a b) (values b (min a b)))) 5 3)) list)
                '(3 3))
  ;; stop when a derived quantity settles -- here the tens digit -- via key:
  (check-equal? ((fixed sub1 = (lambda (n) (quotient n 10))) 25) 24)
  ;; a 3-value tuple exercises a macro-built clause; 5 values fall to the list tail:
  (check-equal? (call-with-values
                 (lambda () ((fixed (lambda (a b c) (values b c (min a b c)))) 9 5 7)) list)
                '(5 5 5))
  (check-equal? (call-with-values
                 (lambda () ((fixed (lambda (a b c d e) (values b c d e (min a b c d e)))) 5 4 3 2 1)) list)
                '(1 1 1 1 1))

  ;; --- lens: a peek wrapped by make-lens (put FIRST), the three ops, composition, the laws ---
  (define fst-lens                          ; a lens onto a list's head (one focus)
    (make-lens (lambda (xs) (values (lambda (x) (cons x (cdr xs))) (first xs)))))
  (check-equal? ((viewer fst-lens) '(1 2 3)) 1)
  (check-equal? ((viewer fst-lens add1) '(1 2 3)) 2)                               ; optional k folds the view: (add1 1)
  (check-equal? ((setter fst-lens 9) '(1 2 3)) '(9 2 3))
  (check-equal? ((updater fst-lens add1) '(1 2 3)) '(2 2 3))
  (check-equal? ((setter fst-lens ((viewer fst-lens) '(1 2))) '(1 2)) '(1 2))      ; get-put
  (check-equal? ((viewer fst-lens) ((setter fst-lens 9) '(1 2))) 9)                ; put-get
  (check-equal? ((setter fst-lens 8) ((setter fst-lens 9) '(1 2)))                  ; put-put
                ((setter fst-lens 8) '(1 2)))
  ;; composition is Racket's compose -- van Laarhoven threads the put-backs: onto first-of-first
  (define fst-fst (compose fst-lens fst-lens))
  (check-equal? ((viewer fst-fst) '((1 2) 3)) 1)
  (check-equal? ((setter fst-fst 9) '((1 2) 3)) '((9 2) 3))
  (check-equal? ((viewer (compose)) 42) 42)                                        ; empty = identity
  (check-equal? ((setter (compose) 9) 42) 9)

  ;; --- list-of (one list focus), lref (fan-out to N foci), varg (rearrange by position) ---
  (define (vlist l . s) (call-with-values (lambda () (apply (viewer l) s)) list))  ; collect viewer's values
  (define li (list-of (make-lens (lambda (p) (values (lambda (x) (cons x (cdr p))) (car p))))))  ; car-lens over a list
  (define gl (list (cons 1 'g) (cons 2 'g) (cons 3 'g)))
  (check-equal? (vlist li gl) (list '(1 2 3)))                      ; list-of is ONE focus: the list
  (check-equal? ((setter li (list 10 20 30)) gl)
                (list (cons 10 'g) (cons 20 'g) (cons 30 'g)))
  (define L (compose li (lref 0 2)))
  (check-equal? (vlist L gl) '(1 3))                               ; lref fans to N foci
  (check-equal? ((setter L 'X 'Y) gl) (list (cons 'X 'g) (cons 2 'g) (cons 'Y 'g)))
  (check-equal? ((updater L add1 add1) gl) (list (cons 2 'g) (cons 2 'g) (cons 4 'g)))
  (check-equal? (length ((setter L 'X) gl)) 3)                     ; under-supplied: length preserved
  (check-equal? (vlist L ((setter L 'X 'Y) gl)) '(X Y))            ; put-get
  (check-equal? (vlist (compose li (lref 0 1 2) (varg 2 0)) gl) '(3 1))            ; varg reorders
  (check-equal? (vlist (compose li (lref 0 1 2) (varg 2 0)) gl)
                (call-with-values (lambda () ((arg 2 0) 1 2 3)) list))            ; viewer of varg = arg

  ;; ldiag: view position i (the bias), the put broadcasts one value to every slot
  (check-equal? ((viewer (ldiag 0)) '(a b c)) 'a)
  (check-equal? ((viewer (ldiag 1)) '(a b c)) 'b)
  (check-equal? ((setter (ldiag 0) 'X) '(a b c)) '(X X X))
  (check-equal? ((updater (ldiag 1) symbol->string) '(a b c)) '("b" "b" "b"))

  ;; vdiag: ldiag on the value stream -- view value i, the put broadcasts to every position
  (check-equal? ((viewer (vdiag 0)) 'a 'b 'c) 'a)
  (check-equal? ((viewer (vdiag 1)) 'a 'b 'c) 'b)
  (check-equal? (call-with-values (lambda () ((setter (vdiag 0) 'X) 'a 'b 'c)) list) '(X X X))
  (check-equal? (call-with-values (lambda () ((updater (vdiag 1) symbol->string) 'a 'b 'c)) list)
                '("b" "b" "b"))

  ;; --- WIP: lockstep -- equivalent twins agree, a bad twin and a shape split are caught ---
  (define sos (lockstep (lambda (xs) (apply + (map (lambda (x) (* x x)) xs)))
                        (lambda (xs) (foldl (lambda (x a) (+ a (* x x))) 0 xs))))
  (check-equal? (sos '(1 2 3 4)) 30)                          ; both branches run and agree
  (check-true (lockstep? sos))
  (check-exn #rx"fell out of step"                            ; a buggy refactor is caught
             (lambda () ((lockstep (lambda (n) (* n n)) (lambda (n) (* n 2))) 3)))
  ;; higher-order: the check defers down the currying to the comparable leaf
  (define adder (lockstep (lambda (a) (lambda (b) (+ a b)))
                          (lambda (a) (lambda (b) (- b (- a))))))
  (check-true (lockstep? (adder 10)))                         ; (adder 10) is itself a lockstep
  (check-equal? ((adder 10) 5) 15)
  (check-exn #rx"shape mismatch"                              ; fn vs value at the same stage
             (lambda () ((lockstep (lambda (a) (lambda (b) (+ a b)))
                                   (lambda (a) (+ a 100))) 2)))
  ;; multiple values: agreement checked per value-position
  (define mv (lockstep (lambda (a b) (values (+ a b) (* a b)))
                       (lambda (a b) (values (+ b a) (* b a)))))   ; commuted twin
  (check-equal? (call-with-values (lambda () (mv 3 4)) list) '(7 12))
  (check-exn #rx"fell out of step"                            ; one value column disagrees
             (lambda () ((lockstep (lambda (a b) (values a b))
                                   (lambda (a b) (values a (add1 b)))) 1 2)))
  (check-exn #rx"arity mismatch"                              ; differing tuple lengths
             (lambda () ((lockstep (lambda (x) (values x x))
                                   (lambda (x) x)) 5)))
  ;; on/off: born on; off runs only the trusted impl (the LAST by default), raw
  (define ordinary (lambda (xs) (apply + (map (lambda (x) (* x x)) xs))))
  (define tuned    (lambda (xs) (foldl (lambda (x a) (+ a (* x x))) 0 xs)))   ; trusted (last)
  (define ls (lockstep ordinary tuned))
  (check-eq? (lockstep-mode ls) 'on)                          ; born on
  (check-equal? (ls '(1 2 3)) 14)                             ; both run, agree
  (define fast (lockstep-off ls))
  (check-eq? (lockstep-mode fast) 'off)
  (check-equal? (fast '(1 2 3)) 14)                           ; runs tuned only, raw
  (check-eq? (lockstep-mode (lockstep-on fast)) 'on)          ; round-trips back
  ;; off short-circuits: a disagreeing impl is never run, so no error
  (check-equal? ((lockstep-off (lockstep (lambda (n) 'wrong) (lambda (n) (* n n)))) 3) 9)
  ;; #:trusted overrides which impl off runs
  (check-equal? ((lockstep-off (lockstep #:trusted 0 add1 sub1)) 10) 11)
  ;; on/off are no-ops on non-locksteps
  (check-eq? (lockstep-off ordinary) ordinary)
  (check-equal? (lockstep-on 42) 42)
  ;; off returns a raw function for HOFs -- no deferral, not a lockstep
  (define curr (lockstep (lambda (a) (lambda (b) (+ a b)))
                         (lambda (a) (lambda (b) (- b (- a))))))
  (check-true  (lockstep? (curr 10)))                         ; on: each stage is a lockstep
  (check-false (lockstep? ((lockstep-off curr) 10)))          ; off: plain closure
  (check-equal? (((lockstep-off curr) 10) 5) 15))
