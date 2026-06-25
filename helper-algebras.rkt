#lang racket

;; Small algebraic helpers, each self-contained and documented at its definition:
;;   iso           a focused (to, from) pair -- a reversible function (detailed below)
;;   lens          van Laarhoven optic (from a peek s -> (values focus put) via `make-lens`); ops
;;                 viewer/setter/updater (curried); composes with plain `compose`.  `vref`: a vector-slot lens
;;   on            (on op f) a b ... = (op (f a) (f b) ...)
;;   arg           ((arg i ...) . xs): project args by 0-based position (the K combinator)
;;   pass          ((pass . args) . fs): apply each f to the fixed args, as values (the thrush / fork)
;;   spread        ((spread h f ...) a ...) = (h (f a) ...): each fn to its arg, then combine (spread-combine)
;;   variadic      lift a binary op + seed to a variadic left fold (op called acc-first)
;;   fixed         iterate an `improve` step to a fixed point
;;   lexicographic lift an element comparison to a 3-way order on sequences
;;
;; The iso is the one with structure worth spelling out here.  An ISO is a focused
;; pair (to, from): applying it runs the focused side, `inverse` toggles the focus,
;; and `expt-iso` raises it to an integer power WITHOUT leaving the type -- the
;; result is still an iso, so it can be inverted or re-exponentiated in turn.
;;
;; The point of closure: isos compose as a group (the identity iso the unit,
;; `inverse` the inverse), so `expt-iso` gets the whole of Z for free -- negatives are the
;; positive powers of the inverse, and (expt-iso i -1) = (inverse i).  This is the
;; "expt -1 = inverse" of generic arithmetic (scmutils' (expt M -1), Lean's
;; a ^ (-1 : Z) = a-inverse), landing INSIDE the iso type rather than handing back
;; a bare function.

(provide (struct-out iso)        ; (iso to from); callable = applies `to`
         compose-iso             ; compose any number of isos; inverses reversed: (g.f)-1 = f-1.g-1
         expt-iso                ; iso x Z -> iso, closed on isos
         iso-law?                ; (iso-law? i x): does x round-trip through i?
         check-iso-laws          ; (check-iso-laws i xs): the inputs that don't
         make-lens               ; (make-lens peek): a coalgebra peek -> a variadic van Laarhoven lens
         viewer setter updater   ; the lens ops, curried -- viewer a getter (foci as values), setter/updater commands
         list-of                 ; (list-of el): map a single-focus element lens over a list -- ONE focus (the list of views)
         lref                    ; (lref i ...): index a list, fanning to N foci; length-safe
         ldiag                   ; (ldiag i): the list diagonal -- view position i (the bias), put broadcasts to all
         varg                    ; (varg i ...): rearrange the value stream by position -- the lens twin of `arg`
         on                      ; (on op f): op on its args, each projected through f
         arg                     ; ((arg i ...) . xs): selected args as values (0-based projection / K)
         pass                    ; ((pass . args) . fs): each f applied to the fixed args, as values (thrush / fork)
         spread                  ; (spread h f ...): apply each fn to its own arg, combine with h (spread-combine)
         variadic                ; (variadic op id): lift a binary op + seed to a variadic left fold
         fixed                   ; (fixed improve [same? equal?] [key list]): iterate to a fixed point
         lexicographic)          ; ((lexicographic cmp) l1 l2): first-difference 3-way order

;; an iso is a focused pair; `prop:procedure` runs the focused (forward) side, so
;; an iso IS a function when called -- only its own combinators see the extra half.
(struct iso (to from)
  #:property prop:procedure (struct-field-index to))

(define (inverse i) (iso (iso-from i) (iso-to i)))

;; compose any number of isos; the inverse of a composite runs the halves in
;; reverse order.  (compose-iso) with no isos is the identity iso.
(define (compose-iso . is)
  (iso (apply compose (map iso-to is))
       (apply compose (map iso-from (reverse is)))))

;; raise to an integer power, staying an iso; negatives go through the inverse.
(define (expt-iso i n)
  (cond [(negative? n) (expt-iso (inverse i) (- n))]
        [else (for/fold ([acc (iso values values)]) ([_ (in-range n)]) (compose-iso i acc))]))

;; the iso law (over equal?): i composed with its inverse is the identity --
;; running a value through `to` then back through `from` returns it unchanged.
(define (iso-law? i x) (equal? ((compose-iso (inverse i) i) x) x))

;; sweep a corpus through the law; the result is the inputs that DON'T round-trip
;; ('() means i is a genuine iso over every one of them).
(define (check-iso-laws i xs) (filter (lambda (x) (not (iso-law? i x))) xs))

;; A LENS is a VARIADIC VAN LAARHOVEN optic over the VALUE STREAM.  It focuses one OR MORE values
;; inside a structure that is itself one or more values, built from a store coalgebra:
;;   peek : structvals ... -> (values put focus ...)   ; the put-back FIRST, then the foci as VALUES
;;   put  : newfocus ...   -> structvals ...           ; rebuild the structure from new foci
;; Composition is plain `compose` -- function composition IS lens composition -- and now carries N
;; values per hop: an outer lens's foci become the inner lens's structvals, so the arities must line
;; up at each seam.  (put comes FIRST because, with variadic foci, that is the only fixed position;
;; and `(compose f g)` already does the multi-value threading `call-with-values` would.)
;;
;; One body serves view AND set; the op picks the functor f.  view = Const (carries the foci out,
;; runs no put); set/over = Identity (raw values, the put runs).  Const alone departs from the
;; default `(apply put ...)`, so it carries the private tag `const-box` (holding the foci as a list);
;; Identity stays bare values.  The ops curry: viewer a getter (-> the foci as MULTIPLE VALUES; one
;; focus -> one value), setter takes one value per focus, updater one function per focus.
(struct const-box (vs))                                        ; the view tag; private; foci as a list
(define ((make-lens peek) k)                                   ; a coalgebra -> a variadic vL lens
  (compose                                                     ; (compose handle peek): peek, then handle
   (lambda (put . foci)
     (let ([r (apply (compose list k) foci)])                  ; run k on the foci, collect its values
       (if (and (pair? r) (null? (cdr r)) (const-box? (car r)))
           (car r)                                             ; view     -> lone const-box: skip the put
           (apply put r))))                                    ; set/over -> rebuild from the new foci
   peek))
(define ((viewer  l)      . s) (apply values (const-box-vs (apply (l (lambda foci (const-box foci))) s))))
(define ((setter  l . xs) . s) (apply (l (lambda _    (apply values xs))) s))
(define ((updater l . fs) . s) (apply (l (lambda foci (apply values (map (lambda (f x) (f x)) fs foci)))) s))

;; `list-of`: a single-focus element LENS lifted over a list -- a SINGLE focus, the list of element
;; views; the put rebuilds element-wise via the element lens's setter (a partial element update is kept).
;; Keeps the chain single-value; `lref` below is where it fans out.
(define (list-of el)
  (make-lens (lambda (xs)
    (values (lambda (ys) (map (lambda (x y) ((setter el y) x)) xs ys))
            (map (viewer el) xs)))))

;; `lref`: index a list at positions `is`, fanning the focus into N values (the picked elements);
;; the put writes them back into a copy.  Length-safe -- it overwrites slots, never reshapes.
(define (lref . is)
  (make-lens (lambda (xs)
    (define v (list->vector xs))
    (apply values
           (lambda nf (define w (vector-copy v))
                      (for ([i (in-list is)] [x (in-list nf)]) (vector-set! w i x))
                      (vector->list w))
           (map (lambda (i) (vector-ref v i)) is)))))

;; `ldiag`: the diagonal of a list -- view position `i` (the bias); the put broadcasts one value to
;; every slot.  The gap-collapsing twin of `(lref i)` (lawful only when the slots are already equal).
(define (ldiag i)
  (make-lens (lambda (xs) (values (lambda (x) (make-list (length xs) x)) (list-ref xs i)))))

;; `varg`: the lens twin of `arg` -- focus the values at positions `is`, in that order; the put
;; writes them back.  ((viewer (varg . is)) ...) = ((arg . is) ...).  Lawful for distinct positions
;; (a selection / permutation); a repeated position is a (lossy) diagonal -- put-get fails.
(define (varg . is)
  (make-lens (lambda structvals
    (define v (list->vector structvals))
    (apply values
           (lambda nf (define w (vector-copy v))
                      (for ([i (in-list is)] [x (in-list nf)]) (vector-set! w i x))
                      (apply values (vector->list w)))
           (map (lambda (i) (vector-ref v i)) is)))))

;; `on`: apply op to all its arguments, each projected through f --
;; (on op f) a b ... = (op (f a) (f b) ...).  The n-ary generalization of Haskell's
;; (binary) Data.Function.on.  With a summary as f it reads each side through that
;; summary -- e.g. wrapping a guide for a bundle: (on guide smr).
(define ((on op f) . args) (apply op (map f args)))

;; `arg`: project arguments by 0-based position -- ((arg i j ...) . xs) returns the
;; i-th, j-th, ... arguments as multiple values.  The generalized projection (the K
;; combinator): (arg 0) selects the first argument -- Haskell's `const` for two args.
;; One pass: vectorize xs only up to the furthest index, then emit in `is` order.
(define ((arg . is) . xs)
  (let ([v (list->vector (take xs (add1 (apply max is))))])
    (apply values (map (lambda (i) (vector-ref v i)) is))))

;; `pass`: hold a tuple of arguments, then apply each function to them, returning
;; the results as multiple values -- ((pass . args) f g ...) = (values (apply f
;; args) (apply g args) ...).  The thrush ((pass x) f) = (f x), flipped to fix the
;; argument and await the function, generalized to a FORK over several functions
;; (Clojure's juxt, as values not a list).  One function gives one value, so it
;; threads straight through `map`.
(define ((pass . args) . fs)
  (apply values (map (lambda (f) (apply f args)) fs)))

;; `spread`: spread-combine -- apply each function to its CORRESPONDING argument, then
;; combine the results with `h` -- ((spread h f g ...) a b ...) = (h (f a) (g b) ...).
;; SDF's spread-combine; the arity-split (`***` / bimap) followed by a combine, and the
;; transpose-dual of `pass` (which forks functions over ONE fixed arg-tuple).  Preprocess
;; a reducer's arguments with it: (variadic (spread combine values coerce) id) folds
;; coerced args -- `values` passes the accumulator through, `coerce` maps the element.
;; A case-lambda dispatches on the function count (no list walk): 1..4 functions get an
;; inlined positional lambda -- no rest-arg list, no map -- so the common arities run at
;; hand-wrapper speed; 5+ falls to a map/apply tail.
(define spread
  (case-lambda
    [(h f)       (lambda (a)       (h (f a)))]
    [(h f g)     (lambda (a b)     (h (f a) (g b)))]
    [(h f g k)   (lambda (a b c)   (h (f a) (g b) (k c)))]
    [(h f g k l) (lambda (a b c d) (h (f a) (g b) (k c) (l d)))]
    [(h . fs)    (lambda xs (apply h (map (lambda (f x) (f x)) fs xs)))]))

;; `variadic`: lift a binary `op` (called accumulator-first, (op acc x)) and a seed
;; `id` to a function of any arity that left-folds its arguments from id --
;; (variadic op id) a b = (op (op id a) b), (variadic op id) = id.  The 0/1/2-ary
;; cases (nearly every call) are inlined: they skip the rest-arg list and the foldl
;; but emit exactly the ops the fold would, so the value is identical for any op/id
;; (id is folded in even in the base cases -- no identity-law assumption).
(define (variadic op id)
  (case-lambda
    [(a b) (op (op id a) b)]
    [(a)   (op id a)]
    [()    id]
    [xs    (foldl (lambda (x acc) (op acc x)) id xs)]))

;; `fixed`: iterate `improve` from a seed to a fixed point, returning the seeker.
;; The seed and `improve` may carry MULTIPLE values; the halt test is an equality
;; over a projection, mirroring `remove-duplicates`'s `[same? equal?] #:key` (here
;; positional): stop when the projected state stops changing.  `key` is applied to
;; the value-tuple AS ARGUMENTS -- default `list` rebuilds the tuple, giving
;; whole-tuple `equal?`, a true fixed point; pick a selector (`(arg 0)`) or a derived
;; quantity to settle on that instead, with an equality (`eq?`, `=`) suited to the
;; projected value.  A step that no-ops when it can make no progress is the natural
;; halt, so such an `improve` needs no separate stop test.
;;
;; The small arities (1..4 -- navigate's ascend/descend are 2-value) are inlined: the
;; internal macro `fixed-case` builds, per clause, a loop on named variables with no
;; per-step list/apply/compose, carrying the running key forward (one key call per step,
;; not two).  Any larger arity falls to `rest-loop`, which reifies the tuple as a list --
;; `(compose list improve)` threads the values straight back as the next call's arguments.
;; `fixed-case` expands at compile time (each clause is straight-line loop code at runtime);
;; it lives inside `fixed` so its template captures improve/same?/key from this scope.
(define (fixed improve [same? equal?] [key list])
  ;; per-clause loop generator -- hand it the clause's vars, get that arity's no-list loop.
  (define-syntax (fixed-case stx)
    (syntax-case stx ()
      [(_ v ...)
       (with-syntax ([(v* ...) (generate-temporaries #'(v ...))])
         #'(let loop ([v v] ... [kp (key v ...)])
             (define-values (v* ...) (improve v ...))
             (define kp* (key v* ...))
             (if (same? kp kp*) (values v* ...) (loop v* ... kp*))))]))
  ;; the generic tail -- the tuple held as a list, for any arity past the inlined ones.
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

;; `lexicographic`: lift an element comparison to a 3-way order on sequences.
;; Walk two lists in parallel; the first non-zero elementwise verdict (`cmp` ->
;; {-1,0,1}) decides.  If they agree up to the shorter, the shorter is the lesser
;; -- a prefix precedes its extension.  ((lexicographic cmp) l1 l2) -> {-1,0,1}.
(define ((lexicographic cmp) xs ys)
  (let loop ([xs xs] [ys ys])
    (cond [(null? xs) (if (null? ys) 0 -1)]
          [(null? ys) 1]
          [else (let ([v (cmp (car xs) (car ys))])
                  (if (zero? v) (loop (cdr xs) (cdr ys)) v))])))

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

  ;; --- on: every argument projected through f, then op (any arity) ---
  (check-equal? ((on + abs) -3 4) 7)               ; abs each, then +
  (check-equal? ((on + abs) -1 2 -3) 6)            ; n-ary, not just binary
  (check-equal? ((on cons add1) 1 2) '(2 . 3))

  ;; --- pass: the thrush holds the args; several functions fork, as values ---
  (check-equal? ((pass 5) add1) 6)                 ; one function = the thrush, one value
  (check-equal? (call-with-values
                 (lambda () ((pass 3 4) + * -)) list)
                '(7 12 -1))                         ; each f applied to (3 4), as values

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
  (check-equal? ((updater (ldiag 1) symbol->string) '(a b c)) '("b" "b" "b")))
