#lang racket

;; Summarised rope, variadic-polymorphic rewrite.
;;
;; The whole library is organised around three variadic "coerce-and-fold"
;; functions, one per carrier:
;;
;;   summary : (string | rope | summary)* -> summary    ; build/read summaries
;;   rope    : (string | rope)*           -> rope        ; build ropes
;;   rope->string                                        ; read a rope's text
;;
;; A summary algebra is just a function (built by `summary-algebra`) that doubles
;; as the system handle `sys` threaded into rope construction. Every rope node is
;; tagged with the algebra it was built under, so `summary` can verify (by eq?)
;; that a rope's cached summary belongs to the algebra now folding with it.
;;
;; Design notes: discussions/2026-05-29/2-claude.md.

(require racket/match)

(provide
 summary-algebra
 rope
 rope->string
 split
 split-rope
 seg-split)

;; ---------- nodes ----------
;; Each node caches its summary AND its algebra (the `summary` fn it was built
;; under). `rope-summary` / `rope-algebra` read those uniformly.

(struct leaf       (text summary algebra)            #:transparent)
(struct leaf-range (text start end summary algebra)  #:transparent)
(struct branch     (left right summary algebra)      #:transparent)

(define (rope? v) (or (leaf? v) (leaf-range? v) (branch? v)))

(define (rope-summary r)
  (match r
    [(leaf _ s _)           s]
    [(leaf-range _ _ _ s _) s]
    [(branch _ _ s _)       s]))

(define (rope-algebra r)
  (match r
    [(leaf _ _ a)           a]
    [(leaf-range _ _ _ _ a) a]
    [(branch _ _ _ a)       a]))

;; ---------- summary algebra ----------
;; (summary-algebra measure combine) -> the variadic `summary` fn / `sys`.
;;
;;   (summary str)         = (measure str)
;;   (summary a b c ...)   = combine, folded left-to-right (order matters; a
;;                           monoid is associative but not commutative)
;;
;; Arguments interleave: strings are measured, ropes contribute their cached
;; summary (guarded same-algebra), summaries pass through. Identity is
;; (summary "") -- no separate empty (relies on measure being a homomorphism).

(define (summary-algebra measure combine)
  (define (summary . parts)
    (define (->s x)
      (cond
        [(string? x) (measure x)]
        [(rope? x)
         (if (eq? (rope-algebra x) summary)
             (rope-summary x)
             (error 'summary
                    "rope was summarised under a different algebra; reconstruction unsupported"))]
        [else x]))
    (when (null? parts)
      (error 'summary "needs at least one argument"))
    (foldl (lambda (x acc) (combine acc (->s x)))
           (->s (car parts))
           (cdr parts)))
  summary)

;; ---------- leaf / piece helpers (internal) ----------

(define ((leaf-rope sys) text)
  (leaf text (sys text) sys))

(define (make-leaf-range sys text start end)
  (if (= start end)
      (empty-rope sys)
      (leaf-range text start end (sys (substring text start end)) sys)))

(define (empty-rope sys)
  (leaf "" (sys "") sys))

(define (empty-rope? r)
  (and (leaf? r) (string=? "" (leaf-text r))))

(define (piece-text piece)
  (match piece
    [(leaf text _ _)           text]
    [(leaf-range text s e _ _) (substring text s e)]))

(define (leaf-piece-bounds piece)
  (match piece
    [(leaf text _ _)           (values text 0 (string-length text))]
    [(leaf-range text s e _ _) (values text s e)]))

(define (leaf-piece-length piece)
  (define-values (_ s e) (leaf-piece-bounds piece))
  (- e s))

;; Termination guard for descent: a rope that `split` cannot make progress on
;; (a leaf of length <= 1). Branches are never atomic.
(define (atom? r)
  (and (not (branch? r)) (<= (leaf-piece-length r) 1)))

;; Bisect a non-atomic leaf/leaf-range into two ranges over the same backing
;; string (no copy). Algebra is recovered from the piece.
(define (split-leaf-piece piece)
  (define sys (rope-algebra piece))
  (define-values (text start end) (leaf-piece-bounds piece))
  (define mid (+ start (quotient (- end start) 2)))
  (values (make-leaf-range sys text start mid)
          (make-leaf-range sys text mid end)))

;; Two adjacent ranges of the same backing string re-fuse into one leaf.
(define (leaf-compatible? l r)
  (and (leaf-range? l) (leaf-range? r)
       (eq? (leaf-range-text l) (leaf-range-text r))
       (= (leaf-range-end l) (leaf-range-start r))))

;; ---------- branch / concat (internal) ----------

(define ((branch-rope sys) l r)
  (branch l r (sys l r) sys))

;; Smart joiner: drops empties, re-fuses adjacent compatible ranges, else
;; branches. This is the rope "rise"/join step.
(define ((concat-rope sys) . ropes)
  (foldr (lambda (l r)
           (cond
             [(empty-rope? l) r]
             [(empty-rope? r) l]
             [(leaf-compatible? l r)
              ((leaf-rope sys) (string-append (piece-text l) (piece-text r)))]
             [else ((branch-rope sys) l r)]))
         (empty-rope sys)
         ropes))

(define (chunk-string text n)
  (for/list ([start (in-range 0 (string-length text) n)])
    (substring text start (min (string-length text) (+ start n)))))

;; ---------- rope builder ----------
;; ((rope sys [#:chunk-size n]) . parts) assembles strings (chunked into leaves)
;; and ropes (passed through) by a dumb concat fold. Subsumes the old
;; string->rope (chunk + assemble) and concat-rope (all-ropes case). Balancing
;; is deferred -- the fold is a right-leaning spine for now.

(define ((rope sys #:chunk-size [chunk 1024]) . parts)
  (define (->rope x)
    (if (string? x)
        (apply (concat-rope sys) (map (leaf-rope sys) (chunk-string x chunk)))
        x))
  (apply (concat-rope sys) (map ->rope parts)))

;; ---------- read ----------

(define (rope->string r)
  (match r
    [(leaf text _ _)           text]
    [(leaf-range text s e _ _) (substring text s e)]
    [(branch l r _ _)          (string-append (rope->string l) (rope->string r))]))

;; ---------- split: one-level guided eliminator ----------
;; Precondition: (not (atom? mr)). Does exactly one structural level: derive the
;; two children with their contexts threaded, read the guide at the split point,
;; dispatch to one handler. No recursion of its own. `sys` is recovered from mr.
;;
;;   ((split guide on-l on-r on-here) before mr after)
;;     guide   : left-total-summary right-total-summary -> -1 | 0 | 1
;;     on-l    : before L after-of-L R-sibling  -> a   ; boundary in the left child
;;     on-r    : L-sibling before-of-R R after  -> a   ; boundary in the right child
;;     on-here : L R                            -> a   ; boundary between the children

(define ((split guide on-l on-r on-here) before mr after)
  (define sys (rope-algebra mr))
  (define-values (L R)
    (if (branch? mr)
        (values (branch-left mr) (branch-right mr))
        (split-leaf-piece mr)))
  (case (guide (sys before L) (sys R after))
    [(-1) (on-l before L (sys R after) R)]
    [(1)  (on-r L (sys before L) R after)]
    [(0)  (on-here L R)]
    [else (error 'split "guide must return -1, 0, or 1")]))

;; ---------- split-rope: partition at one boundary ----------
;; ((split-rope guide) before mr after) -> (values left right)
;; Recurses via `split`, reassembling the untouched sibling on the correct side.

(define ((split-rope guide) before mr after)
  (define sys (rope-algebra mr))
  (define (cat . rs) (apply (rope sys) rs))
  (let walk ([before before] [mr mr] [after after])
    (cond
      [(atom? mr)
       (if (positive? (guide before (sys mr after)))
           (values mr (empty-rope sys))
           (values (empty-rope sys) mr))]
      [else
       ((split guide
          (lambda (b L a R)
            (define-values (ll lr) (walk b L a))
            (values ll (cat lr R)))
          (lambda (L b R a)
            (define-values (rl rr) (walk b R a))
            (values (cat L rl) rr))
          (lambda (L R) (values L R)))
        before mr after)])))

;; ---------- seg-split: select a segment between two boundaries ----------
;; ((seg-split seg-guide) before mr after) -> (values left mid right)
;; Two split-rope passes: the -1 cut finds the left edge, the +1 cut the right.

(define ((seg-split seg-guide) before mr after)
  (define sys (rope-algebra mr))
  (define ((bound off) sl sr) (sgn (+ (seg-guide sl sr) off)))
  (define-values (l rest) ((split-rope (bound -1)) before mr after))
  (define-values (m r)    ((split-rope (bound 1)) (sys before l) rest after))
  (values l m r))

;; ============================================================================
(module+ test
  (require rackunit)

  ;; A trivial algebra: summary = character count.
  (define sum (summary-algebra string-length +))

  ;; --- build & read ---
  (define r ((rope sum) "abcdef"))
  (check-equal? (rope->string r) "abcdef")
  (check-equal? (sum r) 6)                         ; rope coerced -> cached summary

  ;; chunked build still round-trips and summarises
  (define r2 ((rope sum #:chunk-size 2) "hello world"))
  (check-equal? (rope->string r2) "hello world")
  (check-equal? (sum r2) 11)

  ;; --- interleaving strings / ropes / summaries ---
  (check-equal? (sum "ab" r "x") (+ 2 6 1))
  (check-equal? (sum 5 r)        (+ 5 6))          ; a summary value (number) passes through
  (check-equal? (sum "")         0)                ; identity = (summary "")

  ;; assembling mixed parts into a rope
  (define joined ((rope sum) "(" r ")"))
  (check-equal? (rope->string joined) "(abcdef)")
  (check-equal? (sum joined) 8)

  ;; --- same-algebra guard ---
  (define sum2 (summary-algebra string-length +))  ; a different algebra instance
  (check-exn exn:fail? (lambda () (sum2 r)))        ; r was built under `sum`

  ;; --- split-rope: cut at character position k ---
  (define ((at k) left right)
    (cond [(> left k) -1] [(< left k) 1] [else 0]))
  (define e (sum ""))
  (let-values ([(l rr) ((split-rope (at 3)) e r e)])
    (check-equal? (rope->string l)  "abc")
    (check-equal? (rope->string rr) "def"))
  (let-values ([(l rr) ((split-rope (at 2)) e r e)])
    (check-equal? (rope->string l)  "ab")
    (check-equal? (rope->string rr) "cdef"))
  (let-values ([(l rr) ((split-rope (at 0)) e r e)])
    (check-equal? (rope->string l)  "")
    (check-equal? (rope->string rr) "abcdef"))
  (let-values ([(l rr) ((split-rope (at 6)) e r e)])
    (check-equal? (rope->string l)  "abcdef")
    (check-equal? (rope->string rr) ""))

  ;; split on a chunked (multi-leaf) rope
  (let-values ([(l rr) ((split-rope (at 5)) e r2 e)])
    (check-equal? (rope->string l)  "hello")
    (check-equal? (rope->string rr) " world"))

  ;; --- seg-split: select window [a, b) by character position ---
  ;; seg-guide returns sgn(a-left) + sgn(b-left); (bound -1)/(bound +1) cut at a/b.
  (define ((seg a b) left right) (+ (sgn (- a left)) (sgn (- b left))))
  (let-values ([(l m rr) ((seg-split (seg 2 5)) e r e)])
    (check-equal? (rope->string l)  "ab")
    (check-equal? (rope->string m)  "cde")
    (check-equal? (rope->string rr) "f"))
  (let-values ([(l m rr) ((seg-split (seg 0 6)) e r e)])  ; whole thing
    (check-equal? (rope->string l)  "")
    (check-equal? (rope->string m)  "abcdef")
    (check-equal? (rope->string rr) ""))
  ;; Note: an *empty* selection (a = b) is a gap, not a segment. The ±1-offset
  ;; seg machinery has a 2-wide dead zone and cannot express a zero-width window;
  ;; a point cursor is split-rope's job, not seg-split's.
  )
