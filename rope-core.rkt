#lang racket

;; Summarised rope, variadic-polymorphic rewrite.
;;
;; The whole library is organised around three variadic "coerce-and-fold"
;; functions, one per carrier:
;;
;;   summary : (string | rope | summary)* -> summary    ; built by `summariser`
;;   roper   : (string | rope)*           -> rope        ; rope factory
;;   rope->string                                        ; read a rope's text
;;
;; A summary function is built by `summariser` and is the single handle threaded
;; into rope construction (what older versions called `sys`). Threaded summary
;; handles are bound as `smr` to keep them distinct from the canonical `summary`
;; function. Every rope node is tagged with the summary it was built under, so
;; `summary` can verify (by eq?) that a rope's cached value belongs to the
;; summary now folding with it.
;;
;; Factories carry an `-er`/`-r` suffix to read as "the thing that makes X":
;; `summariser`, `roper`, `splitter`, `rope-splitter`, `seg-splitter`. Each takes
;; its config and returns the worker function.
;;
;; Design notes: discussions/2026-05-29/2-claude.md.

(require racket/match)

(provide
 summariser
 roper
 rope->string
 splitter
 rope-splitter
 seg-splitter)

;; ---------- nodes ----------
;; Each node caches its summary value AND the summary fn it was built under.
;; `rope-summary` reads the cached value; `rope-algebra` reads the fn.

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

;; ---------- summariser ----------
;; (summariser measure combine) -> the variadic `summary` fn, the single handle
;; threaded into rope construction.
;;
;;   (summary str)         = (measure str)
;;   (summary a b c ...)   = combine, folded left-to-right (order matters; a
;;                           monoid is associative but not commutative)
;;
;; Arguments interleave: strings are measured, ropes contribute their cached
;; summary (guarded same-summary), summaries pass through. Identity is
;; (summary "") -- no separate empty (relies on measure being a homomorphism).

(define (summariser measure combine)
  (define (summary . parts)
    (define (->s x)
      (cond
        [(string? x) (measure x)]
        [(rope? x)
         (if (eq? (rope-algebra x) summary)
             (rope-summary x)
             (error 'summary
                    "rope was summarised under a different summary; reconstruction unsupported"))]
        [else x]))
    (when (null? parts)
      (error 'summary "needs at least one argument"))
    (foldl (lambda (x acc) (combine acc (->s x)))
           (->s (car parts))
           (cdr parts)))
  summary)

;; ---------- leaf / piece helpers (internal) ----------

(define ((leaf-rope smr) text)
  (leaf text (smr text) smr))

(define (make-leaf-range smr text start end)
  (if (= start end)
      (empty-rope smr)
      (leaf-range text start end (smr (substring text start end)) smr)))

(define (empty-rope smr)
  (leaf "" (smr "") smr))

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

;; Termination guard for descent: a rope that `splitter` cannot make progress on
;; (a leaf of length <= 1). Branches are never atomic.
(define (atom? r)
  (and (not (branch? r)) (<= (leaf-piece-length r) 1)))

;; Bisect a non-atomic leaf/leaf-range into two ranges over the same backing
;; string (no copy). The summary is recovered from the piece.
(define (split-leaf-piece piece)
  (define smr (rope-algebra piece))
  (define-values (text start end) (leaf-piece-bounds piece))
  (define mid (+ start (quotient (- end start) 2)))
  (values (make-leaf-range smr text start mid)
          (make-leaf-range smr text mid end)))

;; Two adjacent ranges of the same backing string re-fuse into one leaf.
(define (leaf-compatible? l r)
  (and (leaf-range? l) (leaf-range? r)
       (eq? (leaf-range-text l) (leaf-range-text r))
       (= (leaf-range-end l) (leaf-range-start r))))

;; ---------- branch / concat (internal) ----------

(define ((branch-rope smr) l r)
  (branch l r (smr l r) smr))

;; Smart joiner: drops empties, re-fuses adjacent compatible ranges, else
;; branches. This is the rope "rise"/join step.
(define ((concat-rope smr) . ropes)
  (foldr (lambda (l r)
           (cond
             [(empty-rope? l) r]
             [(empty-rope? r) l]
             [(leaf-compatible? l r)
              ((leaf-rope smr) (string-append (piece-text l) (piece-text r)))]
             [else ((branch-rope smr) l r)]))
         (empty-rope smr)
         ropes))

(define (chunk-string text n)
  (for/list ([start (in-range 0 (string-length text) n)])
    (substring text start (min (string-length text) (+ start n)))))

;; ---------- roper (rope factory) ----------
;; ((roper smr [#:chunk-size n]) . parts) assembles strings (chunked into
;; leaves) and ropes (passed through) by a dumb concat fold. Subsumes the old
;; string->rope (chunk + assemble) and concat-rope (all-ropes case). Balancing
;; is deferred -- the fold is a right-leaning spine for now.

(define ((roper smr #:chunk-size [chunk 1024]) . parts)
  (define (->rope x)
    (if (string? x)
        (apply (concat-rope smr) (map (leaf-rope smr) (chunk-string x chunk)))
        x))
  (apply (concat-rope smr) (map ->rope parts)))

;; ---------- read ----------

(define (rope->string r)
  (match r
    [(leaf text _ _)           text]
    [(leaf-range text s e _ _) (substring text s e)]
    [(branch l r _ _)          (string-append (rope->string l) (rope->string r))]))

;; ---------- splitter: one-level guided eliminator ----------
;; Precondition: (not (atom? mr)). Does exactly one structural level: derive the
;; two children with their contexts threaded, read the guide at the split point,
;; dispatch to one handler. No recursion of its own. The summary is recovered
;; from mr.
;;
;;   ((splitter guide on-l on-r on-here) before mr after)
;;     guide   : left-total-summary right-total-summary -> -1 | 0 | 1
;;     on-l    : before L after-of-L R-sibling  -> a   ; boundary in the left child
;;     on-r    : L-sibling before-of-R R after  -> a   ; boundary in the right child
;;     on-here : L R                            -> a   ; boundary between the children

(define ((splitter guide on-l on-r on-here) before mr after)
  (define smr (rope-algebra mr))
  (define-values (L R)
    (if (branch? mr)
        (values (branch-left mr) (branch-right mr))
        (split-leaf-piece mr)))
  (case (guide (smr before L) (smr R after))
    [(-1) (on-l before L (smr R after) R)]
    [(1)  (on-r L (smr before L) R after)]
    [(0)  (on-here L R)]
    [else (error 'splitter "guide must return -1, 0, or 1")]))

;; ---------- rope-splitter: partition at one boundary ----------
;; ((rope-splitter guide) before mr after) -> (values left right)
;; Recurses via `splitter`, reassembling the untouched sibling on the correct side.

(define ((rope-splitter guide) before mr after)
  (define smr (rope-algebra mr))
  (define (cat . rs) (apply (roper smr) rs))
  (let walk ([before before] [mr mr] [after after])
    (cond
      [(atom? mr)
       (if (positive? (guide before (smr mr after)))
           (values mr (empty-rope smr))
           (values (empty-rope smr) mr))]
      [else
       ((splitter guide
          (lambda (b L a R)
            (define-values (ll lr) (walk b L a))
            (values ll (cat lr R)))
          (lambda (L b R a)
            (define-values (rl rr) (walk b R a))
            (values (cat L rl) rr))
          (lambda (L R) (values L R)))
        before mr after)])))

;; ---------- seg-splitter: select a segment between two boundaries ----------
;; ((seg-splitter seg-guide) before mr after) -> (values left mid right)
;; Two rope-splitter passes: the -1 cut finds the left edge, the +1 cut the right.

(define ((seg-splitter seg-guide) before mr after)
  (define smr (rope-algebra mr))
  (define ((bound off) sl sr) (sgn (+ (seg-guide sl sr) off)))
  (define-values (l rest) ((rope-splitter (bound -1)) before mr after))
  (define-values (m r)    ((rope-splitter (bound 1)) (smr before l) rest after))
  (values l m r))

;; ============================================================================
(module+ test
  (require rackunit)

  ;; A trivial summary: summary = character count.
  (define sum (summariser string-length +))

  ;; --- build & read ---
  (define r ((roper sum) "abcdef"))
  (check-equal? (rope->string r) "abcdef")
  (check-equal? (sum r) 6)                         ; rope coerced -> cached summary

  ;; chunked build still round-trips and summarises
  (define r2 ((roper sum #:chunk-size 2) "hello world"))
  (check-equal? (rope->string r2) "hello world")
  (check-equal? (sum r2) 11)

  ;; --- interleaving strings / ropes / summaries ---
  (check-equal? (sum "ab" r "x") (+ 2 6 1))
  (check-equal? (sum 5 r)        (+ 5 6))          ; a summary value (number) passes through
  (check-equal? (sum "")         0)                ; identity = (summary "")

  ;; assembling mixed parts into a rope
  (define joined ((roper sum) "(" r ")"))
  (check-equal? (rope->string joined) "(abcdef)")
  (check-equal? (sum joined) 8)

  ;; --- same-summary guard ---
  (define sum2 (summariser string-length +))       ; a different summary instance
  (check-exn exn:fail? (lambda () (sum2 r)))        ; r was built under `sum`

  ;; --- rope-splitter: cut at character position k ---
  (define ((at k) left right)
    (cond [(> left k) -1] [(< left k) 1] [else 0]))
  (define e (sum ""))
  (let-values ([(l rr) ((rope-splitter (at 3)) e r e)])
    (check-equal? (rope->string l)  "abc")
    (check-equal? (rope->string rr) "def"))
  (let-values ([(l rr) ((rope-splitter (at 2)) e r e)])
    (check-equal? (rope->string l)  "ab")
    (check-equal? (rope->string rr) "cdef"))
  (let-values ([(l rr) ((rope-splitter (at 0)) e r e)])
    (check-equal? (rope->string l)  "")
    (check-equal? (rope->string rr) "abcdef"))
  (let-values ([(l rr) ((rope-splitter (at 6)) e r e)])
    (check-equal? (rope->string l)  "abcdef")
    (check-equal? (rope->string rr) ""))

  ;; split on a chunked (multi-leaf) rope
  (let-values ([(l rr) ((rope-splitter (at 5)) e r2 e)])
    (check-equal? (rope->string l)  "hello")
    (check-equal? (rope->string rr) " world"))

  ;; --- seg-splitter: select window [a, b) by character position ---
  ;; seg-guide returns sgn(a-left) + sgn(b-left); (bound -1)/(bound +1) cut at a/b.
  (define ((seg a b) left right) (+ (sgn (- a left)) (sgn (- b left))))
  (let-values ([(l m rr) ((seg-splitter (seg 2 5)) e r e)])
    (check-equal? (rope->string l)  "ab")
    (check-equal? (rope->string m)  "cde")
    (check-equal? (rope->string rr) "f"))
  (let-values ([(l m rr) ((seg-splitter (seg 0 6)) e r e)])  ; whole thing
    (check-equal? (rope->string l)  "")
    (check-equal? (rope->string m)  "abcdef")
    (check-equal? (rope->string rr) ""))
  ;; Note: an *empty* selection (a = b) is a gap, not a segment. The ±1-offset
  ;; seg machinery has a 2-wide dead zone and cannot express a zero-width window;
  ;; a point cursor is rope-splitter's job, not seg-splitter's.
  )
