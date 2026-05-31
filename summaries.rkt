#lang racket

;; Concrete summary algebras built with `summariser`, plus the projections the
;; zipper's guides read out of them. Analogous to the old per-domain summaries.

(require "rope-core.rkt")

(provide
 ;; char-count: summary = number of characters. Its own projection is identity.
 char-count
 ;; sexp: a balanced-paren ("opens frontier") summary, with projections.
 sexp
 (struct-out sx)
 sx-depth
 sx-balanced?)

;; ---------- char-count ----------
;; The trivial summary: a character count. The summary value is the number, so a
;; guide's projection over it is just `values` (identity).
(define char-count (summariser string-length +))

;; ---------- sexp (opens frontier) ----------
;; A balanced-paren summary. For a chunk it records:
;;   chars  : character count
;;   opens  : count of "("        (monotone -> a clean axis for guides)
;;   closes : count of ")"        (monotone)
;;   lo     : minimum running net-depth within the chunk (<= 0), so unmatched
;;            closes are visible and the monoid can locate matches.
;; net depth = opens - closes; combine threads `lo` through the left chunk's depth.
(struct sx (chars opens closes lo) #:transparent)

(define sexp
  (summariser
   ;; measure: scan the chunk, tracking running net-depth `d` and its minimum.
   (lambda (str)
     (for/fold ([n 0] [o 0] [c 0] [d 0] [lo 0] #:result (sx n o c lo))
               ([ch (in-string str)])
       (define d* (cond [(char=? ch #\() (add1 d)]
                        [(char=? ch #\)) (sub1 d)]
                        [else d]))
       (values (add1 n)
               (if (char=? ch #\() (add1 o) o)
               (if (char=? ch #\)) (add1 c) c)
               d*
               (min lo d*))))
   ;; combine: counts add; lo = min(left.lo, left.depth + right.lo).
   (lambda (a b)
     (sx (+ (sx-chars a)  (sx-chars b))
         (+ (sx-opens a)  (sx-opens b))
         (+ (sx-closes a) (sx-closes b))
         (min (sx-lo a) (+ (- (sx-opens a) (sx-closes a)) (sx-lo b)))))))

;; net open depth at the end of the chunk (opens minus closes).
(define (sx-depth s) (- (sx-opens s) (sx-closes s)))

;; a chunk is a balanced run iff it never dips below 0 and returns to 0.
(define (sx-balanced? s) (and (= 0 (sx-depth s)) (>= (sx-lo s) 0)))

;; ============================================================================
(module+ test
  (require rackunit)
  (define s (sexp "(a (b c) d)"))
  (check-equal? (sx-chars s)  11)
  (check-equal? (sx-opens s)  2)
  (check-equal? (sx-closes s) 2)
  (check-equal? (sx-depth s)  0)
  (check-true   (sx-balanced? s))
  ;; coerced through a rope, the cached summary matches
  (define r ((roper sexp) "(a (b c) d)"))
  (check-equal? (sx-opens (sexp r)) 2)
  ;; an unmatched close shows up as lo < 0
  (check-true (negative? (sx-lo (sexp ") ("))))
  (check-false (sx-balanced? (sexp "(()"))))
