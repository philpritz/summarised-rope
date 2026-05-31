#lang racket

;; Concrete summary algebras built with `summariser`, plus the projections the
;; zipper's guides read out of them. Analogous to the old per-domain summaries
;; (deprecated-2/summary-algebras.rkt: char-count, word-count, sexp-frontier).

(require "rope-core.rkt")

(provide
 ;; char-count: summary = number of characters. Its own projection is identity.
 char-count
 ;; sexp: parens + atom (symbol) runs.
 sexp
 (struct-out sx)
 sx-depth
 sx-balanced?)

;; ---------- char-count ----------
;; The trivial summary: a character count. The summary value is the number, so a
;; guide's projection over it is just `values` (identity).
(define char-count (summariser string-length +))

;; ---------- sexp (parens + symbol runs) ----------
;; A chunk's summary records both the paren structure and the atom (symbol) runs:
;;   chars       : character count
;;   opens/closes: counts of "(" / ")"   (monotone)
;;   lo          : minimum running net-depth (<= 0); unmatched closes show up here
;;   starts-atom?: does the chunk begin mid-atom (first char is a symbol char)?
;;   atoms       : number of atom runs (maximal non-delimiter spans)
;;   ends-atom?  : does the chunk end mid-atom?
;; The atom count is a *run-merging* monoid (adapted from the old word-count): two
;; chunks whose seam is mid-atom share one run, so the join subtracts a double-count.
;; A delimiter is whitespace or a paren.
(struct sx (chars opens closes lo starts-atom? atoms ends-atom?) #:transparent)

(define (atom-char? ch)
  (not (or (char-whitespace? ch) (char=? ch #\() (char=? ch #\)))))

(define sexp
  (summariser
   ;; measure: one scan tracking net-depth d (+ its min) and atom runs.
   (lambda (str)
     (for/fold ([o 0] [c 0] [d 0] [lo 0] [atoms 0] [in? #f] [starts? #f] [seen? #f]
                #:result (sx (string-length str) o c lo starts? atoms in?))
               ([ch (in-string str)])
       (define a? (atom-char? ch))
       (define d* (cond [(char=? ch #\() (add1 d)] [(char=? ch #\)) (sub1 d)] [else d]))
       (values (if (char=? ch #\() (add1 o) o)
               (if (char=? ch #\)) (add1 c) c)
               d* (min lo d*)
               (+ atoms (if (and a? (not in?)) 1 0))     ; a new run starts here
               a?                                         ; in-run state -> ends-atom?
               (if seen? starts? a?)                      ; first char's atom-ness
               #t)))
   ;; combine: counts add; lo threads left depth; atom runs merge across the seam.
   (lambda (a b)
     (sx (+ (sx-chars a)  (sx-chars b))
         (+ (sx-opens a)  (sx-opens b))
         (+ (sx-closes a) (sx-closes b))
         (min (sx-lo a) (+ (- (sx-opens a) (sx-closes a)) (sx-lo b)))
         (if (zero? (sx-chars a)) (sx-starts-atom? b) (sx-starts-atom? a))
         (- (+ (sx-atoms a) (sx-atoms b))
            (if (and (sx-ends-atom? a) (sx-starts-atom? b)) 1 0))
         (if (zero? (sx-chars b)) (sx-ends-atom? a) (sx-ends-atom? b))))))

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
  (check-equal? (sx-atoms s)  4)         ; a b c d
  (check-true   (sx-balanced? s))

  ;; run-merging across a seam: the variadic `sexp` combines summaries directly
  (check-equal? (sx-atoms (sexp "ab")) 1)
  (check-equal? (sx-atoms (sexp (sexp "a")  (sexp "b"))) 1)   ; "a" + "b"  = one atom
  (check-equal? (sx-atoms (sexp (sexp "a ") (sexp "b"))) 2)   ; "a " + "b" = two atoms

  ;; coerced through a rope (even chunked), the cached summary matches
  (define r ((roper sexp #:chunk-size 1) "(a (b c) d)"))
  (check-equal? (sx-atoms (sexp r)) 4)
  (check-equal? (sx-opens (sexp r)) 2)

  ;; balance: unmatched close shows up as lo < 0
  (check-true  (negative? (sx-lo (sexp ") ("))))
  (check-false (sx-balanced? (sexp "(()"))))
