#lang racket

;; Per-keystroke cost on an ASTRONOMICALLY large document.
;;   * a generator (sexps + adversarial string literals + line/block comments, extending
;;     sexp-summary.rkt's test generator) builds a realistic ~few-thousand-line base;
;;   * the DAG trick doubles it: doubled(d) = branch(h, h) with BOTH children the same
;;     object, so doubled(d) is 2^d copies of the base in O(d) nodes (each shared node's
;;     summary computed once) -- documents far too large to ever allocate flat;
;;   * we navigate to the middle and time the per-keystroke cycle (insert a char + fold to
;;     the doc + re-render the 60-line viewport), under the lazy bundle.
;; Expectation: cost grows ~linearly in d (= O(log N)) while doc size grows 2^d -- i.e.
;; flat in document size.  d<=40 keeps line/char indexes fixnum (so we time the rope, not
;; bignum arithmetic).
;;
;; Run:  racket scratch/huge-doc-cost.rkt

(require racket/match racket/set rackcheck racket/promise
         "../rope-core.rkt"
         "../summaries/summaries.rkt"
         "../summaries/sexp-summary.rkt"
         "../helper-algebras.rkt"
         "../bench/bench.rkt"
         "lex-normalform.rkt"
         "lex2-highlight-demo.rkt"
         "../zipper-core.rkt")

;; ---- lazy bundle (from bundle-cost.rkt) ----
(struct lbv (slots)
  #:methods gen:summary-part
  [(define (part->summary bv smr)
     (if (hash-has-key? (lbv-slots bv) smr) (force (hash-ref (lbv-slots bv) smr)) bv))])
(define (lazy-bundle . cs)
  (make-summary
   (lambda (s)   (lbv (for/hasheq ([c (in-list cs)]) (values c (delay (c s))))))
   (lambda (a b) (lbv (for/hasheq ([c (in-list cs)]) (values c (delay (c a b))))))))

(define keywords '("define" "lambda" "let" "if" "cond" "list" "displayln" "map"))
(define kwset (list->set keywords))
(define kw-smr (make-kw-smr keywords))
(define buf (lazy-bundle char-smr kw-smr strsexp-smr lex2-smr linecol-smr))

;; ---- the generator: sexps + strings + comments ----
(define gen:kw  (gen:one-of '("define" "lambda" "let" "if" "cond" "list" "displayln" "map")))
(define gen:var (gen:one-of '("x" "xs" "y" "n" "acc" "f" "g" "k" "v" "foo" "bar")))
(define gen:num (gen:map gen:natural number->string))
;; NOTE: raw string literals are dropped here -- strsexp's string-entry parse of a base
;; containing strings is paren-unbalanced, so the DAG (which repeats base 2^d times)
;; accumulates an O(2^d) stack.  That is a DAG-repetition artifact (real docs don't repeat
;; an unbalanced fragment unboundedly), not a per-keystroke cost.  Sexps + comments only.
(define gen:atom (gen:frequency `((4 . ,gen:var) (2 . ,gen:num))))
(define gen:ws (gen:one-of '(" " " " "\n  " "\n      " "")))
(define (gen:node mk d)
  (gen:let ([kids (gen:frequency
                   `((1 . ,(gen:const '()))
                     (5 . ,(gen:let ([h gen:kw]
                                     [r (gen:list (if (zero? d) gen:atom (gen:tree mk d)) #:max-length (sub1 mk))])
                             (cons h r)))))]
            [seps (apply gen:tuple (make-list (max 0 (sub1 (length kids))) gen:ws))])
    (list kids seps)))
(define (gen:tree mk d)
  (if (zero? d) gen:atom (gen:frequency `((2 . ,gen:atom) (2 . ,(gen:node mk (sub1 d)))))))
(define (render t)
  (if (string? t) t
      (let loop ([ks (first t)] [seps (second t)] [acc ""])
        (cond
          [(null? ks)       (string-append "(" acc ")")]
          [(null? (cdr ks)) (string-append "(" acc (render (car ks)) ")")]
          [else (define a (car ks)) (define sep (car seps))
                (define sep* (if (and (string? a) (string? (cadr ks)) (equal? sep "")) " " sep))
                (loop (cdr ks) (cdr seps) (string-append acc (render a) sep*))]))))
(define gen:linec  (gen:map (gen:list gen:var #:max-length 5) (lambda (w) (string-append "; " (string-join w " ")))))
(define gen:blockc (gen:map (gen:list gen:var #:max-length 8) (lambda (w) (string-append "#| " (string-join w " ") " |#"))))
(define gen:form   (gen:map (gen:node 4 3) render))
(define gen:item   (gen:frequency `((6 . ,gen:form) (2 . ,gen:linec) (1 . ,gen:blockc))))

(file-stream-buffer-mode (current-output-port) 'line)
(random-seed 42)
;; gen:item (above) produces sexps + comments + strings.  But the DAG repeats a base 2^d
;; times, and if the base has a>0 (left-unmatched block-comment closers under block-mode
;; reading), lex2's bracket counter SUMS them geometrically -- a(d) ~ 2^d -- and lex2's cpl
;; (the per-level coupling vector, length a) grows to O(2^d).  Forcing lex2 then materializes
;; that vector and blows up.  Verified by inspection (a: 4,13,49,193,769,3073 over the DAG);
;; strsexp stays O(1).  It is the "combine is O(depth), degenerates to O(left) when nesting
;; is O(left)" caveat -- the DAG makes the effective nesting depth O(2^d).  A real document
;; never stacks 2^d unmatched closers, so this is a repetition artifact, not a per-keystroke
;; cost.  For the size-scaling table we repeat a fixed REGULAR block whose a stays 0, so the
;; only variable is document size.
(define base (apply string-append (make-list 60
  "(define (f x)\n  (if (> x 0)\n      (g x)\n      x))\n; a line comment here\n#| a block comment |#\n")))
(define base-nl (for/sum ([c (in-string base)] #:when (char=? c #\newline)) 1))
(define base-rope ((make-rope buf) base))
(define (doubled d) (for/fold ([h base-rope]) ([_ (in-range d)]) ((make-rope buf) h h)))

;; ---- the render + edit machinery (line-guided, carrying state) ----
(define ((col line k) L R)
  (match-define (linecol _ ll lc) (linecol-smr L))
  (cond [(not (= ll line)) (if (< ll line) 1 -1)] [(< lc k) 1] [(> lc k) -1] [else 0]))
(define (bump cs) (cons (add1 (car cs)) (cdr cs)))
(define (paren-spans+ fr regions d0)
  (let loop ([rs regions] [d d0] [acc '()])
    (cond [(null? rs) (values (sort acc < #:key car) d)]
          [(not (eq? (first (car rs)) 'code)) (loop (cdr rs) d acc)]
          [else (match-define (list _ a b) (car rs))
                (define-values (d2 acc2)
                  (for/fold ([d d] [out acc]) ([k (in-range a b)])
                    (define c (string-ref fr k))
                    (cond [(memv c openers) (values (add1 d) (cons (cons k d) out))]
                          [(memv c closers) (values (max 0 (sub1 d)) (cons (cons k (max 0 (sub1 d))) out))]
                          [else (values d out)])))
                (loop (cdr rs) d2 acc2)])))
(define (head-kw+ fr regions counts0)
  (let seg ([rs regions] [counts counts0] [acc '()])
    (cond [(null? rs) (values (sort acc < #:key car) counts)]
          [(eq? (first (car rs)) 'string) (seg (cdr rs) (bump counts) acc)]
          [(memq (first (car rs)) '(line block shebang)) (seg (cdr rs) counts acc)]
          [else (match-define (list _ a b) (car rs))
                (let ch ([k a] [counts counts] [acc acc])
                  (cond [(>= k b) (seg (cdr rs) counts acc)]
                        [(memv (string-ref fr k) openers) (ch (add1 k) (cons 0 counts) acc)]
                        [(memv (string-ref fr k) closers) (ch (add1 k) (if (> (length counts) 1) (bump (cdr counts)) (bump counts)) acc)]
                        [(atom-char? (string-ref fr k))
                         (define e (let s ([t k]) (if (and (< t b) (atom-char? (string-ref fr t))) (s (add1 t)) t)))
                         (define head? (and (> (length counts) 1) (= (car counts) 0)))
                         (ch e (bump counts) (if (and head? (set-member? kwset (substring fr k e))) (cons (cons k e) acc) acc))]
                        [else (ch (add1 k) counts acc)]))])))
(define (cw-col rope T R)
  (define-values (before screen after) ((multisect buf (vector (col T 0) (col (+ T R) 0))) rope))
  (define mode0 (apply-step (lex2-smr before) 'code))
  (define-values (front0 _) (strsexp-spines (strsexp-smr before) (strsexp-smr screen)))
  (define lines (take (regexp-split #rx"\n" (~a screen)) R))
  (let loop ([ls lines] [mode mode0] [depth (sub1 (length front0))] [counts (map intify front0)] [acc '()])
    (cond [(null? ls) (reverse acc)]
          [else (define-values (regions em) (lex-scan (car ls) mode))
                (define-values (kwS ec) (head-kw+ (car ls) regions counts))
                (define-values (pS ed)  (paren-spans+ (car ls) regions depth))
                (loop (cdr ls) em ed ec (cons (list mode (car ls) kwS (string-spans-of regions) (comment-spans-of regions) pS) acc))])))
(define (keystroke zc T R)
  (cw-col ((viewer zipper-focus) (to-root ((setter zipper-focus "x") zc))) T R))

;; ---- the sweep: double the doc, time one keystroke at the middle ----
(printf "base: ~a lines, ~a chars\n\n" base-nl (string-length base))
(printf "~a  ~a  ~a  ~a\n"
        (~a "d" #:min-width 4) (~a "doc lines" #:min-width 22)
        (~a "build ms" #:min-width 10) (~a "edit+render us" #:min-width 14))
(define R 60)
;; per-keystroke timing with a GC BEFORE each timed call -- the lazy bundle's per-keystroke
;; garbage pools across a tight rep-loop and GC-thrashes; collecting first keeps the heap
;; small so we time the keystroke work, not the pooled-garbage collection.  min of 5.
(define (keystroke-us zc T)
  (* 1000.0 (apply min (for/list ([_ (in-range 5)])
                         (collect-garbage)
                         (keystroke zc T R)                          ; warm this rep
                         (define t0 (current-inexact-milliseconds))
                         (keystroke zc T R)
                         (- (current-inexact-milliseconds) t0)))))
(for ([d (in-list '(0 8 16 24 30))])
  (define t0 (current-inexact-milliseconds))
  (define r (doubled d))
  (define t1 (current-inexact-milliseconds))
  (define lines (* base-nl (expt 2 d)))
  (define T (quotient lines 2))
  (define cursor (vector (col (+ T 30) 0) (col (+ T 30) 0)))   ; gap at line start (col 0 resolves cleanly)
  (define zc ((setter zipper-guide cursor) (start buf r cursor)))
  (printf "~a  ~a  ~a  ~a\n"
          (~a d #:min-width 4)
          (~a (~r lines #:notation 'positional) #:min-width 22)
          (~a (~r (- t1 t0) #:precision 1) #:min-width 10)
          (~a (~r (keystroke-us zc T) #:precision 1) #:min-width 14))
  (flush-output))
