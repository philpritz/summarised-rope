#lang racket
;; THROWAWAY: (1) attribute char-smr's ~120ns "+" to the make-summary wrapper layers;
;; (2) build a vector+LAZY bundle and decompose its combine. Delete after.
(require racket/promise
         "../rope-core.rkt"          ; make-summary gen:summary-part part->summary summary-part?
         "../summaries/summaries.rkt"
         "../summaries/sexp-summary.rkt"
         "lex-normalform.rkt"
         "lex2-highlight-demo.rkt")

(define (ns label iters thunk)
  (thunk) (collect-garbage)
  (define-values (_ c r g) (time-apply (lambda () (for ([i (in-range iters)]) (thunk))) '()))
  (printf "  ~a ~a ns/op\n" (~a label #:min-width 34) (~a (~r (/ (* r 1e6) iters) #:precision 1) #:min-width 8 #:align 'right)))
(define N 2000000)

;; ---------- (1) where does char-smr's "+" go? ----------
(printf "(1) char-smr combine (a,b integers) decomposed:\n")
(define a 17) (define b 25)
(ns "raw (+ a b)"                 N (lambda () (+ a b)))
(ns "summary-part? a (generic pred)" N (lambda () (summary-part? a)))
(ns "char-smr a b (full wrapper)"  N (lambda () (char-smr a b)))
;; a hand-coerce mimicking make-summary's match, to show the per-operand cost
(define id0 0)
(define (coerce x)
  (cond [(equal? x "") id0] [(string? x) (string-length x)]
        [(summary-part? x) 'part] [else x]))
(ns "coerce a (the match, once)"   N (lambda () (coerce a)))

;; ---------- vector bundle (eager) and vector+lazy ----------
(struct vbv (slots index) #:methods gen:summary-part
  [(define (part->summary bv smr)
     (define i (hash-ref (vbv-index bv) smr #f)) (if i (vector-ref (vbv-slots bv) i) bv))])
(define (make-vbundle . cs)
  (define n (length cs)) (define comps (list->vector cs))
  (define index (for/hasheq ([c (in-list cs)] [i (in-naturals)]) (values c i)))
  (make-summary
   (lambda (s) (vbv (build-vector n (lambda (i) ((vector-ref comps i) s))) index))
   (lambda (x y) (define sx (vbv-slots x)) (define sy (vbv-slots y))
     (vbv (build-vector n (lambda (i) ((vector-ref comps i) (vector-ref sx i) (vector-ref sy i)))) index))))

(struct vlz (slots index) #:methods gen:summary-part
  [(define (part->summary bv smr)
     (define i (hash-ref (vlz-index bv) smr #f)) (if i (force (vector-ref (vlz-slots bv) i)) bv))])
(define (make-vlazy . cs)
  (define n (length cs)) (define comps (list->vector cs))
  (define index (for/hasheq ([c (in-list cs)] [i (in-naturals)]) (values c i)))
  (make-summary
   (lambda (s) (vlz (build-vector n (lambda (i) (let ([c (vector-ref comps i)]) (delay (c s))))) index))
   (lambda (x y) (vlz (build-vector n (lambda (i) (let ([c (vector-ref comps i)]) (delay (c x y))))) index))))

(define kw-smr (make-kw-smr '("define" "lambda" "let" "if" "cond")))
(define comps (list char-smr kw-smr strsexp-smr lex2-smr linecol-smr))
(define veag (apply make-vbundle comps))
(define vlaz (apply make-vlazy  comps))
(define s1 "(define (fact n)\n  (if (zero? n)\n      1\n      (* n (fact ")
(define s2 "(sub1 n)))))\n(displayln (fact 5))\n")
(define ea (veag s1)) (define eb (veag s2))
(define la (vlaz s1)) (define lb (vlaz s2))

(printf "\n(2) per-combine: vector-eager vs vector+lazy (5 components):\n")
(ns "vector-eager combine"          N (lambda () (veag ea eb)))
(ns "vector+lazy combine (no read)" N (lambda () (vlaz la lb)))
(ns "vector+lazy combine + force char+linecol"
    N (lambda () (let ([v (vlaz la lb)]) (char-smr v) (linecol-smr v))))
(printf "  (nav reads only char+linecol; strsexp/lex2/kw stay unforced under lazy)\n")
