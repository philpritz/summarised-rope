#lang racket
;; THROWAWAY: decompose ONE vector-bundle combine into its parts -- allocation vs the 5
;; component combines -- to see what dominates once the hasheq is gone. Delete after.
(require "../rope-core.rkt"
         "../summaries/summaries.rkt"
         "../summaries/sexp-summary.rkt"
         "lex-normalform.rkt"
         "lex2-highlight-demo.rkt")

;; vector bundle (copied from render-highlight/vbundle.rkt)
(struct vbv (slots index)
  #:methods gen:summary-part
  [(define (part->summary bv smr)
     (define i (hash-ref (vbv-index bv) smr #f))
     (if i (vector-ref (vbv-slots bv) i) bv))])
(define (make-vbundle . cs)
  (define n (length cs)) (define comps (list->vector cs))
  (define index (for/hasheq ([c (in-list cs)] [i (in-naturals)]) (values c i)))
  (make-summary
   (lambda (s) (vbv (build-vector n (lambda (i) ((vector-ref comps i) s))) index))
   (lambda (a b)
     (define sa (vbv-slots a)) (define sb (vbv-slots b))
     (vbv (build-vector n (lambda (i) ((vector-ref comps i) (vector-ref sa i) (vector-ref sb i)))) index))))

(define kw-smr (make-kw-smr '("define" "lambda" "let" "if" "cond")))
(define comps (list char-smr kw-smr strsexp-smr lex2-smr linecol-smr))
(define names '(char kw strsexp lex2 linecol))
(define vbuf  (apply make-vbundle comps))

;; realistic operands: leaf summaries of two halves of a chunk
(define s1 "(define (fact n)\n  (if (zero? n)\n      1\n      (* n (fact ")
(define s2 "(sub1 n)))))\n(displayln (fact 5))\n")
(define va (vbuf s1)) (define vb (vbuf s2))
;; each component's raw operands (what build-vector hands it)
(define ops (for/list ([c comps]) (cons (c va) (c vb))))

(define (ns label iters thunk)
  (thunk) (collect-garbage)
  (define-values (_ c r g) (time-apply (lambda () (for ([i (in-range iters)]) (thunk))) '()))
  (printf "  ~a ~a ns/op\n" (~a label #:min-width 28) (~a (~r (/ (* r 1e6) iters) #:precision 1) #:min-width 8 #:align 'right)))

(define N 1000000)
(printf "ONE vector-bundle combine, decomposed (operands = realistic leaf summaries):\n")
(ns "TOTAL (vbuf a b)" N (lambda () (vbuf va vb)))
(printf "  -- the 5 component combines, each (c slot-a slot-b):\n")
(for ([c (in-list comps)] [nm (in-list names)] [o (in-list ops)])
  (ns (format "   ~a" nm) N (lambda () (c (car o) (cdr o)))))
(printf "  -- pure allocation (build-vector 5 + struct, no component work):\n")
(define z (vbv-slots va))
(ns "   build-vector+struct" N (lambda () (vbv (build-vector 5 (lambda (i) (vector-ref z i))) #f)))
