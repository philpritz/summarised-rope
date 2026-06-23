#lang racket
;; THROWAWAY: peel char-smr's combine layer by layer to attribute the cost to variadic's
;; structure (case-lambda dispatch + the (op (op id a) b) double-call) vs the spread/coerce
;; op it wraps. Delete after.
(require "../rope-core.rkt"            ; summary-part? part->summary
         (only-in "../helper-algebras.rkt" variadic spread))

(define (ns label iters thunk)
  (thunk) (collect-garbage)
  (define-values (_ c r g) (time-apply (lambda () (for ([i (in-range iters)]) (thunk))) '()))
  (printf "  ~a ~a ns/op\n" (~a label #:min-width 40) (~a (~r (/ (* r 1e6) iters) #:precision 1) #:min-width 8 #:align 'right)))
(define N 3000000)
(define a 17) (define b 25)

;; reconstruct char-smr's pieces exactly as make-summary builds them
(define id 0)
(define (coerce x)                                   ; the make-summary match, hand-written
  (cond [(equal? x "") id] [(string? x) (string-length x)]
        [(summary-part? x) (part->summary x #f)] [else x]))
(define op (spread + values coerce))                 ; the inner op: (lambda (x y) (+ (values x) (coerce y)))

(printf "peeling char-smr's combine (a,b = integers), inner -> outer:\n")
(ns "L0  raw (+ (+ 0 a) b)"                  N (lambda () (+ (+ 0 a) b)))
(ns "L0' raw + 2x coerce"                    N (lambda () (+ (coerce a) (coerce b))))
(ns "L1  (op a b)            spread+coerce, ONE call"  N (lambda () (op a b)))
(ns "L2  (op (op id a) b)    op called TWICE by hand"  N (lambda () (op (op id a) b)))
(ns "L3  ((variadic + id) a b)   variadic over BARE +" N (lambda () ((variadic + id) a b)))
(ns "L4  ((variadic op id) a b)  = char-smr"           N (lambda () ((variadic op id) a b)))

;; also: does the case-lambda dispatch itself cost? call the binary clause many ways
(define v+ (variadic + id))
(define vop (variadic op id))
(printf "\ndispatch isolation:\n")
(ns "v+  binary  ((variadic + id) a b)"      N (lambda () (v+ a b)))
(ns "v+  unary   ((variadic + id) a)"        N (lambda () (v+ a)))
(ns "vop binary  ((variadic op id) a b)"     N (lambda () (vop a b)))
