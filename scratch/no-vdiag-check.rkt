#lang racket
;; gap verbs via the tuple (vref 0 1) instead of vdiag -- one lens primitive, all lawful
(require "../sexp-edit.rkt" "../helper-algebras.rkt")
(define index-of (make-lens (lambda (g) (values (guide-index g) slot-guide))))

(define (move* f)     (updater (vref 0 1) (lambda (gs) (make-list 2 ((updater index-of f) (car gs))))))
(define (at*   ix)    (move* (lambda (_) ix)))
(define (each* fl fr) (compose (updater (compose (vref 0) index-of) fl)
                               (updater (compose (vref 1) index-of) fr)))
(define (both* f)     (updater (vref 0 1) (curry map (updater index-of f))))

(define (doc z) (~a ((viewer zipper-focus) (to-root z))))
(define rope ((make-rope sexp-smr) "(aa bb cc)"))
(define z  (cursor rope '(1 0)))
(define z2 (cursor rope '(1 0) '(2 0)))
(printf "at*   ~s expect \"(aa bb xx cc)\"\n" (doc ((setter zipper-focus "xx ") ((updater zipper-guide (at* '(2 0))) z))))
(printf "move* ~s expect \"(aa bb xx cc)\"\n" (doc ((setter zipper-focus "xx ") ((updater zipper-guide (move* (slot add1))) z))))
(printf "each* ~s expect \"bb \"\n" (~a ((viewer zipper-focus) ((updater zipper-guide (each* values (slot add1))) z))))
(printf "both* ~s expect \"cc\"\n" (~a ((viewer zipper-focus) ((updater zipper-guide (both* (slot add1))) z2))))
