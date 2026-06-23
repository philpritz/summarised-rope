#lang racket
;; throwaway: the editing verbs as inline vector-lens expressions over vref + vdiag,
;; index-of the single guide<->index bridge.  Checked against the real engine.
(require "../sexp-edit.rkt"
         "../helper-algebras.rkt")

(define index-of (make-lens (lambda (g) (values (guide-index g) slot-guide))))

(define (at*   ix)    (setter  (compose vdiag index-of) ix))
(define (move* f)     (updater (compose vdiag index-of) f))
(define (each* fl fr) (compose (updater (compose (vref 0) index-of) fl)
                               (updater (compose (vref 1) index-of) fr)))
(define (both* f)     (updater (vref 0 1) (curry map (updater index-of f))))

(define (doc z) (~a ((viewer zipper-focus) (to-root z))))
(define rope ((make-rope sexp-smr) "(aa bb cc)"))
(define z  (cursor rope '(1 0)))
(define z2 (cursor rope '(1 0) '(2 0)))

(printf "at*   ~s expect \"(aa bb xx cc)\"\n"
        (doc ((setter zipper-focus "xx ") ((updater zipper-guide (at* '(2 0))) z))))
(printf "move* ~s expect \"(aa bb xx cc)\"\n"
        (doc ((setter zipper-focus "xx ") ((updater zipper-guide (move* (slot add1))) z))))
(printf "each* ~s expect \"bb \"\n"
        (~a ((viewer zipper-focus) ((updater zipper-guide (each* values (slot add1))) z))))
(printf "both* ~s expect \"cc\"\n"
        (~a ((viewer zipper-focus) ((updater zipper-guide (both* (slot add1))) z2))))
