#lang racket
;; THROWAWAY: eyeball the new binding forms. Delete after.
(require "scope-summary.rkt")
(define (show s)
  (printf "~s\n" s)
  (for ([sp (in-list (analyze s))])
    (match-define (list a b cls) sp)
    (printf "   ~a ~a\n" (~a (substring s a b) #:min-width 12) cls))
  (printf "\n"))
;; the original four still right?
(show "(let ([x 1] [y 2]) (+ x y z))")
(show "(let* ([x 1] [y x]) (+ x y))")
;; new forms
(show "(define x 1)")
(show "(define (f a b) (+ a b x))")
(show "(lambda (a b . rest) (f a b rest c))")
(show "(let-values ([(q r) (quotient/remainder a b)]) (+ q r s))")
(show "(letrec ([ev? (lambda (n) (od? n))] [od? (lambda (n) (ev? n))]) (ev? 10))")
(show "(let loop ([i 0] [acc '()]) (loop (add1 i) acc))")
(show "(for ([x xs] [y ys]) (cons x (cons y z)))")
(show "(define (g n) (define h (* n 2)) (+ h n k))")   ; internal define + sibling visibility
(show "(begin (define a 1) (define b a) (+ a b c))")    ; sequential top-ish defines in begin
