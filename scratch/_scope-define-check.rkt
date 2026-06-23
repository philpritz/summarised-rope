#lang racket
;; THROWAWAY: does scope-summary handle `define`? Delete after.
(require "scope-summary.rkt")
(define (show s)
  (printf "~s\n" s)
  (for ([sp (in-list (analyze s))])
    (match-define (list a b cls) sp)
    (printf "   ~a [~a,~a) ~a\n" (~a (substring s a b) #:min-width 10) a b cls))
  (printf "\n"))
(show "(define x 1)")
(show "(define (f a b) (+ a b))")
(show "(lambda () (define y 2) (+ y 1))")     ; internal define inside a body
(show "(let ([x 1]) (define z x) (+ x z))")    ; define referencing a let binding
