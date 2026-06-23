#lang racket
;; THROWAWAY: show analyze bound/free vs the monoid's in-scope set for the deferred forms.
(require "scope-summary.rkt")
(define (show s body-prefix)
  (printf "~s\n" s)
  (printf "   analyze bound: ~s\n"
          (for/list ([sp (in-list (analyze s))] #:when (eq? (third sp) 'bound)) (substring s (first sp) (second sp))))
  (printf "   analyze free : ~s\n"
          (for/list ([sp (in-list (analyze s))] #:when (eq? (third sp) 'free)) (substring s (first sp) (second sp))))
  (printf "   monoid in-scope at body: ~s\n\n" (sv-in-scope (scope-leaf body-prefix))))
(show "(let-values ([(q r) (f)]) (+ q r))"   "(let-values ([(q r) (f)]) ")
(show "(let ([a 1]) (+ a b))"                  "(let ([a 1]) ")            ; sanity: works
(show "(define-values (a b) (f)) (+ a b)"      "(define-values (a b) (f)) ")
(show "(define ((f a) b) (+ a b))"             "(define ((f a) b) ")
