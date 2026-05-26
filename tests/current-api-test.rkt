#lang racket

(require rackunit
         "../rope-core.rkt"
         "../zipper-core.rkt"
         "../summary-algebras.rkt")

(define char-sys
  (system char-count-algebra))

(define (char-rope text)
  ((string->rope char-sys) text #:chunk-size 2))

(define sexp-sys
  (system sexp-frontier-algebra))

(define sexp-source
  "(define square\n  (lambda (x)\n    (* x x)))\n(+ 1 2)")

(define sexp-rope
  ((string->rope sexp-sys) sexp-source #:chunk-size 3))

(define z0
  ((start sexp-sys) sexp-rope))

(define (zipper-output z)
  (define out (open-output-string))
  (write z out)
  (get-output-string out))

(test-case "character guide splits a rope at a position"
  (define z ((navigate (char-position 2)) ((start char-sys) (char-rope "abcd"))))
  (check-equal? (zipper->cursor-string z "^") "ab^cd")
  (check-equal? (gap-sides z list) '(2 2)))

(test-case "insert-left and insert-right preserve whole rope text"
  (define z ((navigate (char-position 2)) ((start char-sys) (char-rope "abcd"))))
  (define inserted (char-rope "XY"))
  (check-equal? (zipper->cursor-string (insert-left z inserted) "^")
                "abXY^cd")
  (check-equal? (zipper->cursor-string (insert-right z inserted) "^")
                "ab^XYcd"))

(test-case "absolute S-expression guides navigate by slot address"
  (define before-name ((navigate (before-sexp-guide '(0 2))) z0))
  (define after-name ((navigate (after-sexp-guide '(0 2))) z0))
  (check-equal? (zipper-sexp-address before-name) '(0 2))
  (check-equal? (zipper->cursor-string before-name "^")
                "(define ^square\n  (lambda (x)\n    (* x x)))\n(+ 1 2)")
  (check-equal? (zipper-sexp-address after-name) '(0 3))
  (check-equal? (zipper->cursor-string after-name "^")
                "(define square^\n  (lambda (x)\n    (* x x)))\n(+ 1 2)"))

(test-case "relative S-expression navigation composes address transformers and guide makers"
  (define before-name ((navigate (before-sexp-guide '(0 2))) z0))
  (define after-name ((relative-sexp after-sexp-guide) before-name))
  (define before-next ((relative-sexp
                        (compose before-sexp-guide next-sexp-address))
                       before-name))
  (check-equal? (zipper->cursor-string after-name "^")
                "(define square^\n  (lambda (x)\n    (* x x)))\n(+ 1 2)")
  (check-equal? (zipper->cursor-string before-next "^")
                "(define square\n  ^(lambda (x)\n    (* x x)))\n(+ 1 2)"))

(test-case "parent S-expression navigation is expressible as raw composition"
  (define before-x ((navigate (before-sexp-guide '(0 3 2 1))) z0))
  (define parent ((relative-sexp
                   (compose before-sexp-guide parent-sexp-address))
                  before-x))
  (check-equal? (zipper-sexp-address parent) '(0 3 2))
  (check-equal? (zipper->cursor-string parent "^")
                "(define square\n  (lambda ^(x)\n    (* x x)))\n(+ 1 2)"))

(test-case "zipper custom writer exposes cursor and total summaries"
  (define before-name ((navigate (before-sexp-guide '(0 2))) z0))
  (define rendered (zipper-output before-name))
  (check-true (string-contains? rendered "cursor-left:"))
  (check-true (string-contains? rendered "cursor-right:"))
  (check-true (string-contains? rendered "left-total-summary:"))
  (check-true (string-contains? rendered "right-total-summary:")))
