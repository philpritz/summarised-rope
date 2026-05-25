#lang racket

(require rackunit
         "../core.rkt"
         "../measures.rkt")

(define sys (system editor-algebra))
(define from-string (string->rope sys))
(define split-at (split-rope sys))
(define insert-at (insert-rope sys))
(define delete-at (delete-rope sys))
(define slice-at (slice-rope sys))
(define replace-at (replace-rope sys))
(define join (concat-rope sys))
(define valid? (valid-rope? sys))

(define text "hello\nfunctional world")
(define roundtrip-rope (from-string text #:chunk-size 5))
(check-equal? (rope->string roundtrip-rope) text)
(check-true (valid? roundtrip-rope))

(define-values (hello-left hello-right)
  (split-at (from-string "hello world" #:chunk-size 3)
            (char-position 5)))
(check-equal? (rope->string hello-left) "hello")
(check-equal? (rope->string hello-right) " world")
(check-true (valid? hello-left))
(check-true (valid? hello-right))

(define-values (end-left end-right)
  (split-at (from-string "hello world" #:chunk-size 3)
            (position-from-end 5)))
(check-equal? (rope->string end-left) "hello ")
(check-equal? (rope->string end-right) "world")

(define-values (line-left line-right)
  (split-at (from-string "abc\ndef\nghi" #:chunk-size 2)
            (line-column 1 2)))
(check-equal? (rope->string line-left) "abc\nde")
(check-equal? (rope->string line-right) "f\nghi")

(define edit-start (from-string "hello world" #:chunk-size 3))
(define comma (from-string "," #:chunk-size 3))
(define inserted (insert-at edit-start (char-position 5) comma))
(define deleted (delete-at inserted (char-position 5) (char-position 1)))
(define sliced (slice-at edit-start (char-position 6) (char-position 5)))
(define replaced
  (replace-at edit-start
              (char-position 6)
              (char-position 5)
              (from-string "rope")))
(check-equal? (rope->string inserted) "hello, world")
(check-equal? (rope->string deleted) "hello world")
(check-equal? (rope->string sliced) "world")
(check-equal? (rope->string replaced) "hello rope")

(define word-left (from-string "hel"))
(define word-right (from-string "lo world"))
(define word-rope (join word-left word-right))
(check-equal? (hash-ref (rope-measure word-left) 'words) 1)
(check-equal? (hash-ref (rope-measure word-right) 'words) 2)
(check-equal? (hash-ref (rope-measure word-rope) 'words) 2)

(define balanced (from-string "(a (b c))" #:chunk-size 2))
(define unbalanced (from-string ")a(" #:chunk-size 1))
(check-true (parens-balanced? (rope-measure balanced)))
(check-false (parens-balanced? (rope-measure unbalanced)))
(check-equal? (hash-ref (rope-measure balanced) 'paren-max) 2)

(define vowel-sys (system vowel-algebra))
(define vowel-from-string (string->rope vowel-sys))
(define vowel-rope (vowel-from-string "hello measured rope" #:chunk-size 4))
(check-equal? (rope-measure vowel-rope) 8)
(check-equal? (rope->string vowel-rope) "hello measured rope")

(define sexp-sys (system sexp-path-algebra))
(define sexp-from-string (string->rope sexp-sys))
(define split-sexp (split-rope sexp-sys))
(define sexp-source
  "(define square\n  (lambda (x)\n    (* x x)))\n(+ 1 2)")
(define sexp-rope
  (sexp-from-string sexp-source #:chunk-size 3))
(define-values (before-name at-name)
  (split-sexp sexp-rope (sexp-path '(0 1))))
(check-equal? (rope->string before-name) "(define ")
(check-true (string-prefix? (rope->string at-name) "square"))
(define-values (before-x at-x)
  (split-sexp sexp-rope (sexp-path '(0 2 1 0))))
(check-true (string-suffix? (rope->string before-x) "(lambda ("))
(check-true (string-prefix? (rope->string at-x) "x)"))
