#lang racket

;; A longer editing / navigation session over the sexp summary, showing the
;; pieces working together. Run with:  racket examples.rkt
;;
;; Cursor shown inline -- | is a gap (point), [..] a selected segment -- and
;; d=N is the paren depth at the cursor (read from the sexp summary).

(require "rope-core.rkt" "summaries.rkt" "zipper-core.rkt")

(define (show tag z)
  (define h     (zipper-head z))
  (define o     (sx-chars (head-before h)))
  (define foc   (~a (head-rope h)))
  (define whole (text z))
  (printf "~a~a   d=~a\n"
          (~a tag #:min-width 26)
          (if (at-gap? z)
              (string-append (substring whole 0 o) "|" (substring whole o))
              (string-append (substring whole 0 o) "[" foc "]"
                             (substring whole (+ o (string-length foc)))))
          (sx-depth (head-before h))))

;; three navigation dimensions over the sexp summary, sharing one index
(define (by-symbol i [m 'gap]) (run-axis sx-atoms sx-starts-atom? sx-ends-atom? i m))
(define (by-char   i [m 'gap]) (axis sx-chars i m))
(define (by-open   i [m 'gap]) (axis sx-opens i m))
(define ((win field a b) l r) (+ (sgn (- a (field l))) (sgn (- b (field l)))))

(printf "doc: (a b c)\n\n")
(define src ((roper sexp) "(a b c)"))

(define z (navigate ((start (by-symbol 0 'seg)) src)))
(show "select symbol 0:" z)
(set! z (navigate (with-index z 1)))                      (show "  -> symbol 1:" z)
(set! z (navigate (with-index z 2)))                      (show "  -> symbol 2:" z)

;; gap before symbol 1, then insert -- it absorbs into the symbol
(set! z (navigate (gap-mode (with-index z 1))))           (show "gap before symbol 1:" z)
(set! z (insert z "X"))                                   (show "insert \"X\" (absorb):" z)

;; select symbol 2 and replace it (insert over a selection replaces)
(set! z (navigate (with-index z 2)))                      (show "select symbol 2:" z)
(set! z (insert z "cat"))                                 (show "replace with \"cat\":" z)

;; select symbol 0 and delete it
(set! z (navigate (with-index z 0)))                      (show "select symbol 0:" z)
(set! z (delete z))                                       (show "delete it:" z)

;; re-aim onto open-parens; step inside the list -- navigation only, depth shows it
(set! z (navigate (with-axis (with-index (gap-mode z) 1) sx-opens)))
(show "inside the list:" z)

;; re-aim onto characters; insert a leading marker at offset 0
(set! z (navigate (with-axis (with-index z 0) sx-chars)))  (show "char offset 0:" z)
(set! z (insert z ";"))                                    (show "insert \";\":" z)

;; select a character range [3, 6) and delete it
(set! z (delete (select-seg (to-root z) (win sx-chars 3 6))))
(show "delete chars [3,6):" z)

(printf "\nfinal text: ~s\n" (text z))
