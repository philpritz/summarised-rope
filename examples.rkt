#lang racket

;; A navigation + editing session over a summarised rope, through the move/edit
;; (Vim-style) cursor: movement is a single-coordinate gap; editing *plants* a
;; both-ends seg whose index is stable across the edit -- so an insert covers
;; exactly what you typed and a delete leaves a gap at the hole (delete then
;; reinsert round-trips). Run:  racket examples.rkt
;;
;; Inline cursor:  | a gap,  [..] a seg (a selection, or what an edit covers).

(require "rope-core.rkt" "summaries.rkt" "zipper-core.rkt")

;; render a cursor inline; `off` projects the before-summary to a char offset.
(define ((shower off) tag z)
  (define h     (zipper-head z))
  (define o     (off (head-before h)))
  (define foc   (~a (head-rope h)))
  (define whole (text z))
  (printf "~a~a\n"
          (~a tag #:min-width 22)
          (if (at-gap? z)
              (string-append (substring whole 0 o) "|" (substring whole o))
              (string-append (substring whole 0 o) "[" foc "]"
                             (substring whole (+ o (string-length foc)))))))

;; ===== char: movement is a single coordinate; editing plants a both-ends seg =====
(printf "--- char: move, then plant + insert (the seg covers the insert) ---\n")
(define cshow (shower values))                       ; char-count summary IS the offset
(define hello ((roper char-count) "hello world"))
(cshow "navigate to 5:"  (navigate ((start (char-guide 5)) hello)))
(cshow "insert \"XYZ\":" (insert (navigate ((start (char-guide 5)) hello)) "XYZ"))

(printf "\n--- char: select, delete, reinsert (round-trips exactly) ---\n")
(define wsel (select ((start (char-guide 0)) hello) (list 6 0)))   ; the range "world"
(cshow "select \"world\":"   wsel)
(cshow "delete:"             (delete wsel))
(cshow "reinsert \"world\":" (insert (delete wsel) "world"))

;; ===== sexp: address forms by tree path; the seg is char-anchored within a frame =====
(printf "\n--- sexp: select forms by path ---\n")
(define sshow (shower sx-chars))
(define sdoc ((roper sexp) "(a (b c) d)"))
(define (sel p) (select ((start (sexp-guide)) sdoc) (sexp-form-span sdoc p)))
(sshow "form (0 1):"   (sel '(0 1)))                 ; child 1   -> a
(sshow "form (0 2):"   (sel '(0 2)))                 ; child 2   -> (b c)
(sshow "form (0 2 1):" (sel '(0 2 1)))               ; grandchild -> b

(printf "\n--- sexp: edit at a form -- replace, delete (no slurp), reinsert ---\n")
(sshow "replace (0 2)=X:"  (insert (sel '(0 2)) "X"))
(sshow "delete (0 2):"     (delete (sel '(0 2))))            ; gap at the hole, `d` not slurped
(sshow "reinsert (b c):"   (insert (delete (sel '(0 2))) "(b c)"))
