#lang racket

;; A structural editing / navigation session over the sexp opens-frontier summary.
;; The index is a tree PATH -- '(0) is the top-level form, children are 1-indexed
;; inside it -- so navigation addresses real sexp nodes and the moves (parent,
;; next/previous sibling) are just edits to that path.  Run:  racket examples.rkt
;;
;; Cursor inline: | a gap (point), [..] a selected form.  d=N = paren depth.

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
          (sexp-depth (head-before h))))

;; a sexp nav whose index is a path
(define (at-path p [m 'seg]) (addr-axis before-sexp-guide after-sexp-guide p m))

(printf "doc: (let (x 1) (+ x 2))\n\n")
(define src ((roper sexp) "(let (x 1) (+ x 2))"))

;; --- address forms directly by path ---
(define z (navigate ((start (at-path '(0))) src)))   (show "form (0):" z)        ; whole list
(set! z (navigate (with-index z '(0 1))))            (show "form (0 1):" z)       ; let
(set! z (navigate (with-index z '(0 2))))            (show "form (0 2):" z)       ; (x 1)
(set! z (navigate (with-index z '(0 2 1))))          (show "form (0 2 1):" z)     ; x

;; --- structural moves: parent / sibling are edits to the path index ---
(set! z (navigate (move z next-sexp-address)))       (show "next sibling:" z)     ; 1
(set! z (navigate (move z parent-sexp-address)))     (show "parent:" z)           ; (x 1)
(set! z (navigate (move z next-sexp-address)))       (show "next sibling:" z)     ; (+ x 2)

;; --- edit at a location: replace the binding's value, then delete the binding ---
(set! z (insert (navigate (with-index z '(0 2 2))) "99"))  (show "set (0 2 2) = 99:" z)
(set! z (delete (navigate (with-index z '(0 2)))))         (show "delete (0 2):" z)

(printf "\nfinal: ~s\n" (text z))
