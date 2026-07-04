#lang racket/base

;; toolbox: the reusable, project-independent layer -- isos/opts and the value/list
;; combinators (algebra.rkt) plus the persistent iso-deque (deque.rkt). This
;; aggregator re-exports both, so a consumer writes (require "toolbox") for the
;; whole kit; require an individual file directly to pull just one part.
;; Enumerated by hand -- add a re-export line when a file joins the folder.

(require "algebra.rkt" "deque.rkt")
(provide (all-from-out "algebra.rkt" "deque.rkt"))
