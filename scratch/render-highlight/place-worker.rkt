#lang racket
;; A standalone worker module for dynamic-place (no main -> cannot fork-bomb the driver).
(provide go)
(define (go ch) (place-channel-put ch 'up))
