#lang racket
;; Spawn cost of a place via dynamic-place + a separate worker module (no fork bomb).
(require racket/place)
(define (ms) (current-inexact-milliseconds))
(module+ main
  (printf "place-enabled?: ~a\n" (place-enabled?)) (flush-output)
  (for ([i (in-range 6)])
    (define t0 (ms))
    (define p (dynamic-place "scratch/render-highlight/place-worker.rkt" 'go))
    (place-channel-get p)               ; wait for 'up
    (define t1 (ms))
    (place-wait p)
    (printf "place ~a: spawned + first msg in ~a ms\n" i (~r (- t1 t0) #:precision 1))
    (flush-output)))
