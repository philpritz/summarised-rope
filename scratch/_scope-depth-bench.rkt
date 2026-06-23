#lang racket
;; THROWAWAY: scope-summary cost vs NESTING DEPTH.
;; Generator = depth-d nested lets, each binding a distinct name; the cut sits in the
;; innermost body, so all d names are in scope there. Token-aligned fold (sidesteps the
;; case-B seam gap), so this isolates the monoid's depth cost: read + one balanced combine.
(require "scope-summary.rkt" "../rope-core.rkt")

(define (ws-chunks s) (regexp-match* #px"\\S+\\s*|\\s+" s))
(define (fold pieces) (foldl (lambda (c a) (scope+ a (scope-leaf c))) (scope-leaf "") pieces))

;; the full nested form (for display)
(define (nested d)
  (let build ([i 0])
    (if (= i d)
        (string-append "(+ " (string-join (for/list ([j (in-range d)]) (format "a~a" j)) " ") ")")
        (format "(let ([a~a ~a]) ~a)" i i (build (add1 i))))))

;; just the openers, ending at the innermost-body cut; and the matching closers
(define (openers d) (apply string-append (for/list ([i (in-range d)]) (format "(let ([a~a ~a]) " i i))))
(define (closers d) (make-string d #\)))

(printf "generator, depth 3:\n  ~s\n\n" (nested 3))

(define (ns iters thunk)
  (thunk) (collect-garbage)
  (define-values (_ cpu real gc) (time-apply (lambda () (for ([i (in-range iters)]) (thunk))) '()))
  (/ (* real 1e6) iters))                       ; ns/op

(printf "scope cost vs nesting depth (cut in innermost body, all d names live):\n")
(printf "~a ~a ~a ~a ~a\n"
        (~a "depth" #:min-width 7) (~a "names" #:min-width 7) (~a "chars" #:min-width 8)
        (~a "read ns" #:min-width 10) (~a "combine ns" #:min-width 12))
(for ([d (in-list '(1 2 4 8 16 32 64 128 256 512))])
  (define pre  (openers d))
  (define Lsum (fold (ws-chunks pre)))           ; the cut summary (built untimed)
  (define Rsum (scope-leaf (closers d)))         ; d closers -> a depth-d unwind combine
  (define names (length (sv-in-scope Lsum)))
  (define rd  (ns 200000 (lambda () (sv-in-scope Lsum))))
  (define cmb (ns 200000 (lambda () (scope+ Lsum Rsum))))
  (printf "~a ~a ~a ~a ~a\n"
          (~a d #:min-width 7) (~a names #:min-width 7) (~a (string-length pre) #:min-width 8)
          (~a (~r rd #:precision 1) #:min-width 10) (~a (~r cmb #:precision 1) #:min-width 12)))
