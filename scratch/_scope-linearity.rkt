#lang racket
;; THROWAWAY: what is the scope READ actually linear in? Three ways to grow the document:
;;   (1) DEPTH   nested narrow lets   -- N levels x 1 name  -> in-scope = N
;;   (2) WIDTH   one wide let         -- 1 level  x N names  -> in-scope = N
;;   (3) INERT   non-binding bloat    -- fixed depth, pad with (g a b) forms that EXIT
;; If the read tracks in-scope NAME COUNT (not depth, not raw size), (1)==(2) and (3) is flat.
(require "scope-summary.rkt" "../rope-core.rkt")

(define (ws-chunks s) (regexp-match* #px"\\S+\\s*|\\s+" s))
(define (fold pieces) (foldl (lambda (c a) (scope+ a (scope-leaf c))) (scope-leaf "") pieces))
(define (ns iters thunk)
  (thunk) (collect-garbage)
  (define-values (_ cpu real gc) (time-apply (lambda () (for ([i (in-range iters)]) (thunk))) '()))
  (/ (* real 1e6) iters))

(define (nested d) (apply string-append (for/list ([i (in-range d)]) (format "(let ([a~a ~a]) " i i))))
(define (wide  n) (string-append "(let (" (apply string-append (for/list ([i (in-range n)]) (format "[a~a ~a] " i i))) ") "))
(define (padded d pad) (string-append (apply string-append (make-list pad "(g a b) ")) (nested d)))

(define (read-ns prefix)
  (define summ (fold (ws-chunks prefix)))
  (values (length (sv-in-scope summ)) (string-length prefix)
          (ns 100000 (lambda () (sv-in-scope summ)))))

(printf "(1) DEPTH vs (2) WIDTH -- same N names, reached two ways:\n")
(printf "~a  ~a ~a  ~a ~a\n" (~a "N" #:min-width 5)
        (~a "depth:names" #:min-width 11) (~a "read ns" #:min-width 9)
        (~a "width:names" #:min-width 11) (~a "read ns" #:min-width 9))
(for ([N (in-list '(1 2 4 8 16 32 64 128 256))])
  (define-values (dn _dc dr) (read-ns (nested N)))
  (define-values (wn _wc wr) (read-ns (wide  N)))
  (printf "~a  ~a ~a  ~a ~a\n" (~a N #:min-width 5)
          (~a dn #:min-width 11) (~a (~r dr #:precision 1) #:min-width 9)
          (~a wn #:min-width 11) (~a (~r wr #:precision 1) #:min-width 9)))

(printf "\n(3) INERT bloat -- depth fixed at 8, pad with non-binding forms that exit:\n")
(printf "~a  ~a ~a  ~a\n" (~a "pad" #:min-width 6) (~a "chars" #:min-width 8) (~a "names" #:min-width 6) (~a "read ns" #:min-width 9))
(for ([pad (in-list '(0 50 500 5000 50000))])
  (define-values (n c r) (read-ns (padded 8 pad)))
  (printf "~a  ~a ~a  ~a\n" (~a pad #:min-width 6) (~a c #:min-width 8) (~a n #:min-width 6) (~a (~r r #:precision 1) #:min-width 9)))
