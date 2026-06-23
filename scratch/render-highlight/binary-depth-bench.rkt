#lang racket

;; A balanced binary sexp ((...) (...)) to depth d, built by DOUBLING with structure
;; sharing -- the rope-(rope b b) trick from rope-core's `spine` test:
;;   b_0 = "x";   b_k = (make-rope buf "(" b_{k-1} "\n" b_{k-1} ")")
;; the SAME subrope b_{k-1} is passed twice, so building depth d is ~O(d^2) work / nodes
;; even though the text it represents is 2^d lines deep-nested d parens.  Lets us reach
;; large nesting depths without materializing the (exponential) string, and watch how the
;; nav strategies scale as paren depth (= descent depth here) grows.
;;
;; Run:  racket scratch/render-highlight/binary-depth-bench.rkt

(require racket/match
         "../../rope-core.rkt"               ; make-rope
         "../../summaries/summaries.rkt"     ; bundle char-smr linecol-smr (struct-out linecol)
         "renderer.rkt"                      ; render open-doc (struct hl)
         "nav-strategies.rkt"                ; heads/A render-A buf kws kw-smr
         (submod "../../rope-core.rkt" internal))   ; rope-height (characterize the shared tree)

;; the doubling builder, parameterized by bundle (full buf, or a lite char+linecol one)
(define (binary-sexp d [b buf])
  (let loop ([k 0] [acc ((make-rope b) "x")])
    (if (= k d) acc (loop (add1 k) ((make-rope b) "(" acc "\n" acc ")")))))

(define (lines rope) (add1 (linecol-lines (linecol-smr rope))))

(define (ns label rows iters thunk)
  (thunk) (collect-garbage)
  (define-values (_ c r g) (time-apply (lambda () (for ([i (in-range iters)]) (thunk))) '()))
  (printf "    ~a ~a ms/screen  ~a us/line\n"
          (~a label #:min-width 24)
          (~a (~r (/ (exact->inexact r) iters) #:precision 2) #:min-width 7 #:align 'right)
          (~a (~r (/ (* r 1000.0) iters rows) #:precision 1) #:min-width 7 #:align 'right)))

(module+ main
  (define buf-lite (bundle char-smr linecol-smr))
  (printf "binary ((...)(...)) doubling sexp -- 50-line block at the middle, by depth:\n")
  (printf "(depth d => 2^d lines, max paren depth d, balanced rope height ~~ d)\n\n")
  (for ([d (in-list '(8 12 16 18 20))])
    (define-values (rope bc br bg) (time-apply binary-sexp (list d)))
    (define rope*     (first rope))                 ; time-apply wraps results in a list
    (define rope-lite (binary-sexp d buf-lite))
    (define n    (lines rope*))
    (define top  (max 0 (- (quotient n 2) 25)))
    (define rows 50)
    (define h    (hl kws kw-smr buf))
    (define z    (open-doc h rope*))
    (unless (equal? (render h z top rows) (render-A h rope* z top rows))
      (error 'bench "render != render-A at depth ~a" d))
    (printf "depth ~a: ~a lines, rope-height ~a, built in ~a ms  (render==render-A ok)\n"
            d n (rope-height rope*) br)
    (ns "current render (zipper)" rows 40  (lambda () (render   h z     top rows)))
    (ns "render via A"            rows 60  (lambda () (render-A h rope* z top rows)))
    (ns "A nav, full bundle"      rows 100 (lambda () (heads/A rope*     top rows)))
    (ns "A nav, lite (char+lcol)" rows 100 (lambda () (heads/A rope-lite top rows buf-lite)))
    (newline)))
