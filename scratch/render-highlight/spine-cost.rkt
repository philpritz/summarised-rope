#lang racket

;; How expensive is the deep sexp SPINE (the ~d-entry opens/closes lists at depth d)?
;; Linear (((...))) nesting => a cut at line k carries a k-deep opens stack.  We measure:
;;   1. one deep combine (cancel d opens against d closes) vs depth -- is it O(d)?
;;   2. multisect + rejoin the depth-4000 rope -- cost, and does it balloon?
;;   3. how MANY combines an op triggers (so: per navigation / per edit, how often deep?)
;;
;; Run:  racket scratch/render-highlight/spine-cost.rkt

(require racket/match
         "../../rope-core.rkt"               ; make-rope multisect frame
         "../../summaries/summaries.rkt"     ; bundle char-smr linecol-smr (struct-out linecol)
         "../../summaries/sexp-summary.rkt"  ; strsexp-smr
         (submod "../../rope-core.rkt" internal))   ; rope-leaves rope-height

(define buf (bundle char-smr strsexp-smr linecol-smr))

;; guides (inlined)
(define ((col line k) L R)
  (match-define (linecol _ ll lc) (linecol-smr L))
  (cond [(not (= ll line)) (if (< ll line) 1 -1)] [(< lc k) 1] [(> lc k) -1] [else 0]))
(define ((line-end line) L R) (if (zero? (char-smr R)) 0 ((col line 0) L R)))

;; linear nesting: d opens, x, d closes  (2d+1 lines, O(d) to build)
(define (linear-sexp d b)
  (let loop ([k 0] [acc ((make-rope b) "x")])
    (if (= k d) acc (loop (add1 k) ((make-rope b) "(\n" acc "\n)")))))

;; counting bundle: counts binary combines, else delegates to buf (used only to drive
;; multisect/rejoin so reads still resolve through buf's component keys)
(define combines 0)
(define (counting-buf . args)
  (when (= (length args) 2) (set! combines (add1 combines)))
  (apply buf args))
(define (count-reset!) (set! combines 0))

(define (mvals thunk) (call-with-values thunk list))
(define (timed label iters thunk)
  (thunk) (collect-garbage)
  (define-values (_ c r g) (time-apply (lambda () (for ([i (in-range iters)]) (thunk))) '()))
  (printf "  ~a ~a us/op\n" (~a label #:min-width 34)
          (~a (~r (/ (* r 1000.0) iters) #:precision 2) #:min-width 9 #:align 'right)))

(module+ main
  ;; ---------- 1. one deep combine: cancel d opens against d closes ----------
  (printf "one strsexp combine of a d-deep opens spine with a d-deep closes spine:\n")
  (for ([d (in-list '(250 500 1000 2000 4000))])
    (define L (strsexp-smr ((make-rope buf) (apply string-append (make-list d "(\n")))))
    (define R (strsexp-smr ((make-rope buf) (apply string-append (make-list d "\n)")))))
    (timed (format "depth ~a  (strsexp-smr L R)" d) 100000 (lambda () (strsexp-smr L R))))

  ;; ---------- 2. multisect + rejoin the depth-4000 rope ----------
  (printf "\nbuild + operate on the depth-4000 linear rope:\n")
  (define D 4000)
  (define rope (linear-sexp D buf))
  (printf "  rope: ~a lines, ~a leaves, height ~a (no balloon: leaves ~~ lines/maxleaf)\n"
          (add1 (linecol-lines (linecol-smr rope))) (rope-leaves rope) (rope-height rope))
  (define deep-cut (vector (col D 0) (line-end (add1 D))))      ; cut at the innermost x (line D)

  ;; multisect at the deep cut -> 3 pieces; count combines for ONE multisect
  (count-reset!)
  (match-define (list pre mid post) (mvals (lambda () ((multisect counting-buf deep-cut) rope))))
  (define ms-combines combines)
  (timed "multisect at deep cut (split)" 2000 (lambda () ((multisect buf deep-cut) rope)))
  (printf "    -> combines per multisect: ~a   (pieces: ~a + ~a + ~a leaves)\n"
          ms-combines (rope-leaves pre) (rope-leaves mid) (rope-leaves post))

  ;; rejoin the 3 pieces; count combines for ONE rejoin
  (count-reset!)
  (define rejoined ((make-rope counting-buf) pre mid post))
  (define rj-combines combines)
  (timed "rejoin the 3 pieces" 2000 (lambda () ((make-rope buf) pre mid post)))
  (printf "    -> combines per rejoin: ~a   result: ~a leaves, height ~a  (round-trips: ~a)\n"
          rj-combines (rope-leaves rejoined) (rope-height rejoined)
          (equal? (strsexp-smr rejoined) (strsexp-smr rope)))

  ;; ---------- 3. isolate the spine: same line count, shallow vs deep ----------
  (printf "\nsame ~~8000 lines, multisect at the middle -- shallow (depth 1) vs deep (depth 4000):\n")
  (define shallow ((make-rope buf) (apply string-append (make-list 8000 "(a b c)\n"))))
  (timed "shallow doc, cut at line 4000" 2000
         (lambda () ((multisect buf (vector (col 4000 0) (line-end 4001))) shallow)))
  (timed "deep doc,    cut at line 4000" 2000
         (lambda () ((multisect buf (vector (col 4000 0) (line-end 4001))) rope)))

  ;; multisect cost vs cut DEPTH (cut at line k => k-deep stack)
  (printf "\nmultisect cost vs cut depth (linear rope, cut at line k => k unclosed opens):\n")
  (for ([k (in-list '(10 100 1000 2000 4000))])
    (timed (format "cut at line ~a" k) 2000
           (lambda () ((multisect buf (vector (col k 0) (line-end (add1 k))) ) rope)))))
