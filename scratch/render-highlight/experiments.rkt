#lang racket

;; Experiments toward a faster render:
;;   1. allocation cost: building a 4-entry hasheq vs a 4-element vector.
;;   2. a vector-backed bundle (values in a vector + a SHARED smr->index hash) vs the
;;      current hasheq bundle.
;;   3. flat multisect: cut the visible lines in one multisect, reconstruct each line's
;;      (before, after) with a left scan / right scan over the piece summaries.
;;   4. navigate to the block once, then descend per line FROM there (vs from the root).
;;
;; Run:  racket scratch/render-highlight/experiments.rkt

(require "../../rope-core.rkt"
         "../../summaries/summaries.rkt"
         "../../summaries/sexp-summary.rkt"
         "../../zipper-core.rkt"
         "renderer.rkt"
         "highlight.rkt")

(define (ns label iters thunk)
  (thunk)
  (collect-garbage)
  (define-values (_ cpu real gc)
    (time-apply (lambda () (for ([i (in-range iters)]) (thunk))) '()))
  (printf "  ~a ~a ns/op   gc ~a ms\n"
          (~a label #:min-width 28)
          (~a (~r (/ (* real 1e6) iters) #:precision 1) #:min-width 9 #:align 'right) gc))

(define kws   '("define" "lambda" "let" "if" "cond"))
(define kw-smr (make-kw-smr kws))
(define comps (list char-smr kw-smr strsexp-smr linecol-smr))
(define idx   (for/hasheq ([c (in-list comps)] [i (in-naturals)]) (values c i)))

;; ===================== 1. allocation: hasheq vs vector =====================
(printf "=== 1. allocation cost (per op) ===\n")
(ns "for/hasheq (4 entries)" 1000000 (lambda () (for/hasheq ([c (in-list comps)] [i (in-naturals)]) (values c i))))
(ns "hasheq literal (4)"     1000000 (lambda () (hasheq char-smr 1 kw-smr 2 strsexp-smr 3 linecol-smr 4)))
(ns "vector (4)"             1000000 (lambda () (vector 1 2 3 4)))
(ns "vector + shared-idx ref" 1000000 (lambda () (vector-ref (vector 1 2 3 4) (hash-ref idx strsexp-smr))))

;; ===================== 2. vector-backed bundle =====================
;; bvec: component values in a vector, plus the SHARED smr->index hash (same object on
;; every value of this bundle). Combine builds a fresh value-vector positionally and
;; reuses the index hash -- so no per-combine hash construction.
(struct bvec (vals idx)
  #:transparent
  #:methods gen:summary-part
  [(define (part->summary bv smr)
     (define i (hash-ref (bvec-idx bv) smr #f))
     (if i (vector-ref (bvec-vals bv) i) bv))])

(define (make-bundle/vec components)
  (define n    (length components))
  (define cvec (list->vector components))
  (define ix   (for/hasheq ([c (in-list components)] [i (in-naturals)]) (values c i)))
  (make-summary
   (lambda (str) (bvec (build-vector n (lambda (i) ((vector-ref cvec i) str))) ix))
   (lambda (a b)
     (bvec (build-vector n (lambda (i) ((vector-ref cvec i) (vector-ref (bvec-vals a) i)
                                                            (vector-ref (bvec-vals b) i)))) ix))))

(printf "\n=== 2. bundle combine: hasheq vs vector ===\n")
(define hbuf (bundle char-smr kw-smr strsexp-smr linecol-smr))
(define vbuf (make-bundle/vec comps))
(define probe "(define x \"hi\" (f 1))")
(unless (and (equal? (strsexp-smr (vbuf probe)) (strsexp-smr probe))
             (equal? (linecol-smr (vbuf probe)) (linecol-smr probe))
             (equal? (char-smr   (vbuf probe)) (char-smr probe))
             (equal? (vbuf "a" "b") (vbuf "ab")))
  (error "vector bundle reads wrong"))
(printf "  correctness: vector bundle reads == hasheq, folds associatively  ok\n")
(define text "(define (fact n) (if (zero? n) 1 (* n (fact (sub1 n)))))")
(define mid  (quotient (string-length text) 2))
(define (cb smr) (let ([a (smr (substring text 0 mid))] [b (smr (substring text mid))]) (lambda () (smr a b))))
(ns "hasheq bundle combine" 300000 (cb hbuf))
(ns "vector bundle combine" 300000 (cb vbuf))

;; ===================== setup for 3 & 4 =====================
(define h    (make-hl kws))
(define smr  (hl-buf h))
(define snip "(define (fib n)\n  (if (< n 2)\n      n\n      (+ (fib (- n 1))\n         (fib (- n 2)))))\n")
(define doc  (apply string-append (make-list 800 snip)))    ; 4000 lines
(define rope ((make-rope smr) doc))
(define z    (open-doc h doc))

;; ===================== 3. flat multisect + scanl / scanr =====================
;; before(i) = before-block ⊕ pieces[0..i-1]   (left scan)
;; after(i)  = pieces[i+1..] ⊕ after-block      (right scan)
(define (scanl-before s b0 sums)
  (let loop ([acc b0] [ss sums] [out '()])
    (if (null? ss) (reverse out) (loop (s acc (car ss)) (cdr ss) (cons acc out)))))
(define (scanr-after s a0 sums)
  (let loop ([acc a0] [ss (reverse sums)] [out '()])
    (if (null? ss) out (loop (s (car ss) acc) (cdr ss) (cons acc out)))))

(define (heads/flat s rope top rows)
  (define guides (for/vector ([L (in-range top (+ top rows 1))]) (col L 0)))    ; rows+1 cuts
  (define ps (call-with-values (lambda () ((multisect s guides) rope)) list))   ; rows+2 pieces
  (define lines (take (drop ps 1) rows))                                        ; the visible lines
  (define sums  (map s lines))                                                  ; cached, O(1) each
  (values (scanl-before s (s (first ps)) sums) lines (scanr-after s (s (last ps)) sums)))

(printf "\n=== 3. flat multisect + scanl/scanr ===\n")
(let-values ([(bs ls as) (heads/flat smr rope 100 40)])
  (for ([L (in-range 100 140)] [bf (in-list bs)] [lr (in-list ls)] [af (in-list as)])
    (define-values (b m a) (line-head z L))
    (unless (equal? bf b)         (error 'before "line ~a" L))
    (unless (equal? (smr lr) (smr m)) (error 'line "line ~a" L))
    (unless (equal? af a)         (error 'after  "line ~a" L)))
  (printf "  correctness: flat (before,line,after) == per-line line-head  ok\n"))
(define (render-perline) (for ([L (in-range 0 1000)]) (call-with-values (lambda () (line-head z L)) void)))
(define (render-flat)    (let-values ([(bs ls as) (heads/flat smr rope 0 1000)]) (void bs ls as)))
(ns "per-line line-head x1000" 20 render-perline)
(ns "flat multisect+scan x1000" 20 render-flat)

;; ===================== 4. navigate to block, descend per line =====================
(define (block-zipper z top rows) ((zipper-guide (vector (col top 0) (line-end (+ top rows)))) z))
(printf "\n=== 4. descend from root vs from a navigated block ===\n")
(let ([zb (block-zipper z 0 40)])
  (for ([L (in-range 0 40)])
    (define-values (b1 m1 a1) (line-head z  L))
    (define-values (b2 m2 a2) (line-head zb L))
    (unless (and (equal? b1 b2) (equal? (smr m1) (smr m2)) (equal? a1 a2)) (error 'block "line ~a" L)))
  (printf "  correctness: descend-from-block == descend-from-root  ok\n"))
(define (render/root)
  (for ([L (in-range 0 1000)]) (call-with-values (lambda () (line-head z L)) void)))
(define (render/block)
  (define zb (block-zipper z 0 1000))
  (for ([L (in-range 0 1000)]) (call-with-values (lambda () (line-head zb L)) void)))
(ns "descend from root x1000"  20 render/root)
(ns "descend from block x1000" 20 render/block)
