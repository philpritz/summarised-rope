#lang racket

;; A/B for step #1: a VECTOR bundle vs the current `for/hasheq` bundle.
;;
;; The current bundle (summaries.rkt) keys its slots by smr identity in an immutable
;; hasheq. Per combine that costs: a `for/hasheq` build (~one 4-entry hash) PLUS, for
;; each component, TWO `part->summary` hash probes (one per side) to pull that
;; component's slot out of the two operand bundle-vals.
;;
;; The vector bundle fixes a component ORDER once. Slots are a plain vector in that
;; order; combine walks i = 0..n-1 and calls the component smr directly on the two
;; raw slot values `(c (vector-ref sa i) (vector-ref sb i))` -- no hash build, no
;; part->summary, the position IS the key. Reads still need the smr->index map (one
;; probe), same as the hash bundle's read; reads are not the combine bottleneck.
;;
;; Calling the component smr on RAW slot values is cheap: none of the component values
;; (integer / linecol / cs / sexp-val) implement gen:summary-part, so each smr's coerce
;; hits its `[_ x]` fall-through (a few predicate checks), not a hash probe.
;;
;; Run:  racket scratch/render-highlight/vbundle.rkt

(require "../../rope-core.rkt"               ; make-summary gen:summary-part part->summary make-rope
         "../../summaries/summaries.rkt"     ; bundle char-smr linecol-smr
         "../../summaries/sexp-summary.rkt"  ; strsexp-smr
         "highlight.rkt"                     ; make-kw-smr
         "renderer.rkt")                     ; open-doc render line-head col (struct hl) -- real-render impact

(provide make-vbundle (struct-out vbundle-val))

;; ---------- the vector bundle ----------
(struct vbundle-val (slots index)          ; slots : (vectorof value) ; index : #hasheq(smr -> i), shared
  #:transparent
  #:methods gen:summary-part
  [(define (part->summary bv smr)
     (define i (hash-ref (vbundle-val-index bv) smr #f))
     (if i (vector-ref (vbundle-val-slots bv) i) bv))])

(define (make-vbundle . components)
  (define n     (length components))
  (define comps (list->vector components))
  (define index (for/hasheq ([c (in-list components)] [i (in-naturals)]) (values c i)))
  (make-summary
   (lambda (str) (vbundle-val (build-vector n (lambda (i) ((vector-ref comps i) str))) index))
   (lambda (a b)
     (define sa (vbundle-val-slots a))
     (define sb (vbundle-val-slots b))
     (vbundle-val (build-vector n (lambda (i) ((vector-ref comps i) (vector-ref sa i) (vector-ref sb i)))) index))))

;; ---------- the two bundles over the same 4 components ----------
(define kw-smr (make-kw-smr '("define" "lambda" "let" "if" "cond")))
(define comps  (list char-smr kw-smr strsexp-smr linecol-smr))
(define hbuf (bundle      char-smr kw-smr strsexp-smr linecol-smr))   ; hash bundle
(define vbuf (apply make-vbundle comps))                              ; vector bundle

;; ---------- correctness: same leaves, same slot reads, same combine ----------
(module+ test
  (require rackunit)
  (define s1 "(define (fact n) (if (zero")
  (define s2 "? n) 1 (* n (fact (sub1 n)))))")
  ;; leaf agreement: every slot read matches between the two bundles
  (for ([c (in-list comps)])
    (check-equal? (c (vbuf s1)) (c (hbuf s1)) (format "leaf slot ~a" (object-name c))))
  ;; combine agreement: read each slot off (buf a b) for both bundles
  (define hc (hbuf (hbuf s1) (hbuf s2)))
  (define vc (vbuf (vbuf s1) (vbuf s2)))
  (for ([c (in-list comps)])
    (check-equal? (c vc) (c hc) (format "combine slot ~a" (object-name c))))
  ;; reads through a real rope agree too (multisect drives the combine)
  (define hr ((make-rope hbuf) "(define x\ny)"))
  (define vr ((make-rope vbuf) "(define x\ny)"))
  (for ([c (in-list comps)])
    (check-equal? (c vr) (c hr) (format "rope slot ~a" (object-name c)))))

;; ---------- bench ----------
(module+ main
  (require rackunit)
  ;; run the correctness submodule first so a regression aborts the bench
  (define s1 "(define (fact n) (if (zero")
  (define s2 "? n) 1 (* n (fact (sub1 n)))))")
  (define hc (hbuf (hbuf s1) (hbuf s2)))
  (define vc (vbuf (vbuf s1) (vbuf s2)))
  (for ([c (in-list comps)]) (check-equal? (c vc) (c hc)))
  (printf "correctness: vector bundle == hash bundle (slot reads agree)  ok\n\n")

  (define (ns label iters thunk)
    (thunk) (collect-garbage)
    (define-values (_ c r g) (time-apply (lambda () (for ([i (in-range iters)]) (thunk))) '()))
    (printf "  ~a ~a ns/op   gc ~a ms\n"
            (~a label #:min-width 30)
            (~a (~r (/ (* r 1e6) iters) #:precision 1) #:min-width 8 #:align 'right) g))

  (define N 1000000)
  ;; pre-built operands for the combine bench (bundle-vals, as multisect would hand them)
  (define ha (hbuf s1)) (define hb (hbuf s2))
  (define va (vbuf s1)) (define vb (vbuf s2))

  (printf "combine  (buf a b):\n")
  (ns "hash bundle  (for/hasheq)" N (lambda () (hbuf ha hb)))
  (ns "vector bundle"             N (lambda () (vbuf va vb)))

  (printf "\nleaf  (buf str):\n")
  (ns "hash bundle"   N (lambda () (hbuf s1)))
  (ns "vector bundle" N (lambda () (vbuf s1)))

  (printf "\nread one slot  (linecol-smr (buf a b)):\n")
  (ns "hash bundle"   N (lambda () (linecol-smr hc)))
  (ns "vector bundle" N (lambda () (linecol-smr vc)))

  ;; ---------- the real question: does the combine win move the RENDER number? ----------
  ;; Same renderer, same doc -- only the buffer bundle differs (hash vs vector).
  (define kws  '("define" "lambda" "let" "if" "cond"))
  (define snip "(define (fib n)\n  (if (< n 2)\n      n\n      (+ (fib (- n 1))\n         (fib (- n 2)))))\n")
  (define big  (apply string-append (make-list 800 snip)))      ; 4000 lines
  (define h-hash (hl kws kw-smr (bundle char-smr kw-smr strsexp-smr linecol-smr)))
  (define h-vec  (hl kws kw-smr (apply make-vbundle comps)))
  (define z-hash (open-doc h-hash big))
  (define z-vec  (open-doc h-vec  big))
  ;; renders must agree
  (unless (equal? (render h-hash z-hash 0 60) (render h-vec z-vec 0 60))
    (error "hash vs vector render disagree"))
  (printf "\nrender correctness: hash == vector  ok\n")

  (printf "\nrope build  ((make-rope buf) big):\n")
  (ns "hash bundle"   20 (lambda () ((make-rope (hl-buf h-hash)) big)))
  (ns "vector bundle" 20 (lambda () ((make-rope (hl-buf h-vec )) big)))

  (printf "\nline-head (nav+head, mid-doc):\n")
  (ns "hash bundle"   3000 (lambda () (line-head z-hash 2000)))
  (ns "vector bundle" 3000 (lambda () (line-head z-vec  2000)))

  (printf "\nrender 1 line (mid-doc):\n")
  (ns "hash bundle"   3000 (lambda () (render h-hash z-hash 2000 1)))
  (ns "vector bundle" 3000 (lambda () (render h-vec  z-vec  2000 1))))
