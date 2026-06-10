#lang racket

;; Sexp navigation + editing on the summarised rope, on ONE unified, SIGNED address scheme.
;;
;; A cursor address is  (cons (vector s e) rest):
;;   rest         — the shared outer path (innermost-first)
;;   (vector s e) — the innermost signed indices (s the start edge, e the end edge)
;;
;; The SIGN of an index is its anchor basis -- which edge of the inter-form whitespace it
;; leans to.  +p and -p name the two edges of the same structural slot p:
;;   p >  0   -> END of form (p-1)    (the slot's LEFT edge: "after p forms", before the whitespace)
;;   p <  0   -> START of form (-p)   (the slot's RIGHT edge: just before that form, after the whitespace)
;;   p =  0   -> START of form 0      (the frame's left end)
;; So +p and -p are the two anchors of one slot (end of form p-1 vs start of form p), and `flip`
;; is just negation.  A tight single form p is (vector -p (p+1)): its own start and its own end.
;; A gap is s = e (an empty focus), left- or right-leaning by the shared sign.
;;
;; A guide is a comparator (L R) -> {-1,0,1}: +1 if the target boundary is right of the cut,
;; -1 left, 0 at it.  before-/after-sexp-guide are ported from the deprecated locator work; the
;; one adaptation is that the current `sexp` summary stores `opens` INNERMOST-first, so
;; `sexp-next-address` reverses it.  No summary change was needed -- the 7-list already carries
;; ends-form?/ends-atom?, which the end (after) guide reads.

(require racket/match
         "rope-core.rkt"        ; multisect -- plus the rope surface, re-exported below
         "sexp-summary.rkt"     ; sexp  sexp-opens sexp-closes sexp-forms
         "zipper-core.rkt")     ; start navigate to-root peek + the editing verbs

(provide before-sexp-guide after-sexp-guide index->guide sexp-guides
         cursor carve focus-string
         base-left base-right flip
         (all-from-out "rope-core.rkt")
         (all-from-out "sexp-summary.rkt")
         (all-from-out "zipper-core.rkt"))   ; insert/replace/delete/to-root/peek come from here

;; ---------- summary readers (the 7-list: sa? sf? closes forms opens za? zf?) ----------
(define (s-starts-atom? s) (and s (list-ref s 0)))
(define (s-starts-form? s) (and s (list-ref s 1)))
(define (s-forms        s) (if s (list-ref s 3) 0))
(define (s-opens        s) (if s (list-ref s 4) '()))   ; innermost-first
(define (s-ends-atom?   s) (and s (list-ref s 5)))
(define (s-ends-form?   s) (and s (list-ref s 6)))

;; ---------- address arithmetic (paths are outermost-first) ----------
(define (drop-trailing-zeros path)
  (define trimmed
    (let loop ([rev (reverse path)])
      (match rev [(cons 0 rest) (loop rest)] [_ (reverse rev)])))
  (if (null? trimmed) '(0) trimmed))

(define (next-sexp-address path)
  (match (drop-trailing-zeros path)
    [(list i) (list (add1 i))]
    [(cons i rest) (cons i (next-sexp-address rest))]))

(define (sexp-path-compare x y)
  (let cmp ([x (drop-trailing-zeros x)] [y (drop-trailing-zeros y)])
    (match* (x y)
      [('() '()) 0] [('() _) -1] [(_ '()) 1]
      [((cons a as) (cons b bs)) (cond [(< a b) -1] [(> a b) 1] [else (cmp as bs)])])))

;; the address the cut currently sits at, read off the LEFT total summary.
(define (sexp-next-address before)
  (if (not before)
      '(0)
      (let ([forms (s-forms before)]
            [opens (reverse (s-opens before))])    ; reverse: innermost-first -> outermost-first
        (define (opens->addr o)
          (match o
            ['() '()]
            [(list inner) (list (add1 inner))]
            [(cons k rest) (cons k (opens->addr rest))]))
        (cons forms (opens->addr opens)))))

;; ---------- the two structural guides (convention: +1 boundary right of cut) ----------
(define ((before-sexp-guide path) before after)        ; the START of the form at `path`
  (case (sexp-path-compare (sexp-next-address before) path)
    [(-1) 1]
    [(1) -1]
    [(0) (if (and (s-starts-form? after)
                  (not (and (s-ends-atom? before) (s-starts-atom? after))))
             0 1)]))

(define ((after-sexp-guide path) before after)         ; the END of the form at `path`
  (define next-path (next-sexp-address path))
  (case (sexp-path-compare (sexp-next-address before) next-path)
    [(-1) 1]
    [(1) -1]
    [(0) (cond [(and (s-ends-atom? before) (s-starts-atom? after)) 1]
               [(s-ends-form? before) 0]
               [else -1])]))

;; ---------- the sign IS the anchor basis (reversed): + binds the sexp END, - the START ----------
;; +p -> END of form p-1  (left edge of slot p, "after p forms")
;; -p -> START of form p   (right edge of slot p, "before form p")
;;  0 -> START of form 0   (the frame's left end)
(define (index->guide ix rest)
  (cond
    [(positive? ix) (after-sexp-guide  (reverse (cons (- ix 1) rest)))]
    [(negative? ix) (before-sexp-guide (reverse (cons (- ix)   rest)))]
    [else           (before-sexp-guide (reverse (cons 0       rest)))]))

(define (sexp-guides rep)                                ; (cons (vector s e) rest) -> (vector gs ge)
  (match-define (cons (vector s e) rest) rep)
  (vector (index->guide s rest) (index->guide e rest)))

;; ---------- the cursor: navigate the persistent zipper to a rep ----------
(define (cursor rope rep) ((navigate (sexp-guides rep)) (start sexp rope)))   ; -> a zipper

(define (focus-string rope rep)
  (let-values ([(b m a) (peek (cursor rope rep))]) (~a m)))

;; carve: before | focus | after, as strings, for inspection.
(define (carve rope rep)
  (let-values ([(l m r) ((multisect (sexp-guides rep)) rope)])
    (values (~a l) (~a m) (~a r))))

;; editing is the zipper's job now -- navigate with `cursor`, apply insert/replace/delete
;; (re-exported from zipper-core), then peek at (to-root ...) to read the whole document back.

;; ---------- anchor / gravity: read the slot off the cursor's summary, pick a side ----------
;; the structural slot has k_left forms to its left; +k_left leans LEFT (start of the next
;; form), -k_left leans RIGHT (end of the previous form).  flip toggles -- pure negation,
;; since +p and -p are the two anchors of the same slot.
(define (k-left before) (if (null? (s-opens before)) (s-forms before) (car (s-opens before))))

(define (cursor-before z) (let-values ([(b m a) (peek z)]) b))   ; the gap's left summary

(define (base-left  z) (k-left (cursor-before z)))       ; +p: start of the next form
(define (base-right z) (- (base-left z)))                ; -p: end of the previous form
(define (flip ix) (- ix))                                ; involution; toggles the anchor side
