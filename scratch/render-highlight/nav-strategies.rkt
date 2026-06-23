#lang racket

;; A vs A' (vs the current per-line baseline) -- measured, not estimated.
;;
;; The job, for a visible block of `rows` lines: produce (before . focus . after) for each
;; line, where before/after are the summaries of everything outside the line.
;;
;;   baseline : per line, multisect the WHOLE doc at [bol L, bol(L+1)).  rows x 2 bisects
;;              over the full doc (depth ~log N).  (what the renderer does today.)
;;   A'       : multisect once to the block (before-block / block / after-block), then per
;;              line multisect the BLOCK (framed with before-block) at [bol L, bol(L+1)).
;;              rows x 2 bisects over the small block (depth ~log B).
;;   A        : multisect once to the block, then ONE flat multisect of the block into the
;;              rows line ropes (rows-1 guides), then scanl/scanr the cached line summaries
;;              (O(1) combine each) for before/after.  (rows-1) shrinking bisects + a scan.
;;
;; Since multisect (rope-core.rkt:152) is a for/fold of bisects, A and A' do the SAME kind
;; of work; the question is the constant between "rows shrinking 1-guide bisects + a scan"
;; and "rows full-block 2-guide bisects".  This measures it.
;;
;; Run:  racket scratch/render-highlight/nav-strategies.rkt

(require racket/match
         "../../rope-core.rkt"               ; make-rope multisect frame
         "../../summaries/summaries.rkt"     ; bundle char-smr linecol-smr (struct-out linecol)
         "../../summaries/sexp-summary.rkt"  ; strsexp-smr
         "highlight.rkt"                     ; make-kw-smr
         "renderer.rkt")                     ; col line-end line-head open-doc make-hl (struct hl)

(provide heads/baseline heads/Aprime heads/A render-A buf kws kw-smr)

(define kws '("define" "lambda" "let" "if" "cond"))
(define kw-smr (make-kw-smr kws))
(define buf (bundle char-smr kw-smr strsexp-smr linecol-smr))

(define (mvals thunk) (call-with-values thunk list))   ; collect a multisect's values into a list

;; ---------- baseline: per-line multisect over the whole doc ----------
(define (heads/baseline doc top rows)
  (for/list ([L (in-range top (+ top rows))])
    (match-define (list pre foc post)
      (mvals (lambda () ((multisect buf (vector (col L 0) (line-end (+ L 1)))) doc))))
    (list (buf pre) foc (buf post))))

;; ---------- A' : block once, then re-navigate per line inside the block ----------
(define (heads/Aprime doc top rows)
  (match-define (list pre block post)
    (mvals (lambda () ((multisect buf (vector (col top 0) (line-end (+ top rows)))) doc))))
  (define before-block (buf pre))
  (define after-block  (buf post))
  (define framer (frame buf before-block after-block))   ; make block-relative guides judge absolutely
  (for/list ([L (in-range top (+ top rows))])
    (match-define (list bpre foc bpost)
      (mvals (lambda () ((multisect buf (vector (framer (col L 0)) (framer (line-end (+ L 1))))) block))))
    (list (buf before-block (buf bpre)) foc (buf (buf bpost) after-block))))

;; ---------- A : block once, one flat multisect into lines, scan the cached summaries ----------
;; `b` is the bundle to navigate/combine with (defaults to the full buf); pass a lighter
;; bundle to measure the lazy-bundle ceiling (combine only the slots the guides read).
(define (heads/A doc top rows [b buf])
  (match-define (list pre block post)
    (mvals (lambda () ((multisect b (vector (col top 0) (line-end (+ top rows)))) doc))))
  (define before-block (b pre))
  (define after-block  (b post))
  (define framer (frame b before-block after-block))
  (define line-guides (for/vector ([i (in-range 1 rows)]) (framer (col (+ top i) 0))))
  (define line-ropes (mvals (lambda () ((multisect b line-guides) block))))   ; rows pieces
  (define line-smrs  (map b line-ropes))                                      ; cached summaries (O(1) each)
  ;; scanl before, scanr after
  (define befores
    (let loop ([acc before-block] [ss line-smrs] [out '()])
      (if (null? ss) (reverse out) (loop (b acc (car ss)) (cdr ss) (cons acc out)))))
  (define afters
    (let loop ([ss (reverse line-smrs)] [acc after-block] [out '()])
      (if (null? ss) out (loop (cdr ss) (b (car ss) acc) (cons acc out)))))
  (map list befores line-ropes afters))

;; ---------- paint (copied verbatim from renderer.rkt) + a full render via A ----------
(define supers "⁰¹²³⁴⁵⁶⁷⁸⁹")
(define (super n)
  (list->string (for/list ([c (in-string (number->string n))]) (string-ref supers (- (char->integer c) 48)))))
(define (splice s inserts)
  (define sorted (sort inserts (lambda (a b) (or (< (first a) (first b))
                                                 (and (= (first a) (first b)) (< (second a) (second b)))))))
  (let loop ([i 0] [ins sorted] [out '()])
    (match ins
      ['() (apply string-append (reverse (cons (substring s i) out)))]
      [(cons (list at _ mk) rest) (loop at rest (list* mk (substring s i at) out))])))
(define (mark-line text kwS strS pS L sl sc el ec)
  (define len (string-length text))
  (define (ins at pri s) (and (<= 0 at len) (list at pri s)))
  (define raw
    (append
     (append-map (lambda (s) (list (ins (car s) 2 "«") (ins (cdr s) 3 "»"))) kwS)
     (append-map (lambda (s) (list (ins (car s) 2 "‹") (ins (cdr s) 3 "›"))) strS)
     (map (lambda (p) (ins (add1 (car p)) 0 (super (cdr p)))) pS)
     (list (and (= L sl) (ins sc 1 "⟦"))
           (and (= L el) (ins ec 4 "⟧")))))
  (splice text (filter values raw)))
(define (with-cursor z proc)
  (define-values (cs ce) (cursor-span z))
  (match-define (linecol _ sl sc) cs)
  (match-define (linecol _ el ec) ce)
  (proc sl sc el ec))

;; the whole render, but lines come from A's scan instead of per-line zipper line-head.
;; cursor still read once off z; paint identical to renderer's render-line.
(define (render-A h doc z top rows)
  (with-cursor z (lambda (sl sc el ec)
    (for/list ([L (in-range top (+ top rows))] [hd (in-list (heads/A doc top rows))])
      (match-define (list b m a) hd)
      (define-values (fr in? cnt kwS strS pS) (analyze-head (hl-keywords h) (hl-kw-smr h) b m a))
      (define text (string-trim fr "\n" #:left? #f))
      (list L in? (sub1 (length cnt)) (mark-line text kwS strS pS L sl sc el ec))))))

;; ---------- correctness: the three agree, line by line ----------
(define (digest hs)
  (for/list ([h (in-list hs)])
    (match-define (list b foc a) h)
    (list (char-smr b) (linecol-lines (linecol-smr b)) (linecol-cols (linecol-smr b))
          (~a foc) (char-smr a))))

(module+ main
  (define snip "(define (fib n)\n  (if (< n 2)\n      n\n      (+ (fib (- n 1))\n         (fib (- n 2)))))\n")
  (define big  (apply string-append (make-list 800 snip)))   ; 4000 lines
  (define doc  ((make-rope buf) big))
  (define h    (hl kws kw-smr buf))      ; share ONE buf across nav (doc) and zipper (z)
  (define z    (open-doc h big))

  ;; --- correctness ---
  (for ([top (in-list '(0 1000 2000))] [rows (in-list '(50 50 50))])
    (define base (digest (heads/baseline doc top rows)))
    (unless (and (equal? base (digest (heads/Aprime doc top rows)))
                 (equal? base (digest (heads/A doc top rows))))
      (error 'correctness "disagree at top=~a" top)))
  (printf "correctness: baseline == A' == A  (before/focus/after agree, every line)  ok\n\n")

  (define (ns label rows iters thunk)
    (thunk) (collect-garbage)
    (define-values (_ c r g) (time-apply (lambda () (for ([i (in-range iters)]) (thunk))) '()))
    (printf "  ~a ~a ms/screen  ~a us/line   gc ~a\n"
            (~a label #:min-width 26)
            (~a (~r (/ (exact->inexact r) iters) #:precision 2) #:min-width 7 #:align 'right)
            (~a (~r (/ (* r 1000.0) iters rows) #:precision 1) #:min-width 7 #:align 'right) g))

  (define top 2000)
  (for ([rows (in-list '(50 100))])
    (printf "block of ~a lines at line ~a (4000-line doc):\n" rows top)
    (ns "baseline (per-line/doc)" rows 200 (lambda () (heads/baseline doc top rows)))
    (ns "A' (block + re-navigate)" rows 200 (lambda () (heads/Aprime  doc top rows)))
    (ns "A  (block + flat + scan)" rows 200 (lambda () (heads/A       doc top rows)))
    (newline))

  ;; the actual current renderer (zipper line-head) navigation, for reference
  (printf "reference -- zipper line-head (the live renderer's navigation), 50 lines:\n")
  (ns "zipper line-head" 50 200 (lambda () (for ([L (in-range top (+ top 50))]) (line-head z L))))

  ;; ===== the real question: a FULL render (nav + paint) via A vs the live renderer =====
  (newline)
  (for ([rows (in-list '(50 100))])
    (unless (equal? (render h z top rows) (render-A h doc z top rows))
      (error 'render "render != render-A at rows=~a" rows))
    (printf "FULL render (nav + paint), ~a lines  --  render == render-A  ok:\n" rows)
    (ns "current render (zipper)" rows 100 (lambda () (render   h z   top rows)))
    (ns "render via A"           rows 100 (lambda () (render-A h doc z top rows)))
    (newline))

  ;; ===== lazy-bundle ceiling: A navigation combining only the slots guides read =====
  ;; A lazy bundle's best case is to never combine strsexp/kw during nav (guides read only
  ;; char+linecol).  That ceiling = navigating with a char+linecol bundle.  Difference vs
  ;; the full 4-comp bundle = the wasted strsexp+kw combine work a lazy bundle could skip.
  (define buf-lite (bundle char-smr linecol-smr))
  (define doc-lite ((make-rope buf-lite) big))
  ;; line boundaries / focus agree between full and lite navigation (guides read the same slots)
  (unless (equal? (map (lambda (h) (~a (second h))) (heads/A doc 2000 50))
                  (map (lambda (h) (~a (second h))) (heads/A doc-lite 2000 50 buf-lite)))
    (error "full vs lite navigation focus disagree"))
  (printf "lazy-bundle ceiling -- A navigation (50 lines), full bundle vs char+linecol:\n")
  (ns "A nav, full bundle (4 comp)" 50 300 (lambda () (heads/A doc      2000 50)))
  (ns "A nav, lite (char+linecol)"  50 300 (lambda () (heads/A doc-lite 2000 50 buf-lite))))
