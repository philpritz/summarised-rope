#lang racket

;; Lazy vs strict bundle, focused on the SEXP COMBINE -- the component whose cost grows
;; with nesting depth (longer opens/closes stacks).  Two questions:
;;   1. how much does the strsexp combine itself cost as paren depth grows?  (microbench)
;;   2. does a LAZY bundle (char/linecol eager, strsexp deferred behind a promise) let
;;      navigation skip that combine?  Guides read only char/linecol, so a deferred
;;      strsexp slot is never forced during nav -- its combine is never run.
;;
;; Self-contained: only rope-core + summaries (NO zipper/renderer/highlight), so it is
;; independent of the in-flight lens refactor.  Docs are the balanced binary ((...)(...))
;; doubling sexp (depth d => 2^d lines, paren depth d), built with structure sharing.
;;
;; Run:  racket scratch/render-highlight/lazy-bench.rkt

(require racket/match
         "../../rope-core.rkt"               ; make-rope make-summary multisect frame gen:summary-part part->summary
         "../../summaries/summaries.rkt"     ; bundle char-smr linecol-smr (struct-out linecol)
         "../../summaries/sexp-summary.rkt") ; strsexp-smr

;; ---------- line/column guides (inlined from renderer, no zipper) ----------
(define ((col line k) L R)
  (match-define (linecol _ ll lc) (linecol-smr L))
  (cond [(not (= ll line)) (if (< ll line) 1 -1)]
        [(< lc k) 1] [(> lc k) -1] [else 0]))
(define ((line-end line) L R)
  (if (zero? (char-smr R)) 0 ((col line 0) L R)))

;; ---------- A's navigation (inlined from nav-strategies), parameterized by bundle ----------
(define (mvals thunk) (call-with-values thunk list))
(define (heads/A doc top rows b)
  (match-define (list pre block post)
    (mvals (lambda () ((multisect b (vector (col top 0) (line-end (+ top rows)))) doc))))
  (define before-block (b pre))
  (define after-block  (b post))
  (define framer (frame b before-block after-block))
  (define line-guides (for/vector ([i (in-range 1 rows)]) (framer (col (+ top i) 0))))
  (define line-ropes (mvals (lambda () ((multisect b line-guides) block))))
  (define line-smrs  (map b line-ropes))
  (define befores
    (let loop ([acc before-block] [ss line-smrs] [out '()])
      (if (null? ss) (reverse out) (loop (b acc (car ss)) (cdr ss) (cons acc out)))))
  (define afters
    (let loop ([ss (reverse line-smrs)] [acc after-block] [out '()])
      (if (null? ss) out (loop (cdr ss) (b (car ss) acc) (cons acc out)))))
  (map list befores line-ropes afters))

;; ---------- a per-component-lazy bundle ----------
(struct lazy-bv (slots index)
  #:methods gen:summary-part
  [(define (part->summary bv smr)
     (define i (hash-ref (lazy-bv-index bv) smr #f))
     (if i (force (vector-ref (lazy-bv-slots bv) i)) bv))])

(define (make-lazy-bundle specs)          ; specs: (listof (cons smr lazy?))
  (define comps (list->vector (map car specs)))
  (define lz    (list->vector (map cdr specs)))
  (define n     (vector-length comps))
  (define index (for/hasheq ([s (in-list specs)] [i (in-naturals)]) (values (car s) i)))
  (define (mk i thunk) (if (vector-ref lz i) (delay (thunk)) (thunk)))
  (make-summary
   (lambda (str)
     (lazy-bv (build-vector n (lambda (i) (mk i (lambda () ((vector-ref comps i) str))))) index))
   (lambda (a b)
     (define sa (lazy-bv-slots a)) (define sb (lazy-bv-slots b))
     (lazy-bv (build-vector n (lambda (i)
                (mk i (lambda () ((vector-ref comps i) (force (vector-ref sa i)) (force (vector-ref sb i)))))))
              index))))

(define strict-buf (bundle char-smr strsexp-smr linecol-smr))            ; all eager
(define lazy-buf (make-lazy-bundle                                       ; strsexp DEFERRED
                  (list (cons char-smr #f) (cons linecol-smr #f) (cons strsexp-smr #t))))
(define lite-buf (bundle char-smr linecol-smr))                          ; no sexp (the ceiling)

;; ---------- builders ----------
;; binary doubling ((...)(...)): 2^d lines, but every subtree is a COMPLETE form (empty stacks).
(define (binary-sexp d b)
  (let loop ([k 0] [acc ((make-rope b) "x")])
    (if (= k d) acc (loop (add1 k) ((make-rope b) "(" acc "\n" acc ")")))))
;; linear (((...))): 2d+1 lines, O(d) -- so depth can reach thousands; a cut at the
;; innermost x has a d-deep UNCLOSED opens stack, which is what the sexp combine walks.
(define (linear-sexp d b)
  (let loop ([k 0] [acc ((make-rope b) "x")])
    (if (= k d) acc (loop (add1 k) ((make-rope b) "(\n" acc "\n)")))))
(define (lines rope) (add1 (linecol-lines (linecol-smr rope))))

(define (ns label rows iters thunk)
  (thunk) (collect-garbage)
  (define-values (_ c r g) (time-apply (lambda () (for ([i (in-range iters)]) (thunk))) '()))
  (printf "    ~a ~a us/line\n"
          (~a label #:min-width 28)
          (~a (~r (/ (* r 1000.0) iters rows) #:precision 1) #:min-width 7 #:align 'right)))

(define (combine-ns thunk)
  (thunk) (collect-garbage)
  (define-values (_ c r g) (time-apply (lambda () (for ([i (in-range 300000)]) (thunk))) '()))
  (/ (* r 1e6) 300000))

(module+ main
  ;; ---------- 1. the strsexp COMBINE cost, directly, vs depth ----------
  (printf "strsexp combine of two depth-d ropes  (vs char combine, depth-blind):\n")
  (printf "  ~a ~a ~a\n" (~a "depth" #:min-width 7) (~a "char ns" #:min-width 10) "strsexp ns")
  (for ([d (in-list '(4 8 12 16 20 24))])
    (define h (binary-sexp d strict-buf))     ; a depth-d nested rope; combine it with itself
    (printf "  ~a ~a ~a\n"
            (~a d #:min-width 7)
            (~a (~r (combine-ns (lambda () (char-smr    h h))) #:precision 0) #:min-width 10)
            (~r (combine-ns (lambda () (strsexp-smr h h))) #:precision 0)))

  ;; ---------- 2. lazy vs strict NAVIGATION, vs depth ----------
  (printf "\nA navigation (50-line block, mid-doc): strict vs lazy(sexp deferred) vs lite(no sexp):\n")
  (for ([d (in-list '(8 12 16 20))])
    (define rs (binary-sexp d strict-buf))
    (define rl (binary-sexp d lazy-buf))
    (define rt (binary-sexp d lite-buf))
    (define n  (lines rs))
    (define top (max 0 (- (quotient n 2) 25)))
    (define rows 50)
    ;; correctness: forcing the lazy strsexp gives the strict value, every line
    (for ([a (in-list (heads/A rs top rows strict-buf))]
          [b (in-list (heads/A rl top rows lazy-buf))])
      (unless (equal? (strsexp-smr (first a)) (strsexp-smr (first b)))
        (error 'correctness "lazy strsexp != strict at depth ~a" d)))
    (printf "  depth ~a (~a lines)  -- lazy strsexp == strict ok:\n" d n)
    (ns "strict (sexp combined)" rows 100 (lambda () (heads/A rs top rows strict-buf)))
    (ns "lazy   (sexp deferred)" rows 100 (lambda () (heads/A rl top rows lazy-buf)))
    (ns "lite   (no sexp)"       rows 100 (lambda () (heads/A rt top rows lite-buf)))
    (newline))

  ;; ---------- 3. LINEAR (((...))) nesting: deep unbalanced cuts, large depths ----------
  (printf "LINEAR (((...))) nesting -- 50-line block at the deepest region (cut stack ~ depth):\n")
  (for ([d (in-list '(100 500 1000 2000 4000))])
    (define rs (linear-sexp d strict-buf))
    (define rl (linear-sexp d lazy-buf))
    (define rt (linear-sexp d lite-buf))
    (define n  (lines rs))
    (define top (max 0 (- d 25)))            ; the innermost x sits at line d
    (define rows 50)
    (for ([a (in-list (heads/A rs top rows strict-buf))]
          [b (in-list (heads/A rl top rows lazy-buf))])
      (unless (equal? (strsexp-smr (first a)) (strsexp-smr (first b)))
        (error 'correctness "lazy strsexp != strict at linear depth ~a" d)))
    (printf "  depth ~a (~a lines)  -- lazy strsexp == strict ok:\n" d n)
    (ns "strict (sexp combined)" rows 40 (lambda () (heads/A rs top rows strict-buf)))
    (ns "lazy   (sexp deferred)" rows 40 (lambda () (heads/A rl top rows lazy-buf)))
    (ns "lite   (no sexp)"       rows 40 (lambda () (heads/A rt top rows lite-buf)))
    (newline)))
