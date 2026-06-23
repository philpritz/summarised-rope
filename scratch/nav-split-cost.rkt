#lang racket

;; Diagnostic: how much of per-line nav is the guided DESCENT+REBUILD vs everything else?
;;   real    -- multisect with real line guides (the 706 us/line path)
;;   const-0 -- multisect with (const 0) guides: bisect's decide returns 0 at the root, so
;;              it cuts there with NO descent and NO rope-join rebuild (the proposed probe)
;;   lite    -- real guides but a 2-component bundle (char+linecol) instead of 5: same
;;              descent depth, lighter per-node combine -- isolates the bundle-hash cost
;; (Note: const-0 does NOT navigate to a line; it's a cost probe -- it stops bisect at the
;; root. within-ratio balancing is NOT in the guided path, so it's not what we're removing.)
;;
;; Run:  racket scratch/nav-split-cost.rkt

(require "../rope-core.rkt"            ; make-rope multisect
         "../summaries/summaries.rkt"      ; bundle char-smr linecol-smr
         "../summaries/sexp-summary.rkt"   ; strsexp-smr
         "../helper-algebras.rkt"     ; on
         "../bench/bench.rkt"         ; measure (struct-out stats)
         "lex-normalform.rkt"         ; lex2-smr
         "lex2-highlight-demo.rkt")   ; make-kw-smr

(define kw-smr (make-kw-smr '("define" "if" "displayln" "list")))
(define full (bundle char-smr kw-smr strsexp-smr lex2-smr linecol-smr))   ; 5 components (hasheq)
(define lite (bundle char-smr linecol-smr))                                ; 2 components

(define snippet "(define (f x)\n  (if (> x 0)\n      (displayln \"pos\")\n      (list x)))\n")
(define doc (apply string-append (make-list 2000 snippet)))     ; 8000 lines
(define frope ((make-rope full) doc))
(define lrope ((make-rope lite) doc))
(define nlv (list->vector (for/list ([i (in-range (string-length doc))]
                                     #:when (char=? (string-ref doc i) #\newline)) i)))
(define (bol L) (if (= L 0) 0 (add1 (vector-ref nlv (sub1 L)))))
(define ((at k) L R) (cond [(< L k) 1] [(> L k) -1] [else 0]))
(define (lineg k) (on (at k) char-smr))
(define zero-guide (const 0))               ; returns 0 for any args (Racket's `pure 0`)

(define T 4000) (define R 60)
(define Ls (range T (+ T R)))

(define (per-line label thunk)
  (define st (measure thunk #:reps 30))
  (define per (* 1000.0 (/ (stats-min st) R)))
  (printf "  ~a ~a us/line\n" (~a label #:min-width 34) (~a (~r per #:precision 3) #:min-width 9))
  per)

(printf "nav per line (60-line viewport, 8000-line doc):\n")
(define real (per-line "guided cut, full bundle (5 comps)"
  (lambda () (for ([L (in-list Ls)]) ((multisect full (vector (lineg (bol L)) (lineg (bol (add1 L))))) frope)))))
(define triv (per-line "const-0 guides, full bundle (no descent)"
  (lambda () (for ([L (in-list Ls)]) ((multisect full (vector zero-guide zero-guide)) frope)))))
(define lit  (per-line "guided cut, lite bundle (2 comps)"
  (lambda () (for ([L (in-list Ls)]) ((multisect lite (vector (lineg (bol L)) (lineg (bol (add1 L))))) lrope)))))

(printf "\nattribution:\n")
(printf "  descent+rebuild (real - const0) : ~a us/line  (~a% of real)\n"
        (~r (- real triv) #:precision 3) (~r (* 100.0 (/ (- real triv) real)) #:precision 1))
(printf "  per-node bundle weight (real/lite ratio) : ~ax\n" (~r (/ real (max lit 1e-9)) #:precision 1))
