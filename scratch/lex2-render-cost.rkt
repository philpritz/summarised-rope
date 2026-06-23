#lang racket

;; Where does per-line render time actually go?  Split the cost of producing one line's
;; spans into three parts and time each over a viewport on a big document:
;;   nav  -- navigate the rope to the line (multisect to the head b.m.a)
;;   read -- the O(1)/O(depth) summary reads off the head (entry mode, spine, kw lead/trail)
;;   scan -- lex-scan the FOCUS char-by-char + build the spans
;; If scan << nav, optimizing the syntax scan (e.g. reading intervals off the summary) is
;; chasing a small slice.  Then: the interior-line SKIP -- a line wholly inside a block
;; comment, full scan vs the O(1) "a=0 so one comment region" check off the cached LX.
;;
;; Run:  racket scratch/lex2-render-cost.rkt

(require racket/match
         "../rope-core.rkt"            ; make-rope multisect
         "../summaries/summaries.rkt"      ; bundle char-smr linecol-smr
         "../summaries/sexp-summary.rkt"   ; strsexp-smr strsexp-spines
         "../helper-algebras.rkt"     ; on
         "../bench/bench.rkt"         ; measure (struct-out stats)
         "lex-normalform.rkt"         ; lex2-smr apply-step lex-scan (struct-out LX)
         "lex2-highlight-demo.rkt")   ; make-kw-smr head-kw-spans paren-spans *-spans-of intify

(define keywords '("define" "if" "displayln" "list" "lambda" "let" "cond"))
(define kw-smr (make-kw-smr keywords))
(define buf (bundle char-smr kw-smr strsexp-smr lex2-smr linecol-smr))

(define snippet "(define (f x)\n  (if (> x 0)\n      (displayln \"pos\")\n      (list x)))\n")
(define doc (apply string-append (make-list 2000 snippet)))     ; 8000 lines
(define rope ((make-rope buf) doc))
(define nlv (list->vector (for/list ([i (in-range (string-length doc))]
                                     #:when (char=? (string-ref doc i) #\newline)) i)))
(define (bol L) (if (= L 0) 0 (add1 (vector-ref nlv (sub1 L)))))
(define ((at k) L R) (cond [(< L k) 1] [(> L k) -1] [else 0]))
(define (g k) (on (at k) char-smr))
(define (head-of L)
  (call-with-values (lambda () ((multisect buf (vector (g (bol L)) (g (bol (add1 L))))) rope)) list))

;; ---- the three stages, as separately-timeable pieces ----
(define (do-read b m a)                         ; the O(1)/O(depth) summary reads
  (define mode (apply-step (lex2-smr b) 'code))
  (define-values (front _) (strsexp-spines (strsexp-smr b) (strsexp-smr m)))
  (vector mode (map intify front) (sub1 (length front)) (kw-smr b) (kw-smr a)))
(define (do-scan fr mode counts depth bsk ask)  ; the char scan + span builders
  (define-values (regions _) (lex-scan fr mode))
  (vector (head-kw-spans keywords fr regions bsk ask counts)
          (string-spans-of regions) (comment-spans-of regions)
          (paren-spans fr regions depth)))

(define T 4000) (define R 60)                   ; a 60-line viewport mid-document
(define Ls (range T (+ T R)))
(define heads (for/list ([L (in-list Ls)]) (head-of L)))
(define reads
  (for/list ([h (in-list heads)])
    (match-define (list b m a) h)
    (match-define (vector mode counts depth bsk ask) (do-read b m a))
    (list (~a m) mode counts depth bsk ask)))

(define (per-line label thunk)                  ; measure whole-viewport thunk, report per line
  (define st (measure thunk #:reps 30))
  (* 1000.0 (/ (stats-min st) R)))              ; ms total -> us per line

(printf "per-line render cost, 60-line viewport mid an 8000-line doc (us/line):\n\n")
(define nav  (per-line "nav"  (lambda () (for ([L (in-list Ls)]) (head-of L)))))
(define rd   (per-line "read" (lambda () (for ([h (in-list heads)]) (match-define (list b m a) h) (do-read b m a)))))
(define scn  (per-line "scan" (lambda () (for ([r (in-list reads)])
                                           (match-define (list fr mode counts depth bsk ask) r)
                                           (do-scan fr mode counts depth bsk ask)))))
(define tot (+ nav rd scn))
(define (row label v) (printf "  ~a ~a us/line   ~a%\n"
                              (~a label #:min-width 6) (~a (~r v #:precision 3) #:min-width 9)
                              (~a (~r (* 100.0 (/ v tot)) #:precision 1) #:min-width 5)))
(row "nav"  nav) (row "read" rd) (row "scan" scn)
(printf "  ~a ~a us/line\n" (~a "total" #:min-width 6) (~a (~r tot #:precision 3) #:min-width 9))

;; ---- interior block-comment line: full scan vs the O(1) skip ----
(define cdoc (string-append "#|\n"
                            (apply string-append (make-list 4000 "  inside the block comment, line text here\n"))
                            "|#\n(define real 1)\n"))
(define crope ((make-rope buf) cdoc))
(define cnlv (list->vector (for/list ([i (in-range (string-length cdoc))]
                                      #:when (char=? (string-ref cdoc i) #\newline)) i)))
(define (cbol L) (if (= L 0) 0 (add1 (vector-ref cnlv (sub1 L)))))
(define-values (cb cm ca)                       ; an interior comment line's head
  ((multisect buf (vector (g (cbol 2000)) (g (cbol 2001)))) crope))
(define cmode (apply-step (lex2-smr cb) 'code))
(define cfr (~a cm))
(printf "\ninterior block-comment line  (entry mode ~s, ~a chars):\n" cmode (string-length cfr))
(define full (* 1000.0 (stats-min (measure (lambda () (do-scan cfr cmode '(0) 0 (kw-smr cb) (kw-smr ca))) #:reps 4000))))
(define skip (* 1000.0 (stats-min (measure (lambda () (and (eq? (car cmode) 'block) (= (LX-a (lex2-smr cm)) 0)
                                                           (list 'comment 0 (string-length cfr)))) #:reps 4000))))
(printf "  full scan : ~a us/line\n  O(1) skip : ~a us/line  (a=0 off the cached LX -> one comment region)\n"
        (~r full #:precision 3) (~r skip #:precision 3))
