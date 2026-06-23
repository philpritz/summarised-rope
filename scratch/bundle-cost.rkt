#lang racket

;; What is the screen-carve actually costing, and can a lazy bundle or a lighter one cut it?
;;   eager-5  the current bundle (char kw strsexp lex2 linecol), hasheq, all slots eager
;;   lite-2   only char + linecol (the "line info" needed to navigate)
;;   char-1   char-smr alone, no bundle (a summary by itself)
;;   lazy-5   same 5 components but each slot is a promise -- combine builds thunks, slots
;;            forced only on extraction (memoized)
;; We time (a) the raw screen carve (the multisect) under each, and (b) the full carve+walk
;; render (eager-5 vs lazy-5), which also FORCES the syntax slots of `before`/`screen`.
;;
;; Run:  racket scratch/bundle-cost.rkt

(require racket/match racket/set racket/promise
         "../rope-core.rkt"            ; make-rope make-summary multisect gen:summary-part part->summary
         "../summaries/summaries.rkt"      ; bundle char-smr linecol-smr
         "../summaries/sexp-summary.rkt"   ; strsexp-smr strsexp-spines
         "../helper-algebras.rkt"     ; on
         "../bench/bench.rkt"         ; measure (struct-out stats)
         "lex-normalform.rkt"         ; lex2-smr apply-step lex-scan
         "lex2-highlight-demo.rkt"    ; make-kw-smr openers closers atom-char? kw-*-text *-spans-of intify
         "../zipper-core.rkt")        ; start to-root zipper-focus (editing)

;; ---- a lazy bundle: slots are promises ----
(struct lbv (slots) #:transparent
  #:methods gen:summary-part
  [(define (part->summary bv smr)
     (if (hash-has-key? (lbv-slots bv) smr)
         (force (hash-ref (lbv-slots bv) smr))
         bv))])                                  ; the bundle smr itself: already a summary
(define (lazy-bundle . cs)
  (make-summary
   (lambda (s)   (lbv (for/hasheq ([c (in-list cs)]) (values c (delay (c s))))))
   (lambda (a b) (lbv (for/hasheq ([c (in-list cs)]) (values c (delay (c a b))))))))

(define keywords '("define" "if" "displayln" "list" "lambda" "let" "cond"))
(define kw-smr (make-kw-smr keywords))
(define kwset (list->set keywords))
(define eager-5 (bundle char-smr kw-smr strsexp-smr lex2-smr linecol-smr))
(define lite-2  (bundle char-smr linecol-smr))
(define lazy-5  (lazy-bundle char-smr kw-smr strsexp-smr lex2-smr linecol-smr))

(define snippet "(define (f x)\n  (if (> x 0)\n      (displayln \"pos\")\n      (list x)))\n")
(define doc (apply string-append (make-list 2000 snippet)))     ; 8000 lines
(define erope ((make-rope eager-5) doc))
(define lrope ((make-rope lite-2)  doc))
(define crope ((make-rope char-smr) doc))
(define zrope ((make-rope lazy-5)  doc))
(define nlv (list->vector (for/list ([i (in-range (string-length doc))]
                                     #:when (char=? (string-ref doc i) #\newline)) i)))
(define (bol L) (if (= L 0) 0 (add1 (vector-ref nlv (sub1 L)))))
(define ((at k) L R) (cond [(< L k) 1] [(> L k) -1] [else 0]))
(define T 4000) (define R 60)

(define (us label thunk #:reps [reps 200])
  (define st (measure thunk #:reps reps))
  (printf "  ~a ~a us\n" (~a label #:min-width 26) (~a (~r (* 1000.0 (stats-min st)) #:precision 2) #:min-width 9))
  (* 1000.0 (stats-min st)))

;; ---- (a) raw screen carve under each config ----
(printf "screen carve (one multisect for ~a lines):\n" R)
(us "eager-5 (current)" (lambda () ((multisect eager-5 (vector (on (at (bol T)) char-smr) (on (at (bol (+ T R))) char-smr))) erope)))
(us "lite-2  (char+linecol)" (lambda () ((multisect lite-2 (vector (on (at (bol T)) char-smr) (on (at (bol (+ T R))) char-smr))) lrope)))
(us "char-1  (standalone)" (lambda () ((multisect char-smr (vector (at (bol T)) (at (bol (+ T R))))) crope)))
(us "lazy-5  (carve only)" (lambda () ((multisect lazy-5 (vector (on (at (bol T)) char-smr) (on (at (bol (+ T R))) char-smr))) zrope)))

;; ---- (b) full carve+walk render: eager-5 vs lazy-5 (forces the syntax slots too) ----
(define (bump counts) (cons (add1 (car counts)) (cdr counts)))
(define (paren-spans+ fr regions d0)
  (let loop ([rs regions] [d d0] [acc '()])
    (cond [(null? rs) (values (sort acc < #:key car) d)]
          [(not (eq? (first (car rs)) 'code)) (loop (cdr rs) d acc)]
          [else (match-define (list _ a b) (car rs))
                (define-values (d2 acc2)
                  (for/fold ([d d] [out acc]) ([k (in-range a b)])
                    (define c (string-ref fr k))
                    (cond [(memv c openers) (values (add1 d) (cons (cons k d) out))]
                          [(memv c closers) (values (max 0 (sub1 d)) (cons (cons k (max 0 (sub1 d))) out))]
                          [else (values d out)])))
                (loop (cdr rs) d2 acc2)])))
(define (head-kw+ fr regions counts0)
  (define n (string-length fr))
  (let seg ([rs regions] [counts counts0] [acc '()])
    (cond [(null? rs) (values (sort acc < #:key car) counts)]
          [(eq? (first (car rs)) 'string) (seg (cdr rs) (bump counts) acc)]
          [(memq (first (car rs)) '(line block shebang)) (seg (cdr rs) counts acc)]
          [else (match-define (list _ a b) (car rs))
                (let ch ([k a] [counts counts] [acc acc])
                  (cond [(>= k b) (seg (cdr rs) counts acc)]
                        [(memv (string-ref fr k) openers) (ch (add1 k) (cons 0 counts) acc)]
                        [(memv (string-ref fr k) closers) (ch (add1 k) (if (> (length counts) 1) (bump (cdr counts)) (bump counts)) acc)]
                        [(atom-char? (string-ref fr k))
                         (define e (let s ([t k]) (if (and (< t b) (atom-char? (string-ref fr t))) (s (add1 t)) t)))
                         (define head? (and (> (length counts) 1) (= (car counts) 0)))
                         (define hit? (and head? (set-member? kwset (substring fr k e))))
                         (ch e (bump counts) (if hit? (cons (cons k e) acc) acc))]
                        [else (ch (add1 k) counts acc)]))])))
(define (carve+walk buf rope)
  (define-values (before screen after)
    ((multisect buf (vector (on (at (bol T)) char-smr) (on (at (bol (+ T R))) char-smr))) rope))
  (define mode0 (apply-step (lex2-smr before) 'code))
  (define-values (front0 _) (strsexp-spines (strsexp-smr before) (strsexp-smr screen)))
  (define lines (take (regexp-split #rx"\n" (~a screen)) R))
  (let loop ([ls lines] [mode mode0] [depth (sub1 (length front0))] [counts (map intify front0)] [acc '()])
    (cond [(null? ls) (reverse acc)]
          [else (define-values (regions em) (lex-scan (car ls) mode))
                (define-values (kwS ec) (head-kw+ (car ls) regions counts))
                (define-values (pS ed)  (paren-spans+ (car ls) regions depth))
                (loop (cdr ls) em ed ec (cons (list mode (car ls) kwS (string-spans-of regions) (comment-spans-of regions) pS) acc))])))

(unless (equal? (carve+walk eager-5 erope) (carve+walk lazy-5 zrope)) (error "eager != lazy render"))
(printf "\nfull carve+walk render (~a lines, produces lines+spans):\n" R)
(void (us "eager-5" (lambda () (carve+walk eager-5 erope)) #:reps 30))
(void (us "lazy-5"  (lambda () (carve+walk lazy-5  zrope)) #:reps 30))

;; ---- per-keystroke cycle: insert a char at the cursor, fold to the doc, re-render ----
;; carve by LINE guides (robust to the char insert shifting offsets).
(define ((col line k) L R)
  (match-define (linecol _ ll lc) (linecol-smr L))
  (cond [(not (= ll line)) (if (< ll line) 1 -1)] [(< lc k) 1] [(> lc k) -1] [else 0]))
(define (cw-col buf rope)
  (define-values (before screen after) ((multisect buf (vector (col T 0) (col (+ T R) 0))) rope))
  (define mode0 (apply-step (lex2-smr before) 'code))
  (define-values (front0 _) (strsexp-spines (strsexp-smr before) (strsexp-smr screen)))
  (define lines (take (regexp-split #rx"\n" (~a screen)) R))
  (let loop ([ls lines] [mode mode0] [depth (sub1 (length front0))] [counts (map intify front0)] [acc '()])
    (cond [(null? ls) (reverse acc)]
          [else (define-values (regions em) (lex-scan (car ls) mode))
                (define-values (kwS ec) (head-kw+ (car ls) regions counts))
                (define-values (pS ed)  (paren-spans+ (car ls) regions depth))
                (loop (cdr ls) em ed ec (cons (list mode (car ls) kwS (string-spans-of regions) (comment-spans-of regions) pS) acc))])))
;; a persistent cursor (gap) at line T+30, col 5 -- navigated once, outside timing.
(define cursor (vector (col (+ T 30) 5) (col (+ T 30) 5)))
(define (mk-cursor buf rope) ((setter zipper-guide cursor) (start buf rope cursor)))
(define zc-e (mk-cursor eager-5 erope))
(define zc-z (mk-cursor lazy-5  zrope))
(define (keystroke buf zc)                       ; insert 'x' + fold to doc + re-render viewport
  (define rope2 ((viewer zipper-focus) (to-root ((setter zipper-focus "x") zc))))
  (cw-col buf rope2))
(define (edit-only buf zc)                        ; just the edit -> new doc rope (no render)
  ((viewer zipper-focus) (to-root ((setter zipper-focus "x") zc))))

(printf "\nper-keystroke cycle (insert 1 char in viewport, re-render ~a lines):\n" R)
(void (us "eager-5 edit + re-render" (lambda () (keystroke eager-5 zc-e)) #:reps 20))
(void (us "lazy-5  edit + re-render" (lambda () (keystroke lazy-5  zc-z)) #:reps 20))
(void (us "eager-5 edit only"        (lambda () (edit-only eager-5 zc-e)) #:reps 50))
(void (us "lazy-5  edit only"        (lambda () (edit-only lazy-5  zc-z)) #:reps 50))
