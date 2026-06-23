#lang racket

;; Fair comparison: produce the SAME R lines (text + all spans) two ways.
;;   A  per-line: for each line, multisect from root -> head -> analyze (R navigations).
;;   B  carve+walk: ONE multisect for the whole screen, then walk the screen rope splitting
;;      into lines, carrying lexer mode / paren depth / form-counts forward (seeded once off
;;      `before`).  One navigation, then a linear pass.
;; Both return the same list of (mode text kwS strS cmtS pS); we assert equal? then time.
;;
;; Run:  racket scratch/screen-walk-cost.rkt

(require racket/match racket/set
         "../rope-core.rkt"            ; make-rope multisect
         "../summaries/summaries.rkt"      ; bundle char-smr linecol-smr
         "../summaries/sexp-summary.rkt"   ; strsexp-smr strsexp-spines
         "../helper-algebras.rkt"     ; on
         "../bench/bench.rkt"         ; measure (struct-out stats)
         "lex-normalform.rkt"         ; lex2-smr apply-step lex-scan
         "lex2-highlight-demo.rkt")   ; make-kw-smr openers closers atom-char? kw-*-text *-spans-of intify

(define keywords '("define" "if" "displayln" "list" "lambda" "let" "cond"))
(define kw-smr (make-kw-smr keywords))
(define buf (bundle char-smr kw-smr strsexp-smr lex2-smr linecol-smr))
(define snippet "(define (f x)\n  (if (> x 0)\n      (displayln \"pos\")\n      (list x)))\n")
(define doc (apply string-append (make-list 2000 snippet)))     ; 8000 lines, comment-free
(define rope ((make-rope buf) doc))
(define nlv (list->vector (for/list ([i (in-range (string-length doc))]
                                     #:when (char=? (string-ref doc i) #\newline)) i)))
(define (bol L) (if (= L 0) 0 (add1 (vector-ref nlv (sub1 L)))))
(define (eol L) (vector-ref nlv L))                  ; the \n ending line L (line text is [bol,eol))
(define ((at k) L R) (cond [(< L k) 1] [(> L k) -1] [else 0]))
(define (g k) (on (at k) char-smr))

;; span builders that also RETURN exit state, so B can carry it line-to-line.
(define (bump counts) (cons (add1 (car counts)) (cdr counts)))
(define (paren-spans+ fr regions entry-depth)
  (let loop ([rs regions] [depth entry-depth] [acc '()])
    (cond
      [(null? rs) (values (sort acc < #:key car) depth)]
      [(not (eq? (first (car rs)) 'code)) (loop (cdr rs) depth acc)]
      [else (match-define (list _ a b) (car rs))
            (define-values (d2 acc2)
              (for/fold ([d depth] [out acc]) ([k (in-range a b)])
                (define c (string-ref fr k))
                (cond [(memv c openers) (values (add1 d) (cons (cons k d) out))]
                      [(memv c closers) (values (max 0 (sub1 d)) (cons (cons k (max 0 (sub1 d))) out))]
                      [else (values d out)])))
            (loop (cdr rs) d2 acc2)])))
(define (head-kw-spans+ kwset fr regions bs as entry-counts)
  (define n (string-length fr))
  (let seg ([rs regions] [counts entry-counts] [acc '()])
    (cond
      [(null? rs) (values (sort acc < #:key car) counts)]
      [(eq? (first (car rs)) 'string) (seg (cdr rs) (bump counts) acc)]
      [(memq (first (car rs)) '(line block shebang)) (seg (cdr rs) counts acc)]
      [else (match-define (list _ a b) (car rs))
            (let ch ([k a] [counts counts] [acc acc])
              (cond
                [(>= k b) (seg (cdr rs) counts acc)]
                [(memv (string-ref fr k) openers) (ch (add1 k) (cons 0 counts) acc)]
                [(memv (string-ref fr k) closers) (ch (add1 k) (if (> (length counts) 1) (bump (cdr counts)) (bump counts)) acc)]
                [(atom-char? (string-ref fr k))
                 (define e (let s ([t k]) (if (and (< t b) (atom-char? (string-ref fr t))) (s (add1 t)) t)))
                 (define head? (and (> (length counts) 1) (= (car counts) 0)))
                 (define left  (if (= k 0) (kw-trail-text bs) ""))
                 (define right (if (= e n) (kw-lead-text  as) ""))
                 (define hit?  (and head? (set-member? kwset (string-append left (substring fr k e) right))))
                 (ch e (bump counts) (if hit? (cons (cons k e) acc) acc))]
                [else (ch (add1 k) counts acc)]))])))

(define kwset (list->set keywords))
;; one line's record from its focus text + entry state -> (record exit-mode exit-depth exit-counts)
(define (paint fr mode depth counts)
  (define-values (regions exit-mode) (lex-scan fr mode))
  (define-values (kwS counts2) (head-kw-spans+ kwset fr regions "" "" counts))
  (define-values (pS depth2)   (paren-spans+ fr regions depth))
  (values (list mode fr kwS (string-spans-of regions) (comment-spans-of regions) pS)
          exit-mode depth2 counts2))

;; ---- A: per-line nav, R multisects from root ----
(define (analyze-A Ls)
  (for/list ([L (in-list Ls)])
    (define-values (b m a) ((multisect buf (vector (g (bol L)) (g (eol L)))) rope))
    (define mode (apply-step (lex2-smr b) 'code))
    (define-values (front _) (strsexp-spines (strsexp-smr b) (strsexp-smr m)))
    (define-values (rec _em _ed _ec) (paint (~a m) mode (sub1 (length front)) (map intify front)))
    rec))

;; ---- B: one carve for the screen, then a carrying walk ----
(define (analyze-B Ls)
  (define T (first Ls)) (define R (length Ls))
  (define-values (before screen after)
    ((multisect buf (vector (g (bol T)) (g (bol (+ T R))))) rope))
  (define mode0 (apply-step (lex2-smr before) 'code))
  (define-values (front0 _) (strsexp-spines (strsexp-smr before) (strsexp-smr screen)))
  (define lines (take (regexp-split #rx"\n" (~a screen)) R))
  (let loop ([ls lines] [mode mode0] [depth (sub1 (length front0))] [counts (map intify front0)] [acc '()])
    (cond
      [(null? ls) (reverse acc)]
      [else (define-values (rec em ed ec) (paint (car ls) mode depth counts))
            (loop (cdr ls) em ed ec (cons rec acc))])))

;; ---- check both agree, then time ----
(define T 4000) (define R 60)
(define Ls (range T (+ T R)))
(unless (equal? (analyze-A Ls) (analyze-B Ls)) (error "A and B disagree on the rendered lines"))
(printf "correctness: A (per-line) == B (carve+walk), all ~a lines identical\n\n" R)

(define (per-line label thunk)
  (define st (measure thunk #:reps 30))
  (define tot (* 1000.0 (stats-min st)))
  (printf "  ~a ~a us total   ~a us/line\n"
          (~a label #:min-width 22) (~a (~r tot #:precision 1) #:min-width 9)
          (~a (~r (/ tot R) #:precision 3) #:min-width 9))
  tot)

(printf "render a ~a-line screen, both producing the lines+spans (8000-line doc):\n" R)
(define ta (per-line "A per-line nav" (lambda () (analyze-A Ls))))
(define tb (per-line "B carve + walk"  (lambda () (analyze-B Ls))))
(printf "\n  speedup (A/B): ~ax\n" (~r (/ ta (max tb 1e-9)) #:precision 1))
