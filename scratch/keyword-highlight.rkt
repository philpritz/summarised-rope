#lang racket

;; SCRATCH -- keyword highlighter on the summarised rope. Layers so far:
;;   * keywords (boundary monoid, seam-correct)
;;   * positional: keywords colour only in OPERATOR (head) position
;;   * string awareness + string-aware paren depth
;;   * COMMENTS: line (;), nested block (#| |#), shebang (#! at file start)
;; Comments + strings are one LEXER over modes {code, string, line, block(depth), shebang}.
;; A `lex-smr` carries the mode so the focus's ENTRY mode is read off the cached summary at
;; a seam (the strsexp two-case generalized to N modes). In a comment, keywords are quiet
;; and parens don't count; in a string likewise, but a string is one datum (a form).
;;
;; TODO -- (3) the #; datum comment is NOT handled here. It is structural (its extent is the
;; next balanced datum, not a flat finite-state run), so it needs the sexp layer, not just
;; this lexer. Left for a later step.
;;
;; Run:  racket scratch/keyword-highlight.rkt

(require "../rope-core.rkt"         ; make-summary make-rope multisect
         "../summaries/summaries.rkt"     ; bundle char-smr
         "../summaries/sexp-summary.rkt"  ; strsexp-smr strsexp-spines
         "../helper-algebras.rkt") ; on

;; ---------- the alphabet ----------
(define openers (string->list "([{"))
(define closers (string->list ")]}"))
(define delims  (append openers closers (list #\" #\;)))    ; atoms stop at brackets, ws, ", ;
(define (atom-char? c) (not (or (char-whitespace? c) (memv c delims))))

;; ---------- the keyword boundary monoid ----------
(struct kw (lead all? trail) #:transparent)
(define (make-kw-smr keywords)
  (define cap (add1 (apply max 1 (map string-length keywords))))
  (define (capL s) (substring s 0 (min cap (string-length s))))
  (define (capR s) (substring s (max 0 (- (string-length s) cap))))
  (define (run s lo hi step)
    (let loop ([i lo]) (if (and (not (= i hi)) (atom-char? (string-ref s i))) (loop (+ i step)) i)))
  (define (kw-leaf s)
    (and (positive? (string-length s))
         (let* ([n (string-length s)] [lead (run s 0 n 1)] [trail (run s (sub1 n) -1 -1)])
           (kw (capL (substring s 0 lead)) (= lead n) (capR (substring s (add1 trail) n))))))
  (define (kw+ x y)
    (or (and x y
             (kw (if (kw-all? x) (capL (string-append (kw-lead x)  (kw-lead y)))  (kw-lead x))
                 (and (kw-all? x) (kw-all? y))
                 (if (kw-all? y) (capR (string-append (kw-trail x) (kw-trail y))) (kw-trail y))))
        x y))
  (make-summary kw-leaf kw+))
(define (kw-trail-text v) (if (kw? v) (kw-trail v) ""))
(define (kw-lead-text  v) (if (kw? v) (kw-lead  v) ""))

;; ---------- the lexer ----------
;; a mode is 'code | 'string | 'line | 'shebang | (cons 'block depth).
;; (lex-scan s entry) -> (values regions exit-mode), regions a partition of [0,n) into
;; (list kind start end), kind in {code string line block shebang}.
(define (lex-scan s entry)
  (define n (string-length s))
  (define (c i) (string-ref s i))
  (define (two? i a b) (and (< (add1 i) n) (char=? (c i) a) (char=? (c (add1 i)) b)))
  (define (eol i) (let f ([j i]) (cond [(>= j n) n] [(char=? (c j) #\newline) j] [else (f (add1 j))])))
  (define (scan-string i)                       ; from first body char; -> (cons end exit)
    (let f ([j i]) (cond [(>= j n) (cons n 'string)] [(char=? (c j) #\") (cons (add1 j) 'code)] [else (f (add1 j))])))
  (define (scan-block i d)                       ; from after the opener; -> (cons end exit)
    (let f ([j i] [d d])
      (cond [(>= j n) (cons n (cons 'block d))]
            [(two? j #\# #\|) (f (+ j 2) (add1 d))]
            [(two? j #\| #\#) (if (= d 1) (cons (+ j 2) 'code) (f (+ j 2) (sub1 d)))]
            [else (f (add1 j) d)])))
  (define (code-loop i acc)                      ; scan code from i, accumulating regions
    (let scan ([j i])
      (define (with-code upto rest) (if (> upto i) (cons (list 'code i upto) rest) rest))
      (cond
        [(>= j n) (values (reverse (with-code n acc)) 'code)]
        [(char=? (c j) #\;)
         (define e (eol j))
         (if (>= e n) (values (reverse (cons (list 'line j n) (with-code j acc))) 'line)
             (code-loop e (cons (list 'line j e) (with-code j acc))))]
        [(and (= j 0) (two? j #\# #\!))
         (define e (eol j))
         (if (>= e n) (values (reverse (cons (list 'shebang j n) (with-code j acc))) 'shebang)
             (code-loop e (cons (list 'shebang j e) (with-code j acc))))]
        [(two? j #\# #\|)
         (match-define (cons e ex) (scan-block (+ j 2) 1))
         (if (eq? ex 'code) (code-loop e (cons (list 'block j e) (with-code j acc)))
             (values (reverse (cons (list 'block j e) (with-code j acc))) ex))]
        [(char=? (c j) #\")
         (match-define (cons e ex) (scan-string (add1 j)))
         (if (eq? ex 'code) (code-loop e (cons (list 'string j e) (with-code j acc)))
             (values (reverse (cons (list 'string j e) (with-code j acc))) ex))]
        [else (scan (add1 j))])))
  (cond                                          ; resume a non-code entry mode first
    [(eq? entry 'string) (match-define (cons e ex) (scan-string 0))
                         (if (eq? ex 'code) (code-loop e (list (list 'string 0 e))) (values (list (list 'string 0 n)) ex))]
    [(eq? entry 'line)   (define e (eol 0))
                         (if (>= e n) (values (list (list 'line 0 n)) 'line) (code-loop e (list (list 'line 0 e))))]
    [(and (pair? entry) (eq? (car entry) 'block))
                         (match-define (cons e ex) (scan-block 0 (cdr entry)))
                         (if (eq? ex 'code) (code-loop e (list (list 'block 0 e))) (values (list (list 'block 0 n)) ex))]
    [else (code-loop 0 '())]))

;; ---------- the lexer-state summary (entry mode rides this) ----------
(struct lexv (step) #:transparent)               ; step : mode -> exit-mode
(define (exit-of s m) (let-values ([(_ ex) (lex-scan s m)]) ex))
(define (lex-leaf s) (lexv (lambda (m) (exit-of s m))))
(define (lex+ a b)   (lexv (lambda (m) ((lexv-step b) ((lexv-step a) m)))))
(define lex-smr (make-summary lex-leaf lex+))

;; ---------- spans over the lexer's regions ----------
(define (string-spans-of  rs) (for/list ([r (in-list rs)] #:when (eq? (first r) 'string)) (cons (second r) (third r))))
(define (comment-spans-of rs) (for/list ([r (in-list rs)] #:when (memq (first r) '(line block shebang))) (cons (second r) (third r))))

;; per-level form-count stack; a string is one form, a comment is inert
(define (bump counts) (cons (add1 (car counts)) (cdr counts)))
(define (head-kw-spans keywords fr regions bs as entry-counts)
  (define n (string-length fr))
  (define kwset (list->set keywords))
  (let seg ([rs regions] [counts entry-counts] [acc '()])
    (cond
      [(null? rs) (sort acc < #:key car)]
      [(eq? (first (car rs)) 'string) (seg (cdr rs) (bump counts) acc)]      ; a string is one form
      [(memq (first (car rs)) '(line block shebang)) (seg (cdr rs) counts acc)] ; a comment is inert
      [else
       (match-define (list _ a b) (car rs))
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

(define (paren-spans fr regions entry-depth)
  (let loop ([rs regions] [depth entry-depth] [acc '()])
    (cond
      [(null? rs) (sort acc < #:key car)]
      [(not (eq? (first (car rs)) 'code)) (loop (cdr rs) depth acc)]   ; parens count only in code
      [else
       (match-define (list _ a b) (car rs))
       (define-values (d2 acc2)
         (for/fold ([d depth] [out acc]) ([k (in-range a b)])
           (define c (string-ref fr k))
           (cond [(memv c openers) (values (add1 d)         (cons (cons k d)                out))]
                 [(memv c closers) (values (max 0 (sub1 d)) (cons (cons k (max 0 (sub1 d))) out))]
                 [else             (values d out)])))
       (loop (cdr rs) d2 acc2)])))

;; ---------- riding the rope ----------
(define (intify x) (inexact->exact (floor x)))
(define (analyze keywords doc i j)
  (define kw-smr (make-kw-smr keywords))
  (define buf    (bundle char-smr kw-smr strsexp-smr lex-smr))
  (define rope   ((make-rope buf) doc))
  (define ((at n) L R) (cond [(< L n) 1] [(> L n) -1] [else 0]))
  (define-values (b m a)
    ((multisect buf (vector (on (at i) char-smr) (on (at j) char-smr))) rope))
  (define entry-mode ((lexv-step (lex-smr b)) 'code))            ; the focus's lexer mode, from the summary
  (define-values (front _) (strsexp-spines (strsexp-smr b) (strsexp-smr m)))
  (define entry-depth (sub1 (length front)))
  (define entry-counts (map intify front))
  (define fr (~a m))
  (define-values (regions exit) (lex-scan fr entry-mode))
  (values fr entry-mode
          (head-kw-spans keywords fr regions (kw-smr b) (kw-smr a) entry-counts)
          (string-spans-of regions)
          (comment-spans-of regions)
          (paren-spans fr regions entry-depth)))

;; ---------- display ----------
(define (paint fr kwS strS cmtS)       ; «kw»  ⟦str⟧  ⟨comment⟩
  (define tagged (sort (append (map (lambda (s) (list (car s) (cdr s) "«" "»")) kwS)
                               (map (lambda (s) (list (car s) (cdr s) "⟦" "⟧")) strS)
                               (map (lambda (s) (list (car s) (cdr s) "⟨" "⟩")) cmtS))
                       < #:key first))
  (let loop ([i 0] [ts tagged] [out '()])
    (match ts
      ['() (apply string-append (reverse (cons (substring fr i) out)))]
      [(cons (list a b o c) rest)
       (loop b rest (list* (string-append o (substring fr a b) c) (substring fr i a) out))])))

(define kws '("define" "lambda" "let" "if" "cond"))
(printf "keywords: ~s\n\n" kws)

(define (whole label doc)
  (let-values ([(fr mode kwS strS cmtS pS) (analyze kws doc 0 (string-length doc))])
    (printf "~a\n   ~s\n   ~a\n\n" label doc (paint fr kwS strS cmtS))))

(whole "1. line comment"        "(define x 1) ; the x slot")
(whole "2. nested block comment" "#| outer #| inner |# still |# (define y 2)")
(whole "4. shebang"             "#!/usr/bin/env racket\n(define main 0)")
(whole "   suppression"         "(define f) ; (lambda no) define here")
(whole "   all together"        "(define s \"hi\") ; greet (x)\n(if a (lambda () b))")

(define D "#| aaaaaaaa bbbbbbbb cccccccc |#")
(let-values ([(fr mode kwS strS cmtS pS) (analyze kws D 10 22)])
  (printf "5. window [10,22) of a block comment  ~s\n" D)
  (printf "   entry mode = ~s   (read from the cached lex-smr -- the focus opens INSIDE the block)\n" mode)
  (printf "   focus  : ~s\n   ~a   (all comment, carried across the cut)\n" fr (paint fr kwS strS cmtS)))
