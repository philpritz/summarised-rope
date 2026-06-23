#lang racket

;; Demonstration: the comment-aware highlighter DRIVEN BY the normal-form lexer (lex2-smr),
;; on an ADVERSARIAL sample -- strings stuffed with brackets / semicolons / comment-marks,
;; line and nested block comments hiding keywords and parens, a string and a block comment
;; each spanning lines, and a block that closes mid-line back into real code.
;;
;; Per line we read the focus's ENTRY MODE off lex2 (apply-step), then derive keyword /
;; string / comment / paren-depth spans -- keyword-highlight.rkt's analyze, with the
;; closure lex-smr swapped for lex2-smr.  Entry mode is read from the line's PREFIX as a
;; single leaf (so a 2-char #|/|# token can't be split across a 32-char rope leaf -- the
;; known seam gap, orthogonal to this correctness demo).  Prints one record per line.
;;
;; Span/keyword helpers are COPIED VERBATIM from keyword-highlight.rkt; lexer pieces are
;; required from lex-normalform.rkt.
;;
;; Run:  racket scratch/lex2-highlight-demo.rkt

(require "../rope-core.rkt"            ; make-summary
         "../summaries/sexp-summary.rkt"   ; strsexp-smr strsexp-spines
         "lex-normalform.rkt")        ; lex2-smr apply-step lex-scan

(provide make-kw-smr kw-trail-text kw-lead-text string-spans-of comment-spans-of
         head-kw-spans paren-spans intify openers closers atom-char? analyze-line)

;; ===== VERBATIM from keyword-highlight.rkt =====
(define openers (string->list "([{"))
(define closers (string->list ")]}"))
(define delims  (append openers closers (list #\" #\;)))
(define (atom-char? c) (not (or (char-whitespace? c) (memv c delims))))

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

(define (string-spans-of  rs) (for/list ([r (in-list rs)] #:when (eq? (first r) 'string)) (cons (second r) (third r))))
(define (comment-spans-of rs) (for/list ([r (in-list rs)] #:when (memq (first r) '(line block shebang))) (cons (second r) (third r))))

(define (bump counts) (cons (add1 (car counts)) (cdr counts)))
(define (head-kw-spans keywords fr regions bs as entry-counts)
  (define n (string-length fr))
  (define kwset (list->set keywords))
  (let seg ([rs regions] [counts entry-counts] [acc '()])
    (cond
      [(null? rs) (sort acc < #:key car)]
      [(eq? (first (car rs)) 'string) (seg (cdr rs) (bump counts) acc)]
      [(memq (first (car rs)) '(line block shebang)) (seg (cdr rs) counts acc)]
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
      [(not (eq? (first (car rs)) 'code)) (loop (cdr rs) depth acc)]
      [else
       (match-define (list _ a b) (car rs))
       (define-values (d2 acc2)
         (for/fold ([d depth] [out acc]) ([k (in-range a b)])
           (define c (string-ref fr k))
           (cond [(memv c openers) (values (add1 d)         (cons (cons k d)                out))]
                 [(memv c closers) (values (max 0 (sub1 d)) (cons (cons k (max 0 (sub1 d))) out))]
                 [else             (values d out)])))
       (loop (cdr rs) d2 acc2)])))

(define (intify x) (inexact->exact (floor x)))
;; ===== end verbatim =====

;; analyze one line [bol,eol) of doc; lex2 (on the prefix) drives the entry mode.
(define (analyze-line keywords kw-smr doc bol eol)
  (define before (substring doc 0 bol))
  (define fr     (substring doc bol eol))
  (define after  (substring doc eol))
  (define entry-mode (apply-step (lex2-smr before) 'code))           ; <-- the normal-form lexer read
  (define-values (front _) (strsexp-spines (strsexp-smr before) (strsexp-smr fr)))
  (define entry-counts (map intify front))
  (define entry-depth (sub1 (length front)))
  (define-values (regions exit) (lex-scan fr entry-mode))
  (values entry-mode fr
          (head-kw-spans keywords fr regions (kw-smr before) (kw-smr after) entry-counts)
          (string-spans-of regions) (comment-spans-of regions)
          (paren-spans fr regions entry-depth)))

(module+ main
  (define keywords '("define" "displayln" "if" "string?" "list" "cond" "let" "lambda"))
  (define kw-smr (make-kw-smr keywords))
  (define lines
    (list "(define greet \"hi ; ) ( #| |#\")"
          "; cmt: \"looks (like) code\" #| but |# all dead"
          "(displayln \"multi-line string"
          "  with ) ( ; #| define inside\")"
          "#| blk: \"str\" ; and (bal) parens"
          "   #| nested |# outer still |# (+ 1 2)"
          "(if (string? x) \"a)b(c\" (list))"))
  (define doc (string-join lines "\n"))
  (define out
    (let loop ([ls lines] [bol 0] [acc '()])
      (cond
        [(null? ls) (reverse acc)]
        [else
         (define eol (+ bol (string-length (car ls))))
         (define-values (mode fr kwS strS cmtS pS) (analyze-line keywords kw-smr doc bol eol))
         (loop (cdr ls) (+ eol 1) (cons (list mode fr kwS strS cmtS pS) acc))])))
  (write out)
  (newline))
