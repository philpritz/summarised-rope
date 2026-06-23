#lang racket

;; SCRATCH bench for the ADVANCED highlighter (scratch/keyword-highlight.rkt): its new
;; piece is `lex-smr`, an N-mode lexer monoid whose summary VALUE is a closure
;; (step : mode -> exit-mode), composed by `lex+`. strsexp's value is a finite struct
;; reduced at combine time. Question: does reading the focus's entry-mode off a cut stay
;; O(log N) (like the sexp spine read), or does the deferred closure chain make it O(left)?
;;
;; lex-scan + the lex monoid are COPIED VERBATIM from keyword-highlight.rkt (the leaf-
;; speedups.rkt precedent -- a self-contained bench that doesn't import unexported guts).
;;
;; Run:  racket scratch/lex-bench.rkt

(require "../rope-core.rkt"            ; make-rope make-summary multisect
         "../summaries/summaries.rkt"      ; bundle char-smr linecol-smr
         "../summaries/sexp-summary.rkt"   ; strsexp-smr strsexp-spines sand-spines
         "../helper-algebras.rkt"     ; on
         "../bench/bench.rkt")        ; measure (struct-out stats)

;; ===== VERBATIM from keyword-highlight.rkt: the lexer + its summary =====
(define (lex-scan s entry)
  (define n (string-length s))
  (define (c i) (string-ref s i))
  (define (two? i a b) (and (< (add1 i) n) (char=? (c i) a) (char=? (c (add1 i)) b)))
  (define (eol i) (let f ([j i]) (cond [(>= j n) n] [(char=? (c j) #\newline) j] [else (f (add1 j))])))
  (define (scan-string i)
    (let f ([j i]) (cond [(>= j n) (cons n 'string)] [(char=? (c j) #\") (cons (add1 j) 'code)] [else (f (add1 j))])))
  (define (scan-block i d)
    (let f ([j i] [d d])
      (cond [(>= j n) (cons n (cons 'block d))]
            [(two? j #\# #\|) (f (+ j 2) (add1 d))]
            [(two? j #\| #\#) (if (= d 1) (cons (+ j 2) 'code) (f (+ j 2) (sub1 d)))]
            [else (f (add1 j) d)])))
  (define (code-loop i acc)
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
  (cond
    [(eq? entry 'string) (match-define (cons e ex) (scan-string 0))
                         (if (eq? ex 'code) (code-loop e (list (list 'string 0 e))) (values (list (list 'string 0 n)) ex))]
    [(eq? entry 'line)   (define e (eol 0))
                         (if (>= e n) (values (list (list 'line 0 n)) 'line) (code-loop e (list (list 'line 0 e))))]
    [(and (pair? entry) (eq? (car entry) 'block))
                         (match-define (cons e ex) (scan-block 0 (cdr entry)))
                         (if (eq? ex 'code) (code-loop e (list (list 'block 0 e))) (values (list (list 'block 0 n)) ex))]
    [else (code-loop 0 '())]))

(require racket/match)
(struct lexv (step) #:transparent)
(define (exit-of s m) (let-values ([(_ ex) (lex-scan s m)]) ex))
(define (lex-leaf s) (lexv (lambda (m) (exit-of s m))))
(define (lex+ a b)   (lexv (lambda (m) ((lexv-step b) ((lexv-step a) m)))))
(define lex-smr (make-summary lex-leaf lex+))
;; ===== end verbatim =====

(define (fmt x) (~r x #:precision '(= 4) #:min-width 10))

;; ---------- 1. component micro: combine (smr a b) and leaf (smr s) ----------
(define line "(define (fact n) (if (zero? n) 1 (* n (fact (sub1 n)))))")
(define mid  (quotient (string-length line) 2))
(define (ns label iters thunk)
  (thunk) (collect-garbage)
  (define-values (_ cpu real gc) (time-apply (lambda () (for ([i (in-range iters)]) (thunk))) '()))
  (printf "  ~a ~a ns/op\n" (~a label #:min-width 22) (~a (~r (/ (* real 1e6) iters) #:precision 1) #:min-width 9 #:align 'right)))
(define (cb smr) (let ([a (smr (substring line 0 mid))] [b (smr (substring line mid))]) (lambda () (smr a b))))
(printf "component combine (smr a b):\n")
(ns "char-smr"    300000 (cb char-smr))
(ns "strsexp-smr" 300000 (cb strsexp-smr))
(ns "lex-smr"     300000 (cb lex-smr))
(printf "leaf (smr s):\n")
(ns "strsexp-smr" 200000 (lambda () (strsexp-smr line)))
(ns "lex-smr"     200000 (lambda () (lex-smr line)))

;; ---------- 2. THE scaling test: read syntax context at a midpoint cut ----------
;; Build an n-line doc (untimed), cut at the char midpoint, then time two reads off the
;; cut: lex entry-mode (call the composed closure) vs strsexp front-spine (sand-spines on
;; the reduced struct). If lex grows with n and strsexp stays flat, the closure value is
;; the cost.
(define buf (bundle char-smr strsexp-smr lex-smr linecol-smr))
(define one "(define (f x) (+ x 1))\n")
(define ((at k) L R) (cond [(< L k) 1] [(> L k) -1] [else 0]))

(define (prep n)                                   ; -> (values b r) at the char midpoint
  (define doc  (apply string-append (make-list n one)))
  (define rope ((make-rope buf) doc))
  (define k    (quotient (string-length doc) 2))
  ((multisect buf (vector (on (at k) char-smr))) rope))

(printf "\nread syntax context at a midpoint cut, by doc size (per-call us):\n")
(printf "~a  ~a  ~a  ~a  ~a\n"
        (~a "lines" #:min-width 7) (~a "chars" #:min-width 8)
        (~a "lex mode" #:min-width 11) (~a "strsexp spine" #:min-width 14) (~a "lex/strsexp" #:min-width 11))
(for ([n (in-list '(500 1000 2000 4000 8000 16000))])
  (define-values (b r) (prep n))
  (define chars (* n (string-length one)))
  (define lex-st (measure (lambda () ((lexv-step (lex-smr b)) 'code)) #:reps 50))
  (define sx-st  (measure (lambda () (strsexp-spines (strsexp-smr b) (strsexp-smr r))) #:reps 50))
  (define lx (* 1000.0 (stats-min lex-st)))        ; ms -> us
  (define sx (* 1000.0 (stats-min sx-st)))
  (printf "~a  ~a  ~a  ~a  ~a\n"
          (~a n #:min-width 7) (~a chars #:min-width 8)
          (~a (~r lx #:precision 2) #:min-width 11) (~a (~r sx #:precision 2) #:min-width 14)
          (~a (~r (/ lx (max sx 1e-6)) #:precision 1) #:min-width 11)))
