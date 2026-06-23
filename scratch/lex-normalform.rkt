#lang racket

;; SCRATCH: a finite NORMAL-FORM lexer summary, replacing the closure-valued `lex-smr`
;; of keyword-highlight.rkt. The closure version reads the entry-mode at a cut in O(left)
;; (it re-lexes every leaf to the left). This version reduces each fragment to a finite
;; value, so the read is O(1) and combine is O(block-nesting depth) -- the same shape as
;; the sexp summary.
;;
;; The value (struct LX): a fragment's whole transition function, finitely:
;;   fc fs fl : exit mode entered in code / string / line   (the flat finite-state part)
;;   a  b     : block-comment counter -- closers escaping left, openers left open right
;;              (single bracket kind #| |#, so two ints, no stack)
;;   cpl      : per-level coupling -- cpl[k-1] = exit mode entered in a block at depth k,
;;              for the depths 1..a that fall out (the O(depth) piece; the suffix the
;;              close re-exposes as code, which differs per depth -- the |# " |# example).
;; A mode is 'code | 'string | 'line | (cons 'block d).  apply-step reads step_F(m) in
;; O(1); lex2+ composes two LXs in O(a) = O(depth).
;;
;; WARNING -- cpl/a degenerate to O(document) on a DAG of unbalanced fragments.
;;   `a` (= cpl length) is the count of left-unmatched block-closers, and the combine SUMS
;;   them: a(XY) = aX + max(0, aY - bX).  For real input `a` is bounded by the actual block
;;   nesting depth (0-2), so cpl is tiny and the read is O(1).  But if you build a rope that
;;   REPEATS one unbalanced fragment (a>0) 2^d times -- e.g. the DAG trick branch(h,h) --
;;   the dangling closers never cancel, so a roughly DOUBLES per level: a = O(2^d) = O(N).
;;   cpl is then a vector of length O(N), and forcing lex2 builds it -> O(N) per read, GBs
;;   of allocation (seen: a = 4,13,49,193,769,3073 over the DAG; lex2 read 16s at d=24).
;;   This is the "combine is O(depth)" caveat made concrete: a DAG makes the effective depth
;;   O(N) by stacking unmatched closers, NOT by real nesting.  Don't share-repeat unbalanced
;;   fragments under lex2; a genuine document keeps a small and stays O(1).  (See
;;   huge-doc-cost.rkt, which sidesteps it by repeating a balanced block whose a stays 0.)
;;
;; lex-scan (the ground-truth scanner) + the closure monoid are COPIED VERBATIM from
;; keyword-highlight.rkt, as the oracle to validate against and the baseline to bench.
;;
;; Run:  racket scratch/lex-normalform.rkt

(require racket/match
         "../rope-core.rkt"            ; make-rope make-summary multisect
         "../summaries/summaries.rkt"      ; bundle char-smr linecol-smr
         "../summaries/sexp-summary.rkt"   ; strsexp-smr
         "../helper-algebras.rkt"     ; on
         "../bench/bench.rkt")        ; measure (struct-out stats)

(provide lex2-smr apply-step lex-scan mode-exit lex2-leaf lex2+ (struct-out LX))

;; ===== VERBATIM from keyword-highlight.rkt: the scanner =====
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

;; closure monoid (the O(left) baseline)
(struct lexv (step) #:transparent)
(define (exit-of s m) (let-values ([(_ ex) (lex-scan s m)]) ex))
(define (lex-leaf s) (lexv (lambda (m) (exit-of s m))))
(define (lex+ a b)   (lexv (lambda (m) ((lexv-step b) ((lexv-step a) m)))))
(define lex-smr (make-summary lex-leaf lex+))
;; ===== end verbatim =====

;; ===== the normal-form monoid =====
;; a mode: 'code | 'string | 'line | (cons 'block d).  shebang folds to 'line (same
;; continuation -- runs to newline, then code -- and only ever occurs at doc start).
(define (mode-exit s entry)
  (let-values ([(_ ex) (lex-scan s entry)]) (if (eq? ex 'shebang) 'line ex)))

;; block accounting: scan #| (+1) / |# (-1) as one bracket kind.  a = closers escaping
;; left (= -min prefix sum), total = net, positions[k-1] = char index just after the
;; k-th left-unmatched closer (where entry-depth k falls out to code).
(define (block-scan s)
  (define n (string-length s))
  (define (two? j a b) (and (< (add1 j) n) (char=? (string-ref s j) a) (char=? (string-ref s (add1 j)) b)))
  (let loop ([j 0] [sum 0] [curmin 0] [positions '()])
    (cond
      [(>= j n) (values (- curmin) sum (list->vector (reverse positions)))]
      [(two? j #\# #\|) (loop (+ j 2) (add1 sum) curmin positions)]
      [(two? j #\| #\#)
       (define s2 (sub1 sum))
       (if (< s2 curmin)
           (loop (+ j 2) s2 s2 (cons (+ j 2) positions))
           (loop (+ j 2) s2 curmin positions))]
      [else (loop (add1 j) sum curmin positions)])))

(struct LX (fc fs fl a b cpl) #:transparent)

(define (lex2-leaf s)
  (define-values (a total positions) (block-scan s))
  (LX (mode-exit s 'code) (mode-exit s 'string) (mode-exit s 'line)
      a (+ total a)
      (for/vector ([p (in-vector positions)]) (mode-exit (substring s p) 'code))))

;; step_V(m) in O(1): flat modes are table lookups; a block entry either stays in the
;; block (depth arithmetic off a,b) or falls out (the coupling list).
(define (apply-step V m)
  (cond
    [(eq? m 'code)   (LX-fc V)]
    [(eq? m 'string) (LX-fs V)]
    [(eq? m 'line)   (LX-fl V)]
    [else (define d (cdr m))
          (if (> d (LX-a V))
              (cons 'block (+ (- d (LX-a V)) (LX-b V)))
              (vector-ref (LX-cpl V) (sub1 d)))]))

;; compose two transition functions, re-extracting the normal form.  O(a) = O(depth):
;; flat fields + counter are O(1); the coupling rebuilds one entry per fall-out depth.
(define (lex2+ X Y)
  (define aX (LX-a X)) (define bX (LX-b X))
  (define aY (LX-a Y)) (define bY (LX-b Y))
  (define matched (min bX aY))
  (define a (+ aX (- aY matched)))
  (LX (apply-step Y (LX-fc X)) (apply-step Y (LX-fs X)) (apply-step Y (LX-fl X))
      a (+ bY (- bX matched))
      (for/vector ([k (in-range 1 (add1 a))])
        (apply-step Y (if (<= k aX)
                          (vector-ref (LX-cpl X) (sub1 k))
                          (cons 'block (+ (- k aX) bX)))))))

(define lex2-smr (make-summary lex2-leaf lex2+))
;; ===== end normal-form monoid =====

;; ---------- correctness ----------
;; The claim is "normal form == the closure monoid, but O(depth) not O(left)".  So the
;; oracle is the CLOSURE monoid (the baseline being replaced), over every chunking --
;; both share whatever the lexer's per-leaf scan does, including its blind spot for a
;; 2-char token (#| |#) split across a leaf boundary (a separate seam-handling concern,
;; the kw-smr lead/trail pattern, that neither monoid does yet).  The SCANNER oracle is
;; used only on whole, unsplit strings (one leaf), which validates block-scan's a/b/cpl.
(define (chunks s k)
  (for/list ([i (in-range 0 (max 1 (string-length s)) k)])
    (substring s i (min (string-length s) (+ i k)))))
(define (fold-fn f leafs init)
  (if (null? leafs) init (foldl (lambda (c acc) (f acc c)) (car leafs) (cdr leafs))))
(define (nf-of  s k) (fold-fn lex2+ (map lex2-leaf (chunks s k)) (lex2-leaf "")))
(define (clo-of s k) (fold-fn lex+  (map lex-leaf  (chunks s k)) (lex-leaf  "")))
(define (clo-step V m) (let ([e ((lexv-step V) m)]) (if (eq? e 'shebang) 'line e)))

(define corpus
  (list "(define x 1) ; the x slot"
        "#| outer #| inner |# still |# (define y 2)"
        "(define s \"hi\") ; greet (x)\n(if a (lambda () b))"
        "|# x |#" "|# \" |#" "|# \" |# x" "#| a |# b" "|# |# \"" "|# a |# \" |# b"
        "\"abc" "; line\nmore" "#| unterminated" "code \"str #| not |#\" more"
        "a #| b \" c |# d" "#| #| #|" "|# |# |#" "x" "" "\"" "#|" "|#"
        "#| close \" reopen \"" "((deep \"s\" #| c |#))"))
(define test-modes (list 'code 'string 'line '(block . 1) '(block . 2) '(block . 3)))

;; ---------- correctness + bench (racket scratch/lex-normalform.rkt) ----------
(module+ main
  ;; (1) normal form == closure, over all chunkings (the faithful-drop-in test)
  (define eqv-cases 0)
  (for* ([s (in-list corpus)] [m (in-list test-modes)] [k (in-range 1 7)])
    (set! eqv-cases (add1 eqv-cases))
    (define nf  (apply-step (nf-of s k) m))
    (define clo (clo-step (clo-of s k) m))
    (unless (equal? nf clo)
      (error 'faithful "s=~s entry=~s chunk=~a : normalform ~s != closure ~s" s m k nf clo)))

  ;; (2) one leaf == scanner (no split tokens) -- validates block-scan's a/b/cpl
  (for* ([s (in-list corpus)] [m (in-list test-modes)])
    (define nf (apply-step (lex2-leaf s) m))
    (unless (equal? nf (mode-exit s m))
      (error 'scanner "s=~s entry=~s : leaf ~s != scanner ~s" s m nf (mode-exit s m))))

  (printf "correctness: ~a cases normalform==closure (all chunkings); leaf==scanner on ~a strings\n\n"
          eqv-cases (length corpus))

  ;; ---------- 1. component micro ----------
  (define line "(define (fact n) (if (zero? n) 1 (* n (fact (sub1 n)))))")
  (define mid  (quotient (string-length line) 2))
  (define (ns label iters thunk)
    (thunk) (collect-garbage)
    (define-values (_ cpu real gc) (time-apply (lambda () (for ([i (in-range iters)]) (thunk))) '()))
    (printf "  ~a ~a ns/op\n" (~a label #:min-width 24) (~a (~r (/ (* real 1e6) iters) #:precision 1) #:min-width 9 #:align 'right)))
  (define (cb smr) (let ([a (smr (substring line 0 mid))] [b (smr (substring line mid))]) (lambda () (smr a b))))
  (printf "component combine (smr a b):\n")
  (ns "strsexp-smr"           300000 (cb strsexp-smr))
  (ns "lex-smr (closure)"     300000 (cb lex-smr))
  (ns "lex2-smr (normalform)" 300000 (cb lex2-smr))
  (printf "leaf (smr s):\n")
  (ns "strsexp-smr"           200000 (lambda () (strsexp-smr line)))
  (ns "lex-smr (closure)"     200000 (lambda () (lex-smr line)))
  (ns "lex2-smr (normalform)" 200000 (lambda () (lex2-smr line)))

  ;; ---------- 2. THE scaling test: read the entry mode at a midpoint cut ----------
  (define buf (bundle char-smr strsexp-smr lex-smr lex2-smr linecol-smr))
  (define one "(define (f x) (+ x 1))\n")
  (define ((at k) L R) (cond [(< L k) 1] [(> L k) -1] [else 0]))
  (define (prep n)
    (define doc  (apply string-append (make-list n one)))
    (define rope ((make-rope buf) doc))
    (define k    (quotient (string-length doc) 2))
    ((multisect buf (vector (on (at k) char-smr))) rope))

  (printf "\nread entry-mode at a midpoint cut, by doc size (per-call us):\n")
  (printf "~a  ~a  ~a  ~a  ~a\n"
          (~a "lines" #:min-width 7) (~a "chars" #:min-width 8)
          (~a "closure" #:min-width 11) (~a "normalform" #:min-width 12) (~a "strsexp" #:min-width 10))
  (for ([n (in-list '(500 1000 2000 4000 8000 16000))])
    (define-values (b r) (prep n))
    (define m-clo ((lexv-step (lex-smr b)) 'code))
    (define m-nf  (apply-step (lex2-smr b) 'code))
    (unless (equal? m-clo m-nf) (error "read disagreement" m-clo m-nf))
    (define clo (* 1000.0 (stats-min (measure (lambda () ((lexv-step (lex-smr b)) 'code)) #:reps 20))))
    (define nf  (* 1000.0 (stats-min (measure (lambda () (apply-step (lex2-smr b) 'code)) #:reps 3000))))
    (define sx  (* 1000.0 (stats-min (measure (lambda () (strsexp-smr b)) #:reps 3000))))
    (printf "~a  ~a  ~a  ~a  ~a\n"
            (~a n #:min-width 7) (~a (* n (string-length one)) #:min-width 8)
            (~a (~r clo #:precision 2) #:min-width 11) (~a (~r nf #:precision 3) #:min-width 12)
            (~a (~r sx #:precision 3) #:min-width 10))))

;; ---------- summary-law battery (racket -e '(require (submod "scratch/lex-normalform.rkt" laws))') ----------
;; The kit (summaries/summary-laws.rkt) compares values with equal?, so it can only check a
;; summary whose values have a structural equal?.  The closure lexv's value is a lambda --
;; two lambdas are never equal? unless eq? -- so the closure monoid is NOT law-checkable;
;; the finite LX struct (transparent) is.  Laws split into two independent groups:
;;   SUMMARY group (identity, associativity) -- the combine/unit ALGEBRA;
;;   STRING group  (homomorphism)            -- the measure respecting concatenation.
(module+ laws
  (require rackcheck rackunit "../summaries/summary-laws.rkt")

  ;; flat domain: a realistic alphabet WITHOUT the 2-char tokens (#| |# #!), so strings,
  ;; line comments, parens and atoms are exercised and no cut can split a token.
  (define gen:flat (gen:string (gen:one-of (string->list "abc() \";\n")) #:max-length 14))
  (define corpus-flat (list "(define x)" "\"hi\"" "a ; c\nb" "(\"s\")" "; only" "\"open" "()" "" "abc"))

  ;; block domain: tokens incl #| |#, for the ALGEBRA laws (identity/associativity hold
  ;; for combine regardless of split tokens -- splitting is a measure/leaf issue).
  (define gen:blocks
    (gen:map (gen:list (gen:one-of '("#|" "|#" "(" ")" "x" " " "\"" ";")) #:max-length 8)
             (lambda (xs) (apply string-append xs))))

  (printf "lex2-smr (normal form) vs the summary-law battery:\n\n")

  (printf "  flat domain (strings/line-comments/parens -- no #| |# tokens):\n")
  (check-summary-laws lex2-smr gen:flat #:corpus corpus-flat)
  (printf "    identity + associativity + homomorphism : PASS\n\n")

  (printf "  block domain (#| |# present) -- SUMMARY group only:\n")
  (check-property (make-config) (law:identity lex2-smr gen:blocks))
  (check-property (make-config) (law:associativity lex2-smr gen:blocks))
  (printf "    identity + associativity : PASS\n\n")

  (printf "  homomorphism (STRING group) on block tokens -- holds off-token, fails on a split:\n")
  (for ([c (list (list "#| a |#" '(4))     ; cut between tokens
                 (list "#| a |#" '(0 7))    ; trivial cuts
                 (list "#|"      '(1))       ; splits #|  ->  "#" + "|"
                 (list "x|#y"    '(2)))])    ; splits |#  ->  "x|" + "#y"
    (match-define (list s is) c)
    (printf "    ~a cut at ~s : ~a\n" (~s s #:min-width 10) is
            (if (homomorphism-law? lex2-smr s is) "holds" "FAILS (split token)")))

  (printf "\n  the closure lexv is not even law-checkable (lambdas aren't equal?):\n")
  (define x (lex-smr "a")) (define y (lex-smr "b")) (define z (lex-smr "c"))
  (printf "    associativity-law? closure    : ~a\n" (associativity-law? lex-smr x y z))
  (printf "    associativity-law? normalform : ~a\n"
          (associativity-law? lex2-smr (lex2-smr "a") (lex2-smr "b") (lex2-smr "c"))))
