#lang racket

;; Step #3: cheaper leaf scanners. The leaf functions are re-run on every leaf a
;; `bisect`/`rope-split` carves (the navigation re-summarizes the split pieces), and
;; three of them lean on `regexp` / `string->list`, which allocate. Each rewrite below
;; is a SINGLE local procedure with an unchanged contract -- a clean drop-in into its
;; home (`summaries.rkt` linecol-leaf; `sexp-summary.rkt` tokens + transform):
;;
;;   linecol-leaf  regexp-split + a separate for/sum  ->  one index pass, no alloc
;;   sexp tokens   regexp-match* (list of substrings) ->  one index scan, emit first chars
;;   transform     string->list + cons-walk           ->  one index pass over the string
;;
;; The surrounding folds / combines are untouched, so faithfulness reduces to the swapped
;; piece. Each swap is proven against the real source: tokens/transform by output-equality
;; with the regexp originals, linecol by equality with the real `linecol-smr`, and the
;; composed fast smrs by (a) the law battery and (b) matching the REAL smrs on `sand-spines`
;; / `strsexp-spines` at every cut of a corpus -- the only interface the renderer reads.
;;
;; Run:        racket scratch/render-highlight/leaf-speedups.rkt          (A/B timings)
;; Correctness: racket -e '(require (submod ".../leaf-speedups.rkt" test))'

(require racket/match
         "../../rope-core.rkt"                ; make-summary make-rope
         "../../summaries/summaries.rkt"      ; char-smr linecol-smr (struct-out linecol) bundle
         "../../summaries/sexp-summary.rkt"   ; REAL sexp-smr strsexp-smr sand-spines malformed? strsexp-*
         "highlight.rkt"                      ; make-kw-smr
         "vbundle.rkt")                       ; make-vbundle (compose the two levers)

;; ============================================================================
;; copied non-exported internals (verbatim from sexp-summary.rkt / summaries.rkt),
;; with the ONE swapped procedure offered in /orig and /fast forms.
;; ============================================================================

(struct sexp-val (head closes forms opens tail) #:transparent)
(define brackets '((#\( . #\)) (#\[ . #\]) (#\{ . #\})))
(define openers (map car brackets))            ; hoist the (map cdr brackets) alloc out of close?
(define closers (map cdr brackets))
(define (open?  c) (and (memv c openers) #t))
(define (close? c) (and (memv c closers) #t))
(define (matching o c) (eqv? (cdr (assv o brackets)) c))

;; ---------- SWAP 1: the tokenizer ----------
(define (tokens/orig str)                      ; the regexp original
  (for/list ([t (in-list (regexp-match* #px"[][(){}]|[^][(){}\\s]+" str))]) (string-ref t 0)))
(define (tokens/fast str)                      ; one index scan: brackets are 1-char tokens;
  (define n (string-length str))               ; an atom run emits its first char then is skipped
  (let loop ([i 0] [acc '()])
    (if (= i n) (reverse acc)
        (let ([c (string-ref str i)])
          (cond
            [(or (open? c) (close? c)) (loop (add1 i) (cons c acc))]
            [(char-whitespace? c)      (loop (add1 i) acc)]
            [else (let skip ([j (add1 i)])
                    (if (and (< j n)
                             (let ([d (string-ref str j)])
                               (not (or (open? d) (close? d) (char-whitespace? d)))))
                        (skip (add1 j))
                        (loop j (cons c acc))))])))))

;; the sexp leaf fold (verbatim), parameterized by its tokenizer
(define ((sexp-leaf/with tokens) s)
  (define (class c) (cond [(open? c) 'open] [(close? c) 'close] [(char-whitespace? c) 'ws] [else 'atom]))
  (and (positive? (string-length s))
       (let loop ([toks (tokens s)] [closes '()] [forms 0] [opens '()])
         (match toks
           ['() (sexp-val (class (string-ref s 0)) (reverse closes) forms opens
                          (class (string-ref s (sub1 (string-length s)))))]
           [(cons c rest)
            (define (go stack)
              (if (null? stack)
                  (loop rest closes (add1 forms) stack)
                  (loop rest closes forms (cons (cons (caar stack) (add1 (cdar stack))) (cdr stack)))))
            (cond
              [(open? c)                 (loop rest closes forms (cons (cons c 1) opens))]
              [(not (close? c))          (go opens)]
              [(null? opens)             (loop rest (cons (cons c (- (add1 forms))) closes) 1 opens)]
              [(matching (caar opens) c) (go (cdr opens))]
              [else                      'malformed])]))))
(define sexp-leaf/orig (sexp-leaf/with tokens/orig))
(define sexp-leaf/fast (sexp-leaf/with tokens/fast))

;; the sexp combine (verbatim)
(define (sexp+ x y)
  (define (drop-start y)
    (match y
      [(sexp-val _ (cons (cons k cv) rest) _ _ _) (struct-copy sexp-val y [closes (cons (cons k (add1 cv)) rest)])]
      [_ (struct-copy sexp-val y [forms (sub1 (sexp-val-forms y))])]))
  (define (add-inner stack n) (cons (cons (caar stack) (+ (cdar stack) n)) (cdr stack)))
  (define (merge x y)
    (match-define (sexp-val xh xc xf xo _) x)
    (match-define (sexp-val _  yc yf yo yt) y)
    (let loop ([forms xf] [stack xo] [closes yc] [out '()])
      (match closes
        [(cons (cons k cv) rest)
         (cond
           [(null? stack)             (loop 0 stack rest (cons (cons k (- cv forms)) out))]
           [(matching (caar stack) k) (loop forms (cdr stack) rest out)]
           [else                      'malformed])]
        ['() (sexp-val xh (append xc (reverse out))
                       (if (null? stack) (+ forms yf) forms)
                       (if (null? stack) yo (append yo (if (zero? yf) stack (add-inner stack yf))))
                       yt)])))
  (cond
    [(eq? x 'malformed) 'malformed]
    [(eq? y 'malformed) 'malformed]
    [(not x) y] [(not y) x]
    [else (merge x (if (and (eq? (sexp-val-tail x) 'atom) (eq? (sexp-val-head y) 'atom)) (drop-start y) y))]))

;; readers + sand-spines (verbatim) -- the cross-check reads the fast smr through these
(define (sexp-opens  v) (if (sexp-val? v) (sexp-val-opens  v) '()))
(define (sexp-closes v) (if (sexp-val? v) (sexp-val-closes v) '()))
(define (sexp-forms  v) (if (sexp-val? v) (sexp-val-forms  v) 0))
(define (sexp-head v) (and (sexp-val? v) (sexp-val-head v)))
(define (sexp-tail v) (and (sexp-val? v) (sexp-val-tail v)))
(define (cut-kind L R)
  (case (sexp-head R)
    [(atom)  (if (eq? (sexp-tail L) 'atom) 'mid 'start)]
    [(open)  'start]
    [(close) 'end]
    [(ws)    'lean]
    [else    'end]))
(define (sand-spines/s L R)
  (match-define (cons fh fr) (append (map (lambda (e) (sub1 (cdr e))) (sexp-opens L)) (list (sexp-forms L))))
  (match-define (cons bh br) (append (map cdr (sexp-closes R)) (list (- (add1 (sexp-forms R))))))
  (case (cut-kind L R)
    [(start end) (values (cons fh fr)         (cons bh br))]
    [(mid)       (values (cons (- fh 1/2) fr) (cons (+ bh 1/2) br))]
    [(lean)      (values (cons (- fh 1/2) fr) (cons (- bh 1/2) br))]))

;; ---------- SWAP 2: transform (strsexp) ----------
(struct cs (quotes code string) #:transparent)
(define (transform/orig s start-in-string?)    ; the string->list original
  (define out (open-output-string))
  (let loop ([chs (string->list s)] [in? start-in-string?])
    (cond
      [(null? chs) (get-output-string out)]
      [(char=? (car chs) #\")
       (if in? (write-char #\space out) (write-string " ~" out))
       (loop (cdr chs) (not in?))]
      [in?  (loop (cdr chs) in?)]
      [else (write-char (car chs) out) (loop (cdr chs) in?)])))
(define (transform/fast s start-in-string?)    ; index pass, no string->list
  (define n (string-length s))
  (define out (open-output-string))
  (let loop ([i 0] [in? start-in-string?])
    (cond
      [(= i n) (get-output-string out)]
      [(char=? (string-ref s i) #\")
       (if in? (write-char #\space out) (write-string " ~" out))
       (loop (add1 i) (not in?))]
      [in?  (loop (add1 i) in?)]
      [else (write-char (string-ref s i) out) (loop (add1 i) in?)])))

(define ((strsexp-leaf/with transform leaf) s)
  (cs (for/sum ([c (in-string s)] #:when (char=? c #\")) 1)
      (leaf (transform s #f))
      (leaf (transform s #t))))
(define strsexp-leaf/orig (strsexp-leaf/with transform/orig sexp-leaf/orig))
(define strsexp-leaf/fast (strsexp-leaf/with transform/fast sexp-leaf/fast))

(define (strsexp+ a b)                          ; verbatim
  (match-define (cs qa ca sa) a)
  (match-define (cs qb cb sb) b)
  (define flip? (odd? qa))
  (cs (+ qa qb)
      (sexp+ ca (if flip? sb cb))
      (sexp+ sa (if flip? cb sb))))
(define (strsexp-in-string?/s L) (odd? (cs-quotes L)))
(define (strsexp-spines/s L R)
  (sand-spines/s (cs-code L) (if (odd? (cs-quotes L)) (cs-string R) (cs-code R))))

;; ---------- SWAP 3: linecol-leaf ----------
(define (linecol-leaf/orig s)                   ; the regexp-split original
  (define segs (regexp-split #rx"\n" s))
  (linecol (string-length (first segs))
           (for/sum ([c (in-string s)] #:when (char=? c #\newline)) 1)
           (string-length (last segs))))
(define (linecol-leaf/fast s)                   ; one pass: head = chars before 1st \n; cols = chars after last
  (define n (string-length s))
  (let loop ([i 0] [lines 0] [head -1] [last-nl -1])
    (cond
      [(= i n) (linecol (if (= head -1) n head) lines (- n 1 last-nl))]
      [(char=? (string-ref s i) #\newline)
       (loop (add1 i) (add1 lines) (if (= head -1) i head) i)]
      [else (loop (add1 i) lines head last-nl)])))
(define (linecol+ x y)                           ; verbatim
  (match-let ([(linecol xh xl xc) x] [(linecol yh yl yc) y])
    (linecol (if (zero? xl) (+ xh yh) xh)
             (+ xl yl)
             (if (zero? yl) (+ xc yc) yc))))

;; ---------- the smrs built from each leaf side ----------
(define sexp-smr/orig    (make-summary sexp-leaf/orig    sexp+))     ; scratch copy, to validate the copy
(define sexp-smr/fast    (make-summary sexp-leaf/fast    sexp+))
(define strsexp-smr/fast (make-summary strsexp-leaf/fast strsexp+))
(define linecol-smr/fast (make-summary linecol-leaf/fast linecol+))

;; ============================================================================
;; correctness
;; ============================================================================
(define corpus
  (list "" " " "a" "ab cd" "a\nb\n" "\n\n" "  ab  " "x\ny z\nw" "\n" "a\nb" "abc"
        "(define (fact n) (if (zero? n) 1 (* n (fact (sub1 n)))))"
        "(let ([x 1] [y 2]) (+ x y))" "(cond [(a) b] [else c])" "(f [g {h}])"
        "([])" "[()]" "{[()]}" "(a]" "[(])" "{[(])}" "[a)" "[" "])" "}])" "(]" "([)]"
        "(display \"hello (world)\")" "(a \"(\" b)" "\"(\"" "(\"))((\")" "\"unclosed ("
        "x \"y z\" w" "(define s \"hi (there)\")" "tail \" mid ( \" end"
        "((((" "))))" "atom" "(((x)))" "q) cc)" "((a b) (c d))"))

;; all single cuts of the corpus -- a deterministic L/R sweep for the spine cross-checks
(define cuts
  (for*/list ([s (in-list corpus)] [i (in-range 0 (add1 (string-length s)))])
    (cons (substring s 0 i) (substring s i))))
;; every prefix/suffix, for the self-contained tokenizer / transform / linecol proofs
(define fragments (remove-duplicates (append corpus (map car cuts) (map cdr cuts))))

(module+ test
  (require rackunit rackcheck "../../summaries/summary-laws.rkt")

  ;; --- SWAP proofs: each fast piece matches its regexp/list original byte-for-byte ---
  (for ([s (in-list fragments)])
    (check-equal? (tokens/fast s)    (tokens/orig s)    (format "tokens ~s" s))
    (check-equal? (transform/fast s #f) (transform/orig s #f) (format "transform code ~s" s))
    (check-equal? (transform/fast s #t) (transform/orig s #t) (format "transform str ~s" s))
    (check-equal? (linecol-leaf/fast s) (linecol-smr s)  (format "linecol ~s" s)))

  ;; --- the scratch verbatim copies match the REAL smrs (validates sexp+, sand-spines, etc.) ---
  ;; and the fast leaves match too, on the navigation interface at every cut.
  (for ([lr (in-list cuts)])
    (match-define (cons L R) lr)
    (define-values (rf rb) (sand-spines (sexp-smr L) (sexp-smr R)))         ; real
    (define-values (of ob) (sand-spines/s (sexp-smr/orig L) (sexp-smr/orig R)))  ; scratch-orig
    (define-values (ff fb) (sand-spines/s (sexp-smr/fast L) (sexp-smr/fast R)))  ; scratch-fast
    (check-equal? (list of ob) (list rf rb) (format "orig sand ~s|~s" L R))
    (check-equal? (list ff fb) (list rf rb) (format "fast sand ~s|~s" L R))
    ;; strsexp interface: in-string? and spines, fast vs real
    (check-equal? (strsexp-in-string?/s (strsexp-smr/fast L)) (strsexp-in-string? (strsexp-smr L))
                  (format "fast in-string? ~s" L))
    (define-values (srf srb) (strsexp-spines (strsexp-smr L) (strsexp-smr R)))
    (define-values (sff sfb) (strsexp-spines/s (strsexp-smr/fast L) (strsexp-smr/fast R)))
    (check-equal? (list sff sfb) (list srf srb) (format "fast strsexp-spines ~s|~s" L R)))

  ;; --- the law battery on each fast smr (a lawful monoid + measure homomorphism) ---
  (define gen:text (gen:string (gen:one-of (string->list "ab \n(){}[]\"~")) #:max-length 16))
  (check-summary-laws sexp-smr/fast    gen:text #:corpus corpus)
  (check-summary-laws strsexp-smr/fast gen:text #:corpus corpus)
  (check-summary-laws linecol-smr/fast gen:text #:corpus corpus)
  (printf "correctness: all swaps faithful (tokens/transform/linecol == original; fast smrs == real on spines + laws)\n"))

;; ============================================================================
;; timings
;; ============================================================================
(module+ main
  (require rackunit)
  ;; quick correctness gate before timing
  (for ([s (in-list fragments)])
    (check-equal? (tokens/fast s) (tokens/orig s))
    (check-equal? (transform/fast s #f) (transform/orig s #f))
    (check-equal? (linecol-leaf/fast s) (linecol-smr s)))
  (printf "correctness gate: tokens/transform/linecol fast == original  ok\n\n")

  (define (ns label iters thunk)
    (thunk) (collect-garbage)
    (define-values (_ c r g) (time-apply (lambda () (for ([i (in-range iters)]) (thunk))) '()))
    (printf "  ~a ~a ns/op   gc ~a ms\n"
            (~a label #:min-width 26)
            (~a (~r (/ (* r 1e6) iters) #:precision 1) #:min-width 8 #:align 'right) g))

  ;; representative leaf text: a ~26-char line with brackets, atoms, and a string literal
  ;; (so the strsexp transform exercises both modes). Leaf texts are <= max-leaf (32).
  (define line "(define (f x) \"hi (y)\" 1)")
  (define N 300000)

  (printf "tokenizer  (tokens line)            [~a chars]\n" (string-length line))
  (ns "regexp-match*" N (lambda () (tokens/orig line)))
  (ns "index scan"    N (lambda () (tokens/fast line)))

  (printf "\ntransform  (transform line #f):\n")
  (ns "string->list" N (lambda () (transform/orig line #f)))
  (ns "index pass"   N (lambda () (transform/fast line #f)))

  (printf "\nlinecol-leaf  (linecol-leaf line):\n")
  (ns "regexp-split" N (lambda () (linecol-leaf/orig line)))
  (ns "index pass"   N (lambda () (linecol-leaf/fast line)))

  (printf "\nsexp-leaf  (full leaf, tokenizer swapped):\n")
  (ns "regexp tokens" N (lambda () (sexp-leaf/orig line)))
  (ns "index tokens"  N (lambda () (sexp-leaf/fast line)))

  (printf "\nstrsexp-leaf  (full leaf, transform+tokens swapped):\n")
  (ns "regexp+list" N (lambda () (strsexp-leaf/orig line)))
  (ns "index"       N (lambda () (strsexp-leaf/fast line)))

  ;; ---------- end-to-end: rope construction (the leaf+combine-bound work) ----------
  ;; Per-line render / navigation can't be A/B'd from scratch: the renderer reads slots
  ;; through the canonical linecol-smr / strsexp-smr OBJECTS (the bundle is identity-keyed),
  ;; so a fast-leaf bundle -- built from NEW smr objects -- would miss those reads. The real
  ;; drop-in is a BODY swap inside the existing smr (same object), which keeps every reader
  ;; working; that swap's faithfulness is exactly what the `test` submodule proves. What we
  ;; CAN measure here is rope build, which runs leaves+combines with no external slot reads.
  (define kws '("define" "lambda" "let" "if" "cond"))
  (define kw-smr (make-kw-smr kws))
  (define snip "(define (fib n)\n  (if (< n 2)\n      n\n      (+ (fib (- n 1))\n         (fib (- n 2)))))\n")
  (define big  (apply string-append (make-list 800 snip)))      ; 4000 lines
  (define buf-orig (bundle      char-smr kw-smr strsexp-smr      linecol-smr))
  (define buf-fast (bundle      char-smr kw-smr strsexp-smr/fast linecol-smr/fast))
  (define buf-both (make-vbundle char-smr kw-smr strsexp-smr/fast linecol-smr/fast))
  ;; build correctness: cached char/line metrics agree (each read via its own component smr)
  (let ([ro ((make-rope buf-orig) big)] [rf ((make-rope buf-fast) big)])
    (unless (and (= (char-smr ro) (char-smr rf))
                 (equal? (linecol-smr ro) (linecol-smr/fast rf)))
      (error "rope build disagree")))
  (printf "\nrope build correctness: fast leaves == orig (char + linecol metrics)  ok\n")

  (printf "\nrope build  ((make-rope buf) big)  [4000 lines]:\n")
  (ns "orig leaves"          20 (lambda () ((make-rope buf-orig) big)))
  (ns "fast leaves"          20 (lambda () ((make-rope buf-fast) big)))
  (ns "fast leaves + vector" 20 (lambda () ((make-rope buf-both) big))))
