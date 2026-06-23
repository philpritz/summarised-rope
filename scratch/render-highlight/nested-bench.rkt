#lang racket

;; Re-run the A vs current-renderer comparison on a HIGHLY NESTED sexp document, using
;; the sexp generator from sexp-summary.rkt's test submodule (copied here -- it isn't
;; provided), cranked to deep nesting.  Deep nesting lengthens the opens/closes stacks,
;; so the strsexp combine does more work -- which is exactly the slot a lazy bundle would
;; skip during navigation.  So the lazy-bundle ceiling (full vs char+linecol nav) should
;; WIDEN on nested input vs the flat fib snippet.
;;
;; Run:  racket scratch/render-highlight/nested-bench.rkt

(require rackcheck racket/match
         "../../rope-core.rkt"               ; make-rope
         "../../summaries/summaries.rkt"     ; bundle char-smr linecol-smr
         "../../summaries/sexp-summary.rkt"  ; strsexp-smr
         "renderer.rkt"                      ; render open-doc (struct hl)
         "nav-strategies.rkt")               ; heads/A render-A buf kws kw-smr

;; ---------- the generator (verbatim from sexp-summary.rkt's test submodule) ----------
(define gen:word    (gen:one-of '("define" "lambda" "let" "if" "cons" "x" "xs" "foo")))
(define gen:keyword (gen:one-of '("define" "lambda" "let" "if" "+" "list" "cond")))
(define gen:ident
  (gen:let ([c    gen:char-letter]
            [head (gen:string gen:char-letter #:max-length 3)]
            [tail (gen:list (gen:let ([c2 gen:char-letter]
                                      [s2 (gen:string gen:char-letter #:max-length 3)])
                              (string-append (string c2) s2))
                            #:max-length 2)]
            [sfx  (gen:one-of '("" "" "?" "!" "*"))])
    (string-append (string-join (cons (string-append (string c) head) tail) "-") sfx)))
(define gen:number (gen:map gen:natural number->string))
(define gen:atom   (gen:frequency `((4 . ,gen:word) (2 . ,gen:ident) (1 . ,gen:number))))
(define gen:ws (gen:one-of '(" " " " " " "  " "\n" "\n  " "")))
(define (gen:node max-kids d)
  (gen:let ([kids (gen:frequency
                   `((1 . ,(gen:const '()))
                     (5 . ,(gen:let ([h gen:keyword]
                                     [r (gen:list (if (zero? d)
                                                      gen:atom
                                                      (gen:tree max-kids d))
                                                  #:max-length (sub1 max-kids))])
                             (cons h r)))))]
            [seps (apply gen:tuple (make-list (max 0 (sub1 (length kids))) gen:ws))])
    (list kids seps)))
(define (gen:tree max-kids d)
  (if (zero? d)
      gen:atom
      (gen:frequency `((2 . ,gen:atom) (2 . ,(gen:node max-kids (sub1 d)))))))
(define (render-sexp t)
  (if (string? t)
      t
      (let loop ([ks (first t)] [seps (second t)] [acc ""])
        (cond
          [(null? ks)       (string-append "(" acc ")")]
          [(null? (cdr ks)) (string-append "(" acc (render-sexp (car ks)) ")")]
          [else
           (define a    (car ks))
           (define sep  (car seps))
           (define sep* (if (and (string? a) (string? (cadr ks)) (equal? sep ""))
                            " " sep))
           (loop (cdr ks) (cdr seps) (string-append acc (render-sexp a) sep*))]))))

;; deep variant of gen:sexp-doc: depth 10 instead of 3.
(define gen:deep (gen:map (gen:node 4 10) render-sexp))

;; ---------- doc characterization ----------
(define (line-count s) (add1 (for/sum ([c (in-string s)] #:when (char=? c #\newline)) 1)))
(define (max-paren-depth s)
  (let loop ([i 0] [d 0] [mx 0])
    (if (= i (string-length s)) mx
        (let ([d* (cond [(memv (string-ref s i) '(#\( #\[ #\{)) (add1 d)]
                        [(memv (string-ref s i) '(#\) #\] #\})) (sub1 d)]
                        [else d])])
          (loop (add1 i) d* (max mx d*))))))

;; build a >=4200-line nested doc: sample deep sexps, keep the nested multi-line ones,
;; join with blank lines, repeat to the target line count.
(define (nested-doc target-lines)
  (random-seed 20260621)
  (define samples (filter (lambda (s) (and (>= (line-count s) 3) (>= (max-paren-depth s) 6)))
                          (sample gen:deep 120)))
  (define chunk (string-join samples "\n\n"))
  (define reps  (max 1 (exact-ceiling (/ target-lines (line-count chunk)))))
  (string-join (make-list reps chunk) "\n\n"))

;; ---------- the comparison, on whatever doc text ----------
(define (ns label rows iters thunk)
  (thunk) (collect-garbage)
  (define-values (_ c r g) (time-apply (lambda () (for ([i (in-range iters)]) (thunk))) '()))
  (printf "  ~a ~a ms/screen  ~a us/line   gc ~a\n"
          (~a label #:min-width 26)
          (~a (~r (/ (exact->inexact r) iters) #:precision 2) #:min-width 7 #:align 'right)
          (~a (~r (/ (* r 1000.0) iters rows) #:precision 1) #:min-width 7 #:align 'right) g))

(define (bench label text)
  (define top 2000) (define rows 50)
  (define doc      ((make-rope buf) text))
  (define h        (hl kws kw-smr buf))
  (define z        (open-doc h text))
  (define buf-lite (bundle char-smr linecol-smr))
  (define doc-lite ((make-rope buf-lite) text))
  (unless (equal? (render h z top rows) (render-A h doc z top rows))
    (error 'bench "render != render-A for ~a" label))
  (printf "~a  --  ~a lines, max paren depth ~a  (render == render-A ok):\n"
          label (line-count text) (max-paren-depth text))
  (ns "current render (zipper)"   rows 100 (lambda () (render   h z   top rows)))
  (ns "render via A"              rows 100 (lambda () (render-A h doc z top rows)))
  (ns "A nav, full bundle"        rows 200 (lambda () (heads/A doc      top rows)))
  (ns "A nav, lite (char+lcol)"   rows 200 (lambda () (heads/A doc-lite top rows buf-lite)))
  (newline))

(module+ main
  (define fib (apply string-append
                     (make-list 800 "(define (fib n)\n  (if (< n 2)\n      n\n      (+ (fib (- n 1))\n         (fib (- n 2)))))\n")))
  (define nested (nested-doc 4200))
  (bench "flat fib snippet"      fib)
  (bench "nested generated sexp" nested))
