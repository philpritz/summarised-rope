#lang racket

(require racket/contract
         racket/match
         "zipper-core.rkt")

(provide
 text-summary-algebra
 guide/c
 char-count-algebra
 char-position
 word-count-algebra
 word-count
 word-start
 word-end
 row-column-algebra
 row-column
 sexp-frontier-algebra
 sexp-address
 zipper-sexp-address
 next-sexp-address
 previous-sexp-address
 parent-sexp-address
 relative-sexp
 parent-sexp
 before-sexp-guide
 after-sexp-guide)

(define guide/c
  (-> any/c any/c (or/c -1 0 1)))

(define/contract (text-summary-algebra leaf append #:empty [empty (leaf "")])
  (->* (procedure? procedure?) (#:empty any/c) summary-algebra?)
  (summary-algebra empty leaf append))

(define char-count-algebra
  (text-summary-algebra string-length +))

(define/contract (char-position n)
  (-> exact-nonnegative-integer? guide/c)
  (lambda (before after)
    (cond
      [(< before n) 1]
      [(> before n) -1]
      [else 0])))

(define (word? c)
  (or (char-alphabetic? c)
      (char-numeric? c)))

(define (word-leaf s)
  (and (positive? (string-length s))
       (for/fold ([starts? #f] [n 0] [ends? #f]
                  #:result (list starts? n ends?))
                 ([c s] [i (in-naturals)])
         (define w? (word? c))
         (values (if (zero? i) w? starts?)
                 (+ n (if (and w? (not ends?)) 1 0))
                 w?))))

(define (word+ x y)
  (or (and x y
           (match-let ([(list xa? xn xz?) x]
                       [(list ya? yn yz?) y])
             (list xa?
                   (- (+ xn yn) (if (and xz? ya?) 1 0))
                   yz?)))
      x
      y))

(define word-count-algebra
  (text-summary-algebra word-leaf word+))

(define (word-count x)
  (if x (second x) 0))

(define (word-starts? x)
  (and x (first x)))

(define (word-ends? x)
  (and x (third x)))

(define/contract (word-start n)
  (-> exact-nonnegative-integer? guide/c)
  (lambda (before after)
    (cond
      [(< (word-count before) n) 1]
      [(> (word-count before) n) -1]
      [(word-ends? before) 1]
      [(word-starts? after) 0]
      [else 1])))

(define/contract (word-end n)
  (-> exact-nonnegative-integer? guide/c)
  (let ([target (add1 n)])
    (lambda (before after)
      (cond
        [(< (word-count before) target) 1]
        [(> (word-count before) target) -1]
        [(word-starts? after) 1]
        [(word-ends? before) 0]
        [else -1]))))

(define (row-column-leaf s)
  (for/fold ([rows 0] [column 0] [first #f]
             #:result (list rows
                            (string-length s)
                            (or first column)
                            column))
            ([c s])
    (if (char=? c #\newline)
        (values (add1 rows) 0 (or first column))
        (values rows (add1 column) first))))

(define (row-column+ x y)
  (match-let ([(list xrows xchars xfirst xlast) x]
              [(list yrows ychars yfirst ylast) y])
    (list (+ xrows yrows)
          (+ xchars ychars)
          (if (zero? xrows) (+ xchars yfirst) xfirst)
          (if (zero? yrows) (+ xlast ychars) ylast))))

(define row-column-algebra
  (text-summary-algebra row-column-leaf row-column+))

(define/contract (row-column row column)
  (-> exact-nonnegative-integer? (or/c exact-nonnegative-integer? false/c) guide/c)
  (lambda (before after)
    (match-let ([(list before-row before-chars before-first before-column) before]
                [(list after-rows after-chars after-first after-column) after])
      (cond
        [(< before-row row) 1]
        [(> before-row row) -1]
        [(not column)
         (if (or (zero? after-chars)
                 (and (positive? after-rows) (zero? after-first)))
             0
             1)]
        [(< before-column column) 1]
        [(> before-column column) -1]
        [else 0]))))

(define (sexp-atom? c)
  (not (or (char-whitespace? c)
           (char=? c #\()
           (char=? c #\)))))

(define (sexp-form-start? c)
  (or (sexp-atom? c)
      (char=? c #\()))

(define (sexp-form-end? c)
  (or (sexp-atom? c)
      (char=? c #\))))

(define (bump-sexp forms opens)
  (if (null? opens)
      (values (add1 forms) opens)
      (values forms (cons (add1 (car opens)) (cdr opens)))))

(define (sexp-leaf s)
  (and (positive? (string-length s))
       (let-values
           ([(starts-atom? starts-form? closes forms opens in-atom? ends-atom? ends-form?)
             (for/fold ([starts-atom? (sexp-atom? (string-ref s 0))]
                        [starts-form? (sexp-form-start? (string-ref s 0))]
                        [closes '()]
                        [forms 0]
                        [opens '()]
                        [in-atom? #f]
                        [ends-atom? #f]
                        [ends-form? #f])
                       ([c s])
               (cond
                 [(sexp-atom? c)
                  (if in-atom?
                      (values starts-atom? starts-form? closes forms opens #t #t #t)
                      (let-values ([(forms opens) (bump-sexp forms opens)])
                        (values starts-atom? starts-form? closes forms opens #t #t #t)))]
                 [(char=? c #\()
                  (let-values ([(forms opens)
                                (if (null? opens)
                                    (values forms opens)
                                    (bump-sexp forms opens))])
                    (values starts-atom? starts-form? closes forms (cons 0 opens) #f #f #f))]
                 [(char=? c #\))
                  (if (null? opens)
                      (values starts-atom? starts-form? (cons forms closes) 0 opens #f #f #t)
                      (let ([opens (cdr opens)])
                        (values starts-atom? starts-form? closes
                                (if (null? opens) (add1 forms) forms)
                                opens #f #f #t)))]
                 [else
                  (values starts-atom? starts-form? closes forms opens #f #f #f)]))])
         (list starts-atom? starts-form?
               (reverse closes)
               forms
               (reverse opens)
               ends-atom? ends-form?))))

(define (drop-start-sexp-atom x)
  (match-let ([(list starts-atom? starts-form? closes forms opens ends-atom? ends-form?) x])
    (if (pair? closes)
        (list starts-atom? starts-form?
              (cons (sub1 (car closes)) (cdr closes))
              forms opens ends-atom? ends-form?)
        (list starts-atom? starts-form?
              closes (sub1 forms) opens ends-atom? ends-form?))))

(define (add-inner-sexp opens n)
  (match opens
    [(list x) (list (+ x n))]
    [(cons x xs) (cons x (add-inner-sexp xs n))]))

(define (merge-sexp-frontier forms opens closes right-forms right-opens)
  (let loop ([forms forms]
             [stack (reverse opens)]
             [closes closes]
             [out '()])
    (match closes
      ['()
       (if (null? stack)
           (values (reverse out) (+ forms right-forms) right-opens)
           (let* ([opens (reverse stack)]
                  [extra (+ right-forms (if (pair? right-opens) 1 0))]
                  [opens (if (zero? extra) opens (add-inner-sexp opens extra))])
             (values (reverse out) forms (append opens right-opens))))]
      [(cons close-count rest)
       (if (pair? stack)
           (let ([stack (cdr stack)])
             (loop (if (null? stack) (add1 forms) forms) stack rest out))
           (loop 0 stack rest (cons (+ forms close-count) out)))])))

(define (sexp+ x y)
  (or (and x y
           (match-let ([(list xa? xs? xc xf xo xz? xe?) x]
                       [(list ya? ys? yc yf yo yz? ye?) y])
             (define y* (if (and xz? ya?) (drop-start-sexp-atom y) y))
             (match-define (list ya2? ys2? yc2 yf2 yo2 yz2? ye2?) y*)
             (define-values (closes forms opens)
               (merge-sexp-frontier xf xo yc2 yf2 yo2))
             (list xa? xs? (append xc closes) forms opens yz2? ye2?)))
      x
      y))

(define sexp-frontier-algebra
  (text-summary-algebra sexp-leaf sexp+))

(define (valid-sexp-path? path)
  (and (pair? path)
       (andmap exact-nonnegative-integer? path)))

(define (sexp-summary-starts-atom? summary)
  (match summary
    [#f #f]
    [(list starts-atom? _starts-form? _closes _forms _opens _ends-atom? _ends-form?)
     starts-atom?]))

(define (sexp-summary-starts-form? summary)
  (match summary
    [#f #f]
    [(list _starts-atom? starts-form? _closes _forms _opens _ends-atom? _ends-form?)
     starts-form?]))

(define (sexp-summary-ends-atom? summary)
  (match summary
    [#f #f]
    [(list _starts-atom? _starts-form? _closes _forms _opens ends-atom? _ends-form?)
     ends-atom?]))

(define (sexp-summary-ends-form? summary)
  (match summary
    [#f #f]
    [(list _starts-atom? _starts-form? _closes _forms _opens _ends-atom? ends-form?)
     ends-form?]))

(define (sexp-next-address summary)
  (match summary
    [#f '(0)]
    [(list _starts-atom? _starts-form? _closes forms opens _ends-atom? _ends-form?)
     (define (opens->address opens)
       (match opens
         ['() '()]
         [(list innermost) (list (add1 innermost))]
         [(cons child-count rest)
          (cons child-count (opens->address rest))]))
     (cons forms (opens->address opens))]))

(define (next-sexp-address path)
  (match (drop-trailing-zero-addresses path)
    [(list index) (list (add1 index))]
    [(cons index rest) (cons index (next-sexp-address rest))]))

(define (previous-sexp-address path)
  (match (drop-trailing-zero-addresses path)
    [(list index)
     (if (zero? index)
         (error 'sexp-address "cannot move before first S-expression: ~v" path)
         (list (sub1 index)))]
    [(cons index rest) (cons index (previous-sexp-address rest))]))

(define (parent-sexp-address path)
  (define normalized (drop-trailing-zero-addresses path))
  (define (drop-last path)
    (match path
      [(list _last) '()]
      [(cons first rest) (cons first (drop-last rest))]))
  (match normalized
    [(list _index)
     (error 'parent-sexp-address "top-level S-expression has no parent: ~v" path)]
    [_ (drop-last normalized)]))

(define/contract (sexp-address left right)
  (-> any/c any/c valid-sexp-path?)
  (define next-path (sexp-next-address left))
  (if (and (sexp-summary-ends-atom? left)
           (sexp-summary-starts-atom? right))
      (previous-sexp-address next-path)
      next-path))

(define/contract (zipper-sexp-address z)
  (-> zipper? valid-sexp-path?)
  (gap-sides z sexp-address))

(define (drop-trailing-zero-addresses path)
  (define trimmed
    (let loop ([reversed (reverse path)])
      (match reversed
        [(cons 0 rest) (loop rest)]
        [_ (reverse reversed)])))
  (if (null? trimmed) '(0) trimmed))

(define (sexp-path-compare x y)
  (define x* (drop-trailing-zero-addresses x))
  (define y* (drop-trailing-zero-addresses y))
  (define (compare x y)
    (match* (x y)
    [('() '()) 0]
    [('() _) -1]
    [(_ '()) 1]
    [((cons x0 xs) (cons y0 ys))
     (cond
       [(< x0 y0) -1]
       [(> x0 y0) 1]
         [else (compare xs ys)])]))
  (compare x* y*))

(define/contract (before-sexp-guide path)
  (-> valid-sexp-path? guide/c)
  (lambda (before after)
    (case (sexp-path-compare (sexp-next-address before) path)
      [(-1) 1]
      [(1) -1]
      [(0)
       (if (and (sexp-summary-starts-form? after)
                (not (and (sexp-summary-ends-atom? before)
                          (sexp-summary-starts-atom? after))))
           0
           1)])))

(define/contract (after-sexp-guide path)
  (-> valid-sexp-path? guide/c)
  (define next-path (next-sexp-address path))
  (lambda (before after)
    (case (sexp-path-compare (sexp-next-address before) next-path)
      [(-1) 1]
      [(1) -1]
      [(0)
       (cond
         [(and (sexp-summary-ends-atom? before)
               (sexp-summary-starts-atom? after))
          1]
         [(sexp-summary-ends-form? before) 0]
         [else -1])])))

(define/contract (relative-sexp make-target-guide)
  (-> (-> valid-sexp-path? guide/c) (-> zipper? zipper?))
  (lambda (z)
    (define guide
      (gap-sides z
                 (lambda (left right)
                   (make-target-guide (sexp-address left right)))))
    ((navigate guide) z)))

(define parent-sexp
  (relative-sexp
   (compose before-sexp-guide parent-sexp-address)))

(module+ test
  (require rackunit)

  (define sexp-sys (system sexp-frontier-algebra))
  (define sexp-source
    "(define square\n  (lambda (x)\n    (* x x)))\n(+ 1 2)")
  (define sexp-rope
    ((string->rope sexp-sys) sexp-source #:chunk-size 3))

  (define (split-strings guide)
    (zipper-split
     ((navigate guide) ((start sexp-sys) sexp-rope))
     (lambda (left right)
       (values (rope->string left) (rope->string right)))))

  (define (at guide)
    ((navigate guide) ((start sexp-sys) sexp-rope)))

  (test-case "before-sexp-guide locates a nested atom start"
    (define-values (before at-name)
      (split-strings (before-sexp-guide '(0 2))))
    (check-equal? before "(define ")
    (check-true (string-prefix? at-name "square")))

  (test-case "after-sexp-guide locates a nested atom end"
    (define-values (through-name after-name)
      (split-strings (after-sexp-guide '(0 2))))
    (check-equal? through-name "(define square")
    (check-true (string-prefix? after-name "\n  (lambda")))

  (test-case "before-sexp-guide and after-sexp-guide locate deeper paths"
    (define-values (before-x at-x)
      (split-strings (before-sexp-guide '(0 3 2 1))))
    (check-true (string-suffix? before-x "(lambda ("))
    (check-true (string-prefix? at-x "x)"))
    (define-values (through-x after-x)
      (split-strings (after-sexp-guide '(0 3 2 1))))
    (check-true (string-suffix? through-x "(lambda (x"))
    (check-true (string-prefix? after-x ")")))

  (test-case "before-sexp-guide skips whitespace before a list"
    (define-values (before-second at-second)
      (split-strings (before-sexp-guide '(1))))
    (check-true (string-suffix? before-second "\n"))
    (check-true (string-prefix? at-second "(+ 1 2)")))

  (test-case "sexp-address reads the cursor position from total summaries"
    (define before-name (at (before-sexp-guide '(0 2))))
    (define after-name (at (after-sexp-guide '(0 2))))
    (define before-x (at (before-sexp-guide '(0 3 2 1))))
    (check-equal? (zipper-sexp-address before-name) '(0 2))
    (check-equal? (zipper-sexp-address after-name) '(0 3))
    (check-equal? (zipper-sexp-address before-x) '(0 3 2 1)))

  (test-case "sexp-address keeps an inside-atom cursor at the current address"
    (define inside-name (open-right (at (before-sexp-guide '(0 2)))))
    (check-equal? (zipper-sexp-address inside-name) '(0 2)))

  (test-case "zero-padded addresses still mean the same starting boundary"
    (define-values (before at-first)
      (split-strings (before-sexp-guide '(0 0 0))))
    (check-equal? before "")
    (check-true (string-prefix? at-first "(define")))

  (test-case "relative sexp navigation freezes the starting address"
    (define before-name (at (before-sexp-guide '(0 2))))
    (define skipped-name ((relative-sexp after-sexp-guide) before-name))
    (define next-after-name
      ((relative-sexp (compose before-sexp-guide next-sexp-address))
       before-name))
    (check-equal? (zipper-sexp-address skipped-name) '(0 3))
    (check-true (string-suffix?
                 (zipper-split skipped-name
                               (lambda (left _right)
                                 (rope->string left)))
                 "square"))
    (check-equal? (zipper-sexp-address next-after-name) '(0 3))
    (check-true (string-prefix?
                 (zipper-split next-after-name
                               (lambda (_left right)
                                 (rope->string right)))
                 "(lambda")))

  (test-case "parent-sexp navigates to the enclosing slot"
    (define before-x (at (before-sexp-guide '(0 3 2 1))))
    (define parent (parent-sexp before-x))
    (check-equal? (zipper-sexp-address parent) '(0 3 2))
    (check-true (string-suffix?
                 (zipper-split parent
                               (lambda (left _right)
                                 (rope->string left)))
                 "(lambda "))))
