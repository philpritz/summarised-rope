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
 sexp-frontier-algebra)

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

(define (bump-sexp forms opens)
  (if (null? opens)
      (values (add1 forms) opens)
      (values forms (cons (add1 (car opens)) (cdr opens)))))

(define (sexp-leaf s)
  (and (positive? (string-length s))
       (let-values
           ([(starts? closes forms opens in-atom? ends?)
             (for/fold ([starts? (sexp-atom? (string-ref s 0))]
                        [closes '()]
                        [forms 0]
                        [opens '()]
                        [in-atom? #f]
                        [ends? #f])
                       ([c s])
               (cond
                 [(sexp-atom? c)
                  (if in-atom?
                      (values starts? closes forms opens #t #t)
                      (let-values ([(forms opens) (bump-sexp forms opens)])
                        (values starts? closes forms opens #t #t)))]
                 [(char=? c #\()
                  (let-values ([(forms opens)
                                (if (null? opens)
                                    (values forms opens)
                                    (bump-sexp forms opens))])
                    (values starts? closes forms (cons 0 opens) #f #f))]
                 [(char=? c #\))
                  (if (null? opens)
                      (values starts? (cons forms closes) 0 opens #f #f)
                      (let ([opens (cdr opens)])
                        (values starts? closes
                                (if (null? opens) (add1 forms) forms)
                                opens #f #f)))]
                 [else
                  (values starts? closes forms opens #f #f)]))])
         (list starts? (reverse closes) forms (reverse opens) ends?))))

(define (drop-start-sexp-atom x)
  (match-let ([(list starts? closes forms opens ends?) x])
    (if (pair? closes)
        (list starts? (cons (sub1 (car closes)) (cdr closes)) forms opens ends?)
        (list starts? closes (sub1 forms) opens ends?))))

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
           (match-let ([(list xa? xc xf xo xz?) x]
                       [(list ya? yc yf yo yz?) y])
             (define y* (if (and xz? ya?) (drop-start-sexp-atom y) y))
             (match-define (list ya2? yc2 yf2 yo2 yz2?) y*)
             (define-values (closes forms opens)
               (merge-sexp-frontier xf xo yc2 yf2 yo2))
             (list xa? (append xc closes) forms opens yz2?)))
      x
      y))

(define sexp-frontier-algebra
  (text-summary-algebra sexp-leaf sexp+))
