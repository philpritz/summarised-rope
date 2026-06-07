#lang racket

;; Sexp summary: the opens/closes frontier algebra, as an `smr` for the current
;; rope-core (`make-summary`). Ported from deprecated-2/summary-algebras.rkt with one
;; agreed change: `opens` is stored INNERMOST-FIRST (not reversed), so it matches
;; `closes` (already innermost-first) and both read off the head:
;;
;;   (car opens)  of the before-summary = k_left  (children left of the cursor's frame)
;;   (car closes) of the after-summary  = k_right (children right of the cursor's frame)
;;
;; A summary value is either #f (the empty/identity, = (sexp-leaf "")) or a 7-list:
;;
;;   (starts-atom? starts-form? closes forms opens ends-atom? ends-form?)
;;
;;   closes : dangling closes -- ")"s with no matching "(" in this chunk. Each entry =
;;            the form-count that preceded it at that level. INNERMOST-FIRST.
;;   forms  : complete forms at the current (innermost-open, or top) level.
;;   opens  : the open frontier -- one entry per still-open "(", each = the forms
;;            nested in it so far. INNERMOST-FIRST (car = deepest).
;;
;; `closes` faces left (it cancels opens from before), `opens` faces right (they are
;; cancelled by closes after). The monoid `sexp+` combines two chunks by matching the
;; left's `opens` against the right's `closes` at the seam.

(require "rope-core.rkt")          ; make-summary make-rope

(provide sexp                      ; the smr  -- (sexp str), ((make-rope sexp) ...)
         sexp-leaf sexp+           ; the algebra (leaf measure, combine)
         sexp-opens sexp-closes sexp-forms)   ; accessors over a summary value

;; ---------- char classes ----------
(define (sexp-atom? c)
  (not (or (char-whitespace? c) (char=? c #\() (char=? c #\)))))
(define (sexp-form-start? c) (or (sexp-atom? c) (char=? c #\()))
(define (sexp-form-end?   c) (or (sexp-atom? c) (char=? c #\))))

;; a new form at the current level: top-level bumps `forms`, else the innermost
;; open's count (the car, since opens is innermost-first).
(define (bump-sexp forms opens)
  (if (null? opens)
      (values (add1 forms) opens)
      (values forms (cons (add1 (car opens)) (cdr opens)))))

;; ---------- leaf measure ----------
;; #f for the empty string (the identity); otherwise the 7-list. opens is kept
;; innermost-first (NOT reversed); closes is reversed to innermost-first.
(define (sexp-leaf s)
  (and (positive? (string-length s))
       (let-values
           ([(sa? sf? closes forms opens in-atom? za? zf?)
             (for/fold ([sa? (sexp-atom? (string-ref s 0))]
                        [sf? (sexp-form-start? (string-ref s 0))]
                        [closes '()]
                        [forms 0]
                        [opens '()]
                        [in-atom? #f]
                        [za? #f]
                        [zf? #f])
                       ([c (in-string s)])
               (cond
                 [(sexp-atom? c)
                  (if in-atom?
                      (values sa? sf? closes forms opens #t #t #t)   ; same atom continues
                      (let-values ([(forms opens) (bump-sexp forms opens)])
                        (values sa? sf? closes forms opens #t #t #t)))]  ; new atom = new form
                 [(char=? c #\()
                  (let-values ([(forms opens)
                                (if (null? opens) (values forms opens) (bump-sexp forms opens))])
                    (values sa? sf? closes forms (cons 0 opens) #f #f #f))]   ; push a new open
                 [(char=? c #\))
                  (if (null? opens)
                      (values sa? sf? (cons forms closes) 0 opens #f #f #t)   ; dangling close
                      (let ([opens (cdr opens)])                              ; pop one open
                        (values sa? sf? closes
                                (if (null? opens) (add1 forms) forms)
                                opens #f #f #t)))]
                 [else
                  (values sa? sf? closes forms opens #f #f #f)]))])           ; whitespace
         (list sa? sf? (reverse closes) forms opens za? zf?))))

;; ---------- combine ----------
;; If the left ends mid-atom and the right starts mid-atom, the right's leading atom
;; is a continuation, not a new form: drop it from the right's first form-count.
(define (drop-start-sexp-atom x)
  (match-define (list sa? sf? closes forms opens za? zf?) x)
  (if (pair? closes)
      (list sa? sf? (cons (sub1 (car closes)) (cdr closes)) forms opens za? zf?)
      (list sa? sf? closes (sub1 forms) opens za? zf?)))

;; add n forms to the innermost open (the car, since opens is innermost-first).
(define (add-inner-sexp opens n)
  (cons (+ (car opens) n) (cdr opens)))

;; reconcile the left's (forms, opens innermost-first) against the right's
;; (closes innermost-first, forms, opens). opens/closes/stack all innermost-first,
;; so the matching walks both from the head -- no reversing.
(define (merge-sexp-frontier forms opens closes right-forms right-opens)
  (let loop ([forms forms]
             [stack opens]          ; innermost-first
             [closes closes]        ; innermost-first
             [out '()])
    (match closes
      ['()
       (if (null? stack)
           ;; right cancelled every left open: combined = right's frontier
           (values (reverse out) (+ forms right-forms) right-opens)
           ;; leftover left opens (outer frames). The right's content (its forms, plus
           ;; 1 if it left anything open) became children of the innermost leftover
           ;; frame; the right's opens nest inside as the new innermost.
           (let* ([extra (+ right-forms (if (pair? right-opens) 1 0))]
                  [stack (if (zero? extra) stack (add-inner-sexp stack extra))])
             (values (reverse out) forms (append right-opens stack))))]
      [(cons close-count rest)
       (if (pair? stack)
           (let ([stack (cdr stack)])                                  ; close matches an open: pop
             (loop (if (null? stack) (add1 forms) forms) stack rest out))
           (loop 0 stack rest (cons (+ forms close-count) out)))])))   ; still dangling: emit

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

;; ---------- the smr + accessors ----------
(define sexp (make-summary sexp-leaf sexp+))

(define (sexp-opens  s) (if s (list-ref s 4) '()))
(define (sexp-closes s) (if s (list-ref s 2) '()))
(define (sexp-forms  s) (if s (list-ref s 3) 0))

;; ============================================================================
(module+ test
  (require rackunit)
  (define (opens  x) (sexp-opens  (sexp x)))
  (define (closes x) (sexp-closes (sexp x)))
  (define (forms  x) (sexp-forms  (sexp x)))

  ;; --- the touching parts of a cut  (_ _ ^)  ---
  (check-equal? (opens  "(_ _ ") '(2))   ; k_left = 2 (two children to the left, frame open)
  (check-equal? (forms  "(_ _ ") 0)
  (check-equal? (closes "(_ _ ") '())
  (check-equal? (closes ")")     '(0))   ; k_right = 0 (nothing right of the cursor before ")")
  (check-equal? (opens  ")")     '())
  (check-equal? (forms  ")")     0)

  ;; head reads give the two counts directly
  (check-equal? (car (opens  "(_ _ ")) 2)   ; k_left
  (check-equal? (car (closes ")"))     0)    ; k_right

  ;; --- a complete top-level form ---
  (check-equal? (forms  "(_ _)") 1)
  (check-equal? (opens  "(_ _)") '())
  (check-equal? (closes "(_ _)") '())

  ;; --- multi-level opens, innermost-first ---
  (check-equal? (opens "((a ")    '(1 1))  ; inner frame: 1 child (a); outer: 1 child (the list)
  (check-equal? (opens "((a b) ") '(1))    ; inner closed -> outer has 1 child, inner's kids gone

  ;; --- associativity: a chunked rope's summary == the single-leaf summary ---
  ;; (this is the real test of the innermost-first merge adaptation)
  (define (chunked str k) (sexp ((make-rope sexp #:chunk-size k) str)))
  (for* ([str (list "(_ _)" "((a b) c)" "(define (f x) (+ x 1))"
                    "(_ _ " "((a " ")" "a b c" "(((x)))" ") foo (bar")]
         [k (in-range 1 6)])
    (check-equal? (chunked str k) (sexp str)
                  (format "chunk size ~a of ~s" k str))))
