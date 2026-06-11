#lang racket

;; Sexp summary: the opens/closes frontier algebra as a SIGNED struct, for the current
;; rope-core (`make-summary`).  Two storage decisions distinguish it from the 7-list
;; version it replaces (deprecated-5 era):
;;
;; SIGNED STACKS.  `opens` entries are +(k+1), `closes` entries are -(k+1), so a value
;; reads (frontier ... (negatives) forms (positives) ...) and the slot indexes read
;; DIRECTLY off the stack heads: front = (car opens) of the before summary, back =
;; (car closes) of the after summary.  Neither stack ever holds 0 (zero stays the
;; frame's own boundary).  This reverses 2026-06-07/1 Part D's counts-only storage:
;; the offset IS storable once both stacks carry it symmetrically and the combine
;; compensates -- the associativity battery below is the proof.
;;
;; COMPLETION COUNTING.  A frame counts on the enclosing level at its ")" (the pop
;; bumps what it exposes), not at its "(".  Atoms still count at their first char.
;; So an open frame's interior leads with the same value as the frame's own start
;; slot -- a prefix extension of it -- instead of colliding with the NEXT sibling's
;; start, and spine comparison is naively lexicographic (see sexp-edit.rkt).  The
;; old at-"(" bump was inherited from the original fold, never a reasoned choice.
;; Mirrored on the right: a dangling ")" seeds the level above with the frame it
;; closed (forms := 1), so `closes` entries count frames whose close is ahead.
;; Every completion is counted exactly once, by whichever side saw the ")":
;; a leaf pop bumps; the merge's cancellation does NOT (the right chunk counted it).
;;
;;   closes : dangling ")"s, innermost-first; entry = -(k+1), k = forms preceding it
;;            at its level (atoms by start, frames by close).
;;   forms  : complete forms at the current (innermost-open, or top) level.
;;   opens  : open "("s, innermost-first; entry = +(k+1), k = children so far.

(require racket/match
         "rope-core.rkt")          ; make-summary make-rope

(provide sexp-smr                  ; the smr  -- (sexp-smr str), ((make-rope sexp-smr) ...)
         sexp-leaf sexp+           ; the algebra (leaf measure, combine)
         (struct-out frontier)     ; the summary value
         ;; #f-safe readers (#f is the empty/identity summary)
         sexp-opens sexp-closes sexp-forms
         sexp-starts-atom? sexp-starts-form? sexp-ends-atom? sexp-ends-form?)

;; ---------- the summary value ----------
;; #f stays the empty/identity (= (sexp-smr "")).
(struct frontier
  (starts-atom?     ; first char continues/starts an atom
   starts-form?     ; first char starts a form (atom char or "(")
   closes           ; dangling ")"s, innermost-first; entries -(k+1)
   forms            ; complete forms at the current level (a plain count)
   opens            ; open "("s, innermost-first; entries +(k+1)
   ends-atom?       ; last char is mid/end of an atom
   ends-form?)      ; last char ends a form (atom char or ")")
  #:transparent)

;; ---------- char classes ----------
(define (sexp-atom? c)
  (not (or (char-whitespace? c) (char=? c #\() (char=? c #\)))))
(define (sexp-form-start? c) (or (sexp-atom? c) (char=? c #\()))

;; a new ATOM at the current level counts at its first char: top-level bumps `forms`,
;; else the innermost open's entry (the offset rides along: +(k+1)+1 = +((k+1)+1)).
(define (bump-sexp forms opens)
  (if (null? opens)
      (values (add1 forms) opens)
      (values forms (cons (add1 (car opens)) (cdr opens)))))

;; ---------- leaf measure ----------
;; signed sites: "(" pushes 1 (the open frame is +(0+1));  a dangling ")" emits
;; -(forms+1).  counting sites: "(" does NOT bump the enclosing level; ")" does --
;; a real pop bumps what it exposes, a dangling close seeds the level above with
;; the frame it just closed (forms := 1).
(define (sexp-leaf s)
  (and (positive? (string-length s))
       (let-values
           ([(sa? sf? closes forms opens in-atom? za? zf?)
             (for/fold ([sa? (sexp-atom? (string-ref s 0))]
                        [sf? (sexp-form-start? (string-ref s 0))]
                        [closes '()] [forms 0] [opens '()]
                        [in-atom? #f] [za? #f] [zf? #f])
                       ([c (in-string s)])
               (cond
                 [(sexp-atom? c)
                  (if in-atom?
                      (values sa? sf? closes forms opens #t #t #t)   ; same atom continues
                      (let-values ([(forms opens) (bump-sexp forms opens)])
                        (values sa? sf? closes forms opens #t #t #t)))]
                 [(char=? c #\()
                  (values sa? sf? closes forms (cons 1 opens) #f #f #f)]
                 [(char=? c #\))
                  (if (null? opens)
                      (values sa? sf? (cons (- (add1 forms)) closes) 1 opens #f #f #t)
                      (let ([opens (cdr opens)])
                        (if (null? opens)
                            (values sa? sf? closes (add1 forms) opens #f #f #t)
                            (values sa? sf? closes forms
                                    (cons (add1 (car opens)) (cdr opens)) #f #f #t))))]
                 [else (values sa? sf? closes forms opens #f #f #f)]))])
         (frontier sa? sf? (reverse closes) forms opens za? zf?))))

;; ---------- combine ----------
;; If the left ends mid-atom and the right starts mid-atom, the right's leading atom
;; is a continuation, not a new form: undo its count.  On -(k+1) the decrement is an
;; add1 -- -(k+1)+1 = -((k-1)+1).
(define (drop-start-atom y)
  (match y
    [(frontier _ _ (cons c rest) _ _ _ _)
     (struct-copy frontier y [closes (cons (add1 c) rest)])]
    [_ (struct-copy frontier y [forms (sub1 (frontier-forms y))])]))

;; add n forms to the innermost open; the offset rides along.
(define (add-inner-sexp opens n) (cons (+ (car opens) n) (cdr opens)))

;; reconcile the left's (forms, opens) against the right's (closes, forms, opens);
;; all stacks innermost-first, so the matching walks both from the head.
(define (merge-sexp-frontier forms opens closes right-forms right-opens)
  (let loop ([forms forms] [stack opens] [closes closes] [out '()])
    (match closes
      ['()
       (if (null? stack)
           ;; right cancelled every left open: combined = right's frontier
           (values (reverse out) (+ forms right-forms) right-opens)
           ;; leftover left opens: the right's completed content becomes children of
           ;; the innermost leftover frame (its own open frames count only on close)
           (let ([stack (if (zero? right-forms) stack (add-inner-sexp stack right-forms))])
             (values (reverse out) forms (append right-opens stack))))]
      [(cons c rest)
       (if (pair? stack)
           ;; close matches an open: pop, no bump -- the right chunk saw the ")" as
           ;; dangling and already counted the completion (its forms := 1 seed)
           (loop forms (cdr stack) rest out)
           ;; still dangling: the left's forms precede it; on -(k+1) the addition is
           ;; a subtraction -- -(k+1) - f = -((k+f)+1)
           (loop 0 stack rest (cons (- c forms) out)))])))

(define (sexp+ x y)
  (or (and x y
           (let ([y (if (and (frontier-ends-atom? x) (frontier-starts-atom? y))
                        (drop-start-atom y)
                        y)])
             (match-let ([(frontier xa? xs? xc xf xo _ _) x]
                         [(frontier _ _ yc yf yo yz? ye?) y])
               (define-values (closes forms opens) (merge-sexp-frontier xf xo yc yf yo))
               (frontier xa? xs? (append xc closes) forms opens yz? ye?))))
      x y))

;; ---------- the smr + #f-safe readers ----------
(define sexp-smr (make-summary sexp-leaf sexp+))

(define (sexp-opens  s) (if s (frontier-opens  s) '()))
(define (sexp-closes s) (if s (frontier-closes s) '()))
(define (sexp-forms  s) (if s (frontier-forms  s) 0))
(define (sexp-starts-atom? s) (and s (frontier-starts-atom? s)))
(define (sexp-starts-form? s) (and s (frontier-starts-form? s)))
(define (sexp-ends-atom?   s) (and s (frontier-ends-atom?   s)))
(define (sexp-ends-form?   s) (and s (frontier-ends-form?   s)))

;; ============================================================================
(module+ test
  (require rackunit)
  (define (opens  x) (sexp-opens  (sexp-smr x)))
  (define (closes x) (sexp-closes (sexp-smr x)))
  (define (forms  x) (sexp-forms  (sexp-smr x)))

  ;; --- signed, completion-counting worked values ---
  (check-equal? (opens "(")          '(1))      ; the open frame itself is +(0+1)
  (check-equal? (opens "(aa ")       '(2))
  (check-equal? (opens "(aa (p ")    '(2 2))    ; enclosing NOT bumped at the "("...
  (check-equal? (opens "(aa (p q)")  '(3))      ; ...bumped at the ")"
  (check-equal? (opens "(aa bb cc")  '(4))
  (check-equal? (closes ")")         '(-1))     ; "-1 = after last", directly in storage
  (check-equal? (forms  ")")         1)         ; the closed frame seeds the level above
  (check-equal? (closes "aa bb cc)") '(-4))
  (check-equal? (closes "q) cc)")    '(-2 -3))  ; outer entry counts the closed frame
  (check-equal? (forms  "()")        1)
  (check-equal? (opens  "()")        '())

  ;; --- the cut reads: front = (car opens) of before, back = (car closes) of after ---
  (check-equal? (car (opens  "(aa "))   2)
  (check-equal? (car (closes "bb cc)")) -3)

  ;; --- flags ---
  (check-true  (sexp-ends-atom?   (sexp-smr "(aa bb cc")))
  (check-true  (sexp-starts-form? (sexp-smr "(p q) cc)")))
  (check-false (sexp-starts-form? (sexp-smr ") cc)")))

  ;; --- associativity: k-char pieces combine to the whole-string summary ---
  ;; (the real test of the signed completion-counting merge -- the variadic
  ;; `sexp` folds `combine` over the measured pieces, vs one straight measure)
  (define (chunked str k)
    (apply sexp-smr (for/list ([i (in-range 0 (string-length str) k)])
                  (substring str i (min (string-length str) (+ i k))))))
  (for* ([str (list "(_ _)" "((a b) c)" "(define (f x) (+ x 1))"
                    "(_ _ " "((a " ")" "a b c" "(((x)))" ") foo (bar"
                    "()" "(())" "(aa (p q) cc)" "((a b) (c d))" "x (y) z"
                    "q) cc)" "((a b) c")]
         [k (in-range 1 6)])
    (check-equal? (chunked str k) (sexp-smr str)
                  (format "chunk size ~a of ~s" k str))))
