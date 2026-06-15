#lang racket

;; Summaries: the general summary combinators plus the concrete summary algebras.
;; The general piece is `bundle` (a product of summaries -- see below); the rest of
;; the file is the sexp instance.  The summary *protocol* (make-summary and the
;; gen:summary-part extension point) lives in rope-core; this file builds on it.
;;
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
         "rope-core.rkt")          ; make-summary; gen:summary-part (for bundle-val)

(provide bundle                    ; (bundle s1 s2 ...) -> the product smr
         (struct-out bundle-val)   ; the product summary value
         sexp-smr                  ; the smr  -- (sexp-smr str), ((make-rope sexp-smr) ...)
         sexp-leaf sexp+           ; the algebra (leaf measure, combine)
         (struct-out frontier)     ; the summary value
         ;; #f-safe readers (#f is the empty/identity summary)
         sexp-opens sexp-closes sexp-forms
         sexp-head sexp-tail
         sexp-starts-atom? sexp-starts-form? sexp-ends-atom? sexp-ends-form?)

;; ---------- bundle: a product of summaries ----------
;; (bundle s1 s2 ...) -> the product smr; build & stamp ropes under it.  Its value
;; is a `bundle-val` carrying every component, keyed by the component's own smr.
;; Applying a component smr to a bundle-val selects that component (gen:summary-part);
;; any other smr -- e.g. the product smr itself -- passes it through unchanged.  A
;; guide/reader for component c reads through it with (on g c) (helper-algebras' `on`).
(struct bundle-val (slots)              ; slots : #hasheq(component-smr -> value)
  #:transparent
  #:methods gen:summary-part
  [(define (part->summary bv smr)
     (if (hash-has-key? (bundle-val-slots bv) smr)
         (hash-ref (bundle-val-slots bv) smr)
         bv))])

(define (bundle . components)
  (make-summary
   (lambda (str) (bundle-val (for/hasheq ([c (in-list components)]) (values c (c str)))))
   (lambda (a b)  (bundle-val (for/hasheq ([c (in-list components)]) (values c (c a b)))))))

;; ---------- the summary value ----------
;; #f stays the empty/identity (= (sexp-smr "")).  head/tail flank the three
;; measure fields, so a value reads left-to-right like the fragment itself.
(struct frontier
  (head             ; class of the first char: 'open | 'close | 'ws | 'atom
   closes           ; dangling ")"s, innermost-first; entries -(k+1)
   forms            ; complete forms at the current level (a plain count)
   opens            ; open "("s, innermost-first; entries +(k+1)
   tail)            ; class of the last char:  'open | 'close | 'ws | 'atom
  #:transparent)

;; ---------- char classes ----------
;; every char falls in exactly one class; head/tail store the class of the
;; first/last char, so a ")" edge is distinct from a whitespace edge (the four
;; old booleans collapsed ")"/ws at a head and "("/ws at a tail).
(define (char-class c)
  (cond [(char=? c #\() 'open]
        [(char=? c #\)) 'close]
        [(char-whitespace? c) 'ws]
        [else 'atom]))

;; a new form at the current level -- an atom at its first char, or a frame at
;; its ")" -- counts the same way: top level bumps `forms`, else the innermost
;; open's entry (the offset rides along: +(k+1)+1 = +((k+1)+1)).
(define (bump-sexp forms opens)
  (if (null? opens)
      (values (add1 forms) opens)
      (values forms (cons (add1 (car opens)) (cdr opens)))))

;; ---------- leaf measure ----------
;; Tokenize the fragment into parens and maximal atom runs (whitespace falls
;; away), then fold the signed completion-counting algebra over the tokens:
;;   open   pushes a fresh frame, +(0+1);
;;   atom   registers one form at the current level (bump-sexp);
;;   close  pops its frame and registers it one level up (bump-sexp on the
;;          popped stack) -- closing a frame and starting an atom are one act.
;;          A DANGLING ")" instead seeds a fresh base level (forms := 1) and
;;          emits -(forms+1), a frame whose close is still ahead.
;; head/tail are just the first/last char's class -- off the ends, no scanning.
;; #f stays the empty/identity (the positive? guard).
(define (sexp-tokens s)                              ; parens and atom runs, in order
  (map (lambda (t) (char-class (string-ref t 0)))
       (regexp-match* #px"[()]|[^()\\s]+" s)))       ; -> list of 'open | 'close | 'atom

(define (sexp-leaf s)
  (and (positive? (string-length s))
       (let-values
           ([(closes forms opens)
             (for/fold ([closes '()] [forms 0] [opens '()])
                       ([tok (in-list (sexp-tokens s))])
               (case tok
                 [(atom)  (let-values ([(f o) (bump-sexp forms opens)])
                            (values closes f o))]
                 [(open)  (values closes forms (cons 1 opens))]
                 [(close) (if (null? opens)
                              (values (cons (- (add1 forms)) closes) 1 opens)
                              (let-values ([(f o) (bump-sexp forms (cdr opens))])
                                (values closes f o)))]))])
         (frontier (char-class (string-ref s 0))
                   (reverse closes) forms opens
                   (char-class (string-ref s (sub1 (string-length s))))))))

;; ---------- combine ----------
;; If the left ends mid-atom and the right starts mid-atom, the right's leading atom
;; is a continuation, not a new form: undo its count.  On -(k+1) the decrement is an
;; add1 -- -(k+1)+1 = -((k-1)+1).
(define (drop-start-atom y)
  (match y
    [(frontier _ (cons c rest) _ _ _)
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
           (let ([y (if (and (eq? (frontier-tail x) 'atom)
                             (eq? (frontier-head y) 'atom))
                        (drop-start-atom y)
                        y)])
             (match-let ([(frontier xh xc xf xo _) x]
                         [(frontier _  yc yf yo yt) y])
               (define-values (closes forms opens) (merge-sexp-frontier xf xo yc yf yo))
               (frontier xh (append xc closes) forms opens yt))))
      x y))

;; ---------- the smr + #f-safe readers ----------
(define sexp-smr (make-summary sexp-leaf sexp+))

(define (sexp-opens  s) (if s (frontier-opens  s) '()))
(define (sexp-closes s) (if s (frontier-closes s) '()))
(define (sexp-forms  s) (if s (frontier-forms  s) 0))
(define (sexp-head s) (and s (frontier-head s)))   ; class of first char, #f if empty
(define (sexp-tail s) (and s (frontier-tail s)))   ; class of last char,  #f if empty
;; the edge predicates, rederived from the head/tail classes (#f-safe: an empty
;; summary has #f head/tail, so every predicate is #f).
(define (sexp-starts-atom? s) (eq? (sexp-head s) 'atom))
(define (sexp-starts-form? s) (case (sexp-head s) [(atom open)  #t] [else #f]))
(define (sexp-ends-atom?   s) (eq? (sexp-tail s) 'atom))
(define (sexp-ends-form?   s) (case (sexp-tail s) [(atom close) #t] [else #f]))

;; ============================================================================
(module+ test
  (require rackunit)
  (define (opens  x) (sexp-opens  (sexp-smr x)))
  (define (closes x) (sexp-closes (sexp-smr x)))
  (define (forms  x) (sexp-forms  (sexp-smr x)))

  ;; --- bundle: a product summary; each component smr selects its own part ---
  (let* ([cc (make-summary string-length +)]      ; a second summary: char count
         [b  (bundle sexp-smr cc)])
    (check-equal? (sexp-smr (b "(aa bb")) (sexp-smr "(aa bb"))   ; sexp part selected
    (check-equal? (cc (b "(aa bb")) 6)                            ; char part selected
    (check-equal? (b "(aa" " bb") (b "(aa bb")))                  ; product folds associatively

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
                  (format "chunk size ~a of ~s" k str)))

  ;; --- the summary-law battery (summary-laws.rkt) on realistic generated sexps ---
  (require "summary-laws.rkt" rackcheck)

  ;; atoms: a lexicon of real words, synthesized lispy identifiers (hyphenated,
  ;; ?/!/* suffixed -- multi-char, so mid-atom cuts have targets), and numbers.
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

  ;; whitespace between siblings, drawn per junction (so shrinking simplifies
  ;; it): spaces, runs, newlines, and "" -- zero-width, legal where parens abut.
  (define gen:ws (gen:one-of '(" " " " " " "  " "\n" "\n  " "")))

  ;; trees: leaf = atom string; node = (list kids seps), keyword-headed forms
  ;; with 0..3 further children, depth-bounded so termination is structural.
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

  ;; deterministic render; a zero-width separator between two ATOMS would fuse
  ;; them (changing the ground truth), so it falls back to a space there.
  (define (render t)
    (if (string? t)
        t
        (let loop ([ks (first t)] [seps (second t)] [acc ""])
          (cond
            [(null? ks)       (string-append "(" acc ")")]
            [(null? (cdr ks)) (string-append "(" acc (render (car ks)) ")")]
            [else
             (define a    (car ks))
             (define sep  (car seps))
             (define sep* (if (and (string? a) (string? (cadr ks)) (equal? sep ""))
                              " " sep))
             (loop (cdr ks) (cdr seps) (string-append acc (render a) sep*))]))))

  (define gen:sexp-doc (gen:map (gen:node 4 3) render))

  ;; curated corpus: real code, the chunk-test strings above, edge cases. Swept
  ;; deterministically (every entry, every single cut), then mixed into the
  ;; random stream. Shrunk counterexamples from failed runs get appended here.
  (define sexp-corpus
    (list "(define (fact n) (if (zero? n) 1 (* n (fact (sub1 n)))))"
          "(define (map f xs) (if (null? xs) '() (cons (f (car xs)) (map f (cdr xs)))))"
          "(let loop ([i 0] [acc '()]) (if (= i 10) (reverse acc) (loop (add1 i) (cons i acc))))"
          "(lambda (x . rest) (apply + x rest))"
          "'(1 2 . 3)"
          "(display \"hello world\")"
          ";; a comment line\n(+ 1 2)"
          "(define (f λ) (λ))"
          "(_ _)" "((a b) c)" "(define (f x) (+ x 1))"
          "(_ _ " "((a " ")" "a b c" "(((x)))" ") foo (bar"
          "()" "(())" "(aa (p q) cc)" "((a b) (c d))" "x (y) z"
          "q) cc)" "((a b) c"
          "" " " "((((" "))))" "atom"))

  (check-summary-laws sexp-smr gen:sexp-doc #:corpus sexp-corpus))
