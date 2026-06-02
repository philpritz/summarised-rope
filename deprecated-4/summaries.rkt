#lang racket

;; Concrete summary algebras built with `summariser`, plus the projections and
;; guides the zipper reads out of them.
;;
;; char-count: summary = number of characters.
;; sexp:       the opens-FRONTIER monoid (ported from the old
;;             deprecated-2/summary-algebras.rkt sexp-frontier-algebra). Its
;;             summary value IS the cursor's structural position, so guides can
;;             address a specific sexp node by tree path, e.g. '(0 3 2 1).

(require racket/match
         "rope-core.rkt"
         "zipper-core.rkt")

(provide
 ;; char-count -- projection is identity
 char-count
 ;; sexp summary + projections
 sexp
 (struct-out sx)
 sexp-depth
 sexp-balanced?
 ;; addresses (tree paths) and the moves over them
 sexp-address
 next-sexp-address previous-sexp-address parent-sexp-address
 ;; boundary guides: the gap before / after the form at a path
 before-sexp-guide after-sexp-guide
 ;; cursor guides + the sexp selection helper
 char-guide sexp-guide carve-form sexp-carve sexp-form-span)

;; ---------- char-count ----------
(define char-count (summariser string-length +))

;; ---------- sexp (opens frontier) ----------
;; A chunk's summary records the cursor's structural position as a frontier:
;;   chars        : character count (offset axis / rendering)
;;   starts-atom? / starts-form? : first char is an atom char / a form start ("(" or atom)
;;   ends-atom?   / ends-form?   : last char ...
;;   closes       : form-counts for ")"s that pop opens opened *before* this chunk
;;   forms        : forms completed at the current (innermost-open, or top) level
;;   opens        : the open frontier -- a stack; each entry = forms nested in it so far
;;   atoms        : number of atom (symbol) runs -- a convenience for symbol guides
;; combine reconciles the left's open stack against the right's closes
;; (merge-sexp-frontier) and merges a symbol split across the seam.
(struct sx (chars starts-atom? starts-form? ends-atom? ends-form? closes forms opens atoms)
  #:transparent)

(define (sexp-atom? c) (not (or (char-whitespace? c) (char=? c #\() (char=? c #\)))))
(define (sexp-form-start? c) (or (sexp-atom? c) (char=? c #\()))

;; a new form at the current level: top-level bumps `forms`, else the innermost open's count
(define (bump-sexp forms opens)
  (if (null? opens)
      (values (add1 forms) opens)
      (values forms (cons (add1 (car opens)) (cdr opens)))))

(define (sexp-leaf s)
  (define n (string-length s))
  (if (zero? n)
      (sx 0 #f #f #f #f '() 0 '() 0)                ; the monoid identity (empty chunk)
      (let-values
          ([(closes forms opens in? ef? atoms)
            (for/fold ([closes '()] [forms 0] [opens '()] [in? #f] [ef? #f] [atoms 0])
                      ([c (in-string s)])
              (cond
                [(sexp-atom? c)
                 (if in?
                     (values closes forms opens #t #t atoms)            ; same atom continues
                     (let-values ([(forms opens) (bump-sexp forms opens)])
                       (values closes forms opens #t #t (add1 atoms))))] ; new atom = new form
                [(char=? c #\()
                 (let-values ([(forms opens)
                               (if (null? opens) (values forms opens) (bump-sexp forms opens))])
                   (values closes forms (cons 0 opens) #f #f atoms))]    ; push a new open
                [(char=? c #\))
                 (if (null? opens)
                     (values (cons forms closes) 0 opens #f #t atoms)    ; unmatched close
                     (let ([opens (cdr opens)])                          ; pop one open
                       (values closes (if (null? opens) (add1 forms) forms) opens #f #t atoms)))]
                [else (values closes forms opens #f #f atoms)]))])       ; whitespace
        (sx n
            (sexp-atom? (string-ref s 0))
            (sexp-form-start? (string-ref s 0))
            in? ef?
            (reverse closes) forms (reverse opens) atoms))))

;; drop the double-counted form-start at y's beginning when the seam is mid-atom
(define (drop-start-sexp-atom x)
  (match-define (sx ch sa? sf? za? zf? closes forms opens at) x)
  (if (pair? closes)
      (sx ch sa? sf? za? zf? (cons (sub1 (car closes)) (cdr closes)) forms opens at)
      (sx ch sa? sf? za? zf? closes (sub1 forms) opens at)))

(define (add-inner-sexp opens n)
  (match opens
    [(list x)    (list (+ x n))]
    [(cons x xs) (cons x (add-inner-sexp xs n))]))

;; reconcile the left's (forms, opens-stack) against the right's (closes, forms, opens)
(define (merge-sexp-frontier forms opens closes right-forms right-opens)
  (let loop ([forms forms] [stack (reverse opens)] [closes closes] [out '()])
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

(define (sexp-combine x y)
  (cond
    [(zero? (sx-chars x)) y]                 ; identity short-circuits (keeps y intact)
    [(zero? (sx-chars y)) x]
    [else
     (match-define (sx xch xa? xs? xz? xe? xc xf xo xat) x)
     (define seam-atom? (and xz? (sx-starts-atom? y)))
     (define y* (if seam-atom? (drop-start-sexp-atom y) y))
     (match-define (sx ych _ya? _ys? yz? ye? yc yf yo yat) y*)
     (define-values (closes forms opens) (merge-sexp-frontier xf xo yc yf yo))
     (sx (+ xch ych) xa? xs? yz? ye?
         (append xc closes) forms opens
         (- (+ xat yat) (if seam-atom? 1 0)))]))

(define sexp (summariser sexp-leaf sexp-combine))

;; current nesting depth (unclosed opens) and balance, off a prefix summary
(define (sexp-depth s) (length (sx-opens s)))
(define (sexp-balanced? s) (and (null? (sx-opens s)) (null? (sx-closes s))))

;; ---------- addresses (tree paths) ----------
;; The address of the cursor sitting just after a prefix: the form-count at the
;; top level, then the per-open form counts down the frontier, with the innermost
;; bumped (so it names the *next* form to start). #f / empty -> '(0).
(define (sexp-next-address s)
  (define (opens->address opens)
    (match opens
      ['() '()]
      [(list innermost) (list (add1 innermost))]
      [(cons child-count rest) (cons child-count (opens->address rest))]))
  (cons (sx-forms s) (opens->address (sx-opens s))))

(define (drop-trailing-zero-addresses path)
  (define trimmed
    (let loop ([r (reverse path)])
      (match r [(cons 0 rest) (loop rest)] [_ (reverse r)])))
  (if (null? trimmed) '(0) trimmed))

(define (next-sexp-address path)
  (match (drop-trailing-zero-addresses path)
    [(list index)      (list (add1 index))]
    [(cons index rest) (cons index (next-sexp-address rest))]))

(define (previous-sexp-address path)
  (match (drop-trailing-zero-addresses path)
    [(list index)
     (if (zero? index)
         (error 'sexp-address "cannot move before the first form: ~v" path)
         (list (sub1 index)))]
    [(cons index rest) (cons index (previous-sexp-address rest))]))

(define (parent-sexp-address path)
  (define normalized (drop-trailing-zero-addresses path))
  (define (drop-last p) (match p [(list _) '()] [(cons f r) (cons f (drop-last r))]))
  (match normalized
    [(list _) (error 'parent-sexp-address "top-level form has no parent: ~v" path)]
    [_ (drop-last normalized)]))

(define (sexp-path-compare x y)
  (define (cmp x y)
    (match* (x y)
      [('() '()) 0] [('() _) -1] [(_ '()) 1]
      [((cons x0 xs) (cons y0 ys))
       (cond [(< x0 y0) -1] [(> x0 y0) 1] [else (cmp xs ys)])]))
  (cmp (drop-trailing-zero-addresses x) (drop-trailing-zero-addresses y)))

;; the address of the form straddled by a (before, after) cut -- an inside-atom
;; cut keeps the address of the atom it sits in.
(define (sexp-address left right)
  (define next-path (sexp-next-address left))
  (if (and (sx-ends-atom? left) (sx-starts-atom? right))
      (previous-sexp-address next-path)
      next-path))

;; ---------- guides ----------
;; gap just before the form at `path`
(define (before-sexp-guide path)
  (lambda (before after)
    (case (sexp-path-compare (sexp-next-address before) path)
      [(-1) 1]
      [(1) -1]
      [(0) (if (and (sx-starts-form? after)
                    (not (and (sx-ends-atom? before) (sx-starts-atom? after))))
               0 1)])))

;; gap just after the form at `path`
(define (after-sexp-guide path)
  (define next-path (next-sexp-address path))
  (lambda (before after)
    (case (sexp-path-compare (sexp-next-address before) next-path)
      [(-1) 1]
      [(1) -1]
      [(0) (cond [(and (sx-ends-atom? before) (sx-starts-atom? after)) 1]
                 [(sx-ends-form? before) 0]
                 [else -1])])))

;; ---------- concrete cursor guides (built on zipper-core's pieces) ----------
;; These are the only callers of the `guide` constructor; the zipper stays
;; summary-agnostic and just calls the faces they fill in.

;; char: the char-count summary *is* the offset, so the projection is identity.
;;   move    : a point at the char index (the gap)
;;   resolve : read the two anchors -> the both-ends span (start end)
;;   carve   : a flat char span over the whole document
(define (char-guide i)
  (guide i
         (lambda (idx) ((point values) idx))
         (lambda (z) (let ([h (zipper-head z)])
                       (list (head-before h) (head-after h))))
         (lambda (seg) (local-span values (first seg) (second seg)))))

;; sexp: a seg-index is ((start end) path) -- char offsets within the form at
;; `path` (the frame). carve descends to that frame structurally (the path gives
;; edit-stable addressing), then carves the char span inside it (the offsets give
;; an exact, hole-marking span). `end` is recovered here by carving the frame;
;; reading it off an enriched right summary (sx-chars-to-close) is the alternative.
(define (carve-form path)
  (lambda (b t a) (carve2 (before-sexp-guide path) (after-sexp-guide path) b t a)))

(define (sexp-carve seg-idx)
  (match-define (list (list start end) path) seg-idx)
  (lambda (b t a)
    (define smr (rope-algebra t))
    (define-values (lF F rF) ((carve-form path) b t a))
    (define-values (fl m fr)
      ((local-span sx-chars start end) (smr b lF) F (smr rF a)))
    (values ((roper smr) lF fl) m ((roper smr) fr rF))))

(define (sexp-guide)
  (guide #f
         (lambda (idx) (before-sexp-guide idx))   ; movement to a structural gap
         (lambda (z) (error 'sexp-guide "plant sexp segs via select / sexp-form-span"))
         (lambda (seg) (sexp-carve seg))))

;; sexp-form-span: the seg-index selecting the form at `fpath`, framed by its
;; parent so deleting it leaves a gap (not a slurp of the next sibling). Read off
;; by carving the form and its parent and differencing the char offsets.
(define (sexp-form-span doc fpath)
  (define parent (parent-sexp-address fpath))
  (define smr (rope-algebra doc))
  (define e (smr ""))
  (define-values (l form r)  ((carve-form fpath)  e doc e))
  (define-values (lp par rp) ((carve-form parent) e doc e))
  (list (list (- (sx-chars (smr l)) (sx-chars (smr lp)))
              (- (sx-chars (smr r)) (sx-chars (smr rp))))
        parent))

;; ============================================================================
(module+ test
  (require rackunit)

  ;; --- the monoid: counts, atoms, balance, and chunk-invariance ---
  (define s (sexp "(a (b c) d)"))
  (check-equal? (sx-chars s) 11)
  (check-equal? (sx-atoms s) 4)            ; a b c d
  (check-equal? (sexp-depth s) 0)
  (check-true   (sexp-balanced? s))
  (check-true   (positive? (sexp-depth (sexp "(("))))      ; two unclosed opens
  (check-equal? (sexp-depth (sexp "((")) 2)

  ;; the same text chunked any which way gives the same summary (associativity)
  (check-equal? (sexp "(a (b c) d)")
                (sexp (sexp "(a (b") (sexp " c) d)")))
  (check-equal? (sx-atoms (sexp (sexp "a") (sexp "b")))  1)   ; "a"  + "b"  = one atom
  (check-equal? (sx-atoms (sexp (sexp "ab ") (sexp "c"))) 2)  ; "ab " + "c" = two atoms

  ;; --- addresses ---
  (check-equal? (sexp-next-address (sexp "")) '(0))
  (check-equal? (next-sexp-address '(0 2)) '(0 3))
  (check-equal? (previous-sexp-address '(0 3)) '(0 2))
  (check-equal? (parent-sexp-address '(0 3 2)) '(0 3))
  (check-equal? (sexp-path-compare '(0 2) '(0 3)) -1)
  (check-equal? (sexp-path-compare '(0 3 0 0) '(0 3)) 0)    ; trailing zeros are the same boundary

  ;; --- integration: navigate + edit through the concrete guides ---
  (define (focus z) (~a (head-rope (zipper-head z))))

  ;; char: navigate to a gap and insert -- the seg covers exactly what was typed
  (define hello ((roper char-count) "hello world"))
  (check-equal? (focus (insert (navigate ((start (char-guide 5)) hello)) "XYZ")) "XYZ")
  (check-equal? (text  (insert (navigate ((start (char-guide 5)) hello)) "XYZ")) "helloXYZ world")

  ;; char: select a range, delete it, reinsert -- round-trips exactly
  (define wsel (select ((start (char-guide 0)) hello) (list 6 0)))   ; "world"
  (check-equal? (focus wsel) "world")
  (check-equal? (text (delete wsel)) "hello ")
  (check-equal? (text (insert (delete wsel) "world")) "hello world")

  ;; sexp: select a form by its tree path
  (define sdoc ((roper sexp) "(a (b c) d)"))
  (define (sel p) (select ((start (sexp-guide)) sdoc) (sexp-form-span sdoc p)))
  (check-equal? (focus (sel '(0 2)))   "(b c)")   ; child 2 of the top form
  (check-equal? (focus (sel '(0 1)))   "a")       ; child 1
  (check-equal? (focus (sel '(0 3)))   "d")       ; child 3
  (check-equal? (focus (sel '(0 2 1))) "b")       ; grandchild

  ;; sexp: replace a form (insert over the selection), covering the new text
  (check-equal? (text  (insert (sel '(0 2)) "X")) "(a X d)")
  (check-equal? (focus (insert (sel '(0 2)) "X")) "X")

  ;; sexp: delete leaves a gap at the hole -- `d` is NOT slurped -- and reinsert round-trips
  (check-equal? (text (delete (sel '(0 2)))) "(a  d)")
  (check-equal? (text (insert (delete (sel '(0 2))) "(b c)")) "(a (b c) d)")

  ;; sexp: a chunked document selects the same forms (chunk-invariance through the zipper)
  (define sdoc2 ((roper sexp #:chunk-size 3) "(define square\n  (lambda (x)\n    (* x x)))"))
  (define (sel2 p) (select ((start (sexp-guide)) sdoc2) (sexp-form-span sdoc2 p)))
  (check-equal? (focus (sel2 '(0 2))) "square")
  (check-true   (string-prefix? (focus (sel2 '(0 3))) "(lambda"))
  (check-equal? (focus (sel2 '(0 3 2 1))) "x"))
