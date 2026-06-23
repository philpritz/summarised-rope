#lang racket

;; Summaries: the general summary toolkit -- the combinators and plain-text metrics,
;; independent of any one structure.  `bundle` (a product of summaries -- see below) is
;; the general combinator; then a group of plain-text metrics -- `char-smr` (the offset
;; axis), `word-smr` (seam-aware word count), and `linecol-smr` (line/column).  Last,
;; `buffer-smr` bundles the sexp summary instance (sexp-summary.rkt) with the three
;; metrics into one editor-buffer product.  The summary *protocol* (make-summary and the
;; gen:summary-part extension point) lives in rope-core; this file builds on it.  The
;; sexp summary instance and its highlighting seeds live in sexp-summary.rkt.

(require racket/match
         "../../rope-core.rkt"          ; make-summary; gen:summary-part (for bundle-val)
         "../../summaries/sexp-summary.rkt")      ; sexp-smr (for buffer-smr)

(provide bundle                    ; (bundle s1 s2 ...) -> the product smr
         (struct-out bundle-val)   ; the product summary value
         char-smr                  ; plain-text metric: the char offset axis
         word-smr (struct-out wc)  ; word count (seam-aware); wc-n reads the count
         linecol-smr (struct-out linecol)   ; line/column at a cut: linecol-lines / linecol-cols
         buffer-smr)               ; the editor-buffer bundle: sexp navigation + the metrics above

;; ---------- bundle: a product of summaries ----------
;; (bundle s1 s2 ...) -> the product smr; build & stamp ropes under it.  Its value is
;; a `bundle-val` carrying every component, keyed by the component's own smr.  Applying
;; a component smr to a bundle-val selects that component (gen:summary-part); any other
;; smr -- e.g. the product smr itself -- passes it through unchanged.  A guide/reader
;; for component c reads through it with (on g c) (helper-algebras' `on`).
;;
;; `bundle` is a MACRO (not a function): it captures each component's source identifier
;; so a bundle-val can print as (bundle [name value] ...) -- see bundle-write.

;; `names` is display-only for the printer
(struct bundle-val (slots names)        ; slots : #hasheq(smr -> value) ; names : #hasheq(smr -> symbol)
  #:transparent
  #:property prop:custom-write (lambda (bv port mode) (bundle-write bv port))
  #:methods gen:summary-part
  [(define (part->summary bv smr)
     (if (hash-has-key? (bundle-val-slots bv) smr)
         (hash-ref (bundle-val-slots bv) smr)
         bv))])

;; the to-string walk behind prop:custom-write (mirrors rope-write-text): prints
;; (bundle [name value] ...), one per line, names padded to the widest and sorted
;; (hasheq order is otherwise unstable).
(define (bundle-write bv port)
  (define names (bundle-val-names bv))
  (define (nm k) (hash-ref names k (lambda () (object-name k))))   ; fallback: 'smr
  (define entries
    (sort (for/list ([(k v) (in-hash (bundle-val-slots bv))]) (cons (nm k) v))
          symbol<? #:key car))
  (define w (apply max 0 (map (lambda (e) (string-length (symbol->string (car e)))) entries)))
  (fprintf port "(bundle")
  (for ([e (in-list entries)])
    (fprintf port "\n  [~a ~v]" (~a (car e) #:min-width w) (cdr e)))
  (fprintf port ")"))

;; build the product smr, recording the captured names (display-only) alongside the
;; slots; both hashes are built once and shared by every value the smr produces.
(define (make-bundle named)             ; named : (listof (cons smr name|#f))
  (define components (map car named))
  (define names (for/hasheq ([p (in-list named)] #:when (cdr p)) (values (car p) (cdr p))))
  (make-summary
   (lambda (str) (bundle-val (for/hasheq ([c (in-list components)]) (values c (c str))) names))
   (lambda (a b)  (bundle-val (for/hasheq ([c (in-list components)]) (values c (c a b))) names))))

;; capture each argument's source identifier for the printer; computed args -> #f
(define-syntax (bundle stx)
  (syntax-case stx ()
    [(_ c ...)
     (with-syntax ([(named ...)
                    (map (lambda (cc) (if (identifier? cc) #`(cons #,cc '#,cc) #`(cons #,cc #f)))
                         (syntax->list #'(c ...)))])
       #'(make-bundle (list named ...)))]))

;; ---------- plain-text metrics ----------
;; General (non-sexp) summaries, each a monoid over a text measure, read at a cut
;; off the all-left summary.  Two shapes recur (the law battery in summary-laws.rkt
;; checks both):
;;   pointwise   (make-summary measure +) -- the measure already distributes over
;;               ++, so the homomorphism is free (char-smr).
;;   seam-aware  a word can straddle a chunk boundary, so the value carries edge
;;               state and the combine reconciles the seam, like `sexp+` (word-smr;
;;               #f is the identity, short-circuited in the combine).

;; char count -- the offset axis.  string-length is O(1), so the measure is a
;; direct length, not a per-char scan.
(define char-smr (make-summary string-length +))

;; word count -- words are maximal non-whitespace runs.  Seam-aware: a word split
;; across the cut ("hel" ++ "lo") is ONE word, so the combine drops the straddler.
;; The value records whether each EDGE char is a word char; #f is the identity.
(struct wc (head n tail) #:transparent)        ; head/tail: is the edge char non-whitespace?
(define (word-leaf s)
  (and (positive? (string-length s))
       (wc (not (char-whitespace? (string-ref s 0)))
           (length (regexp-match* #px"\\S+" s))
           (not (char-whitespace? (string-ref s (sub1 (string-length s))))))))
(define (word+ x y)
  (or (and x y
           (match-let ([(wc xh xn xt) x] [(wc yh yn yt) y])
             (wc xh (- (+ xn yn) (if (and xt yh) 1 0)) yt)))  ; both word chars at the seam -> one word, not two
      x y))
(define word-smr (make-summary word-leaf word+))

;; line/column -- newline count plus the chars since the last newline.  Read at a
;; cut off the all-left summary: line (0-based) = `linecol-lines`, column (0-based)
;; = `linecol-cols` (add 1 each for editor display).  (linecol 0 0) is the identity,
;; so no #f sentinel; the combine keeps the left's trailing column only while the
;; right operand has no newline of its own.
(struct linecol (lines cols) #:transparent)
(define (linecol-leaf s)
  (linecol (for/sum ([c (in-string s)] #:when (char=? c #\newline)) 1)
           (string-length (last (regexp-split #rx"\n" s)))))   ; chars after the last \n
(define (linecol+ x y)
  (match-let ([(linecol la ca) x] [(linecol lb cb) y])
    (linecol (+ la lb) (if (zero? lb) (+ ca cb) cb))))
(define linecol-smr (make-summary linecol-leaf linecol+))

;; ---------- the buffer bundle ----------
;; The editor-buffer summary: sexp navigation AND the plain-text metrics in one
;; product, so a single rope built under `buffer-smr` caches everything the cursor
;; and a status line need.  Navigation reads the sexp slot exactly as before --
;; (on sand-spines sexp-smr) -- while (char-smr v) / (word-smr v) / (linecol-smr v)
;; select the metric slots off the same value.  Components are keyed by smr
;; IDENTITY (eq?), so read each slot through THESE provided bindings, never a fresh
;; (make-summary ...) -- a different object is a different key and misses the slot.
(define buffer-smr (bundle sexp-smr char-smr word-smr linecol-smr))

;; ============================================================================
(module+ test
  (require rackunit)

  ;; --- bundle: a product summary; each component smr selects its own part ---
  (let* ([cc (make-summary string-length +)]      ; a second summary: char count
         [b  (bundle sexp-smr cc)])
    (check-equal? (sexp-smr (b "(aa bb")) (sexp-smr "(aa bb"))   ; sexp part selected
    (check-equal? (cc (b "(aa bb")) 6)                            ; char part selected
    (check-equal? (b "(aa" " bb") (b "(aa bb")))                  ; product folds associatively

  ;; --- plain-text metrics: worked values + the law battery ---
  (check-equal? (char-smr "hello") 5)
  ;; count-where: a summary counting chars satisfying `pred` (pointwise, combine =
  ;; +).  A general plain-text family -- whitespace here, but also digits, a search
  ;; char, or quotes (str-smr's measure).  Lives here: nothing in the library uses it.
  (define (count-where pred)
    (make-summary (lambda (s) (for/sum ([c (in-string s)] #:when (pred c)) 1)) +))
  (check-equal? ((count-where char-whitespace?) "a b  c") 3)

  ;; word count: maximal non-whitespace runs; a word straddling a chunk seam is
  ;; counted once (the combine drops the double-count)
  (define (word-count s) (cond [(word-smr s) => wc-n] [else 0]))
  (check-equal? (word-count "the quick brown fox") 4)
  (check-equal? (word-count "  ") 0)
  (check-equal? (word-count "")  0)
  (check-equal? (word-smr "hel" "lo") (word-smr "hello"))   ; one word across the seam
  (check-equal? (word-smr "a b" " c")  (word-smr "a b c"))

  ;; line/column off the all-left summary, both 0-based
  (define (line/col s i)
    (let ([v (linecol-smr (substring s 0 i))]) (cons (linecol-lines v) (linecol-cols v))))
  (check-equal? (line/col "ab\ncd\nef" 0) '(0 . 0))
  (check-equal? (line/col "ab\ncd\nef" 4) '(1 . 1))    ; line 1, just before the 'd'
  (check-equal? (line/col "ab\ncd\nef" 6) '(2 . 0))
  (check-equal? (linecol-smr "ab\nc" "d\nef") (linecol-smr "ab\ncd\nef"))  ; trailing col carries the seam

  ;; the battery on each, over text with spaces, newlines, and parens
  (require "../../summaries/summary-laws.rkt" rackcheck)
  (define gen:text (gen:string (gen:one-of (string->list "ab  \n()")) #:max-length 16))
  (define metric-corpus (list "" " " "a" "ab cd" "a\nb\n" "\n\n" "  ab  " "x\ny z\nw"))
  (check-summary-laws char-smr    gen:text #:corpus metric-corpus)
  (check-summary-laws word-smr    gen:text #:corpus metric-corpus)
  (check-summary-laws linecol-smr gen:text #:corpus metric-corpus)

  ;; --- the buffer bundle: one value, every slot selected by its component smr ---
  (let ([v (buffer-smr "(define x\ny)")])
    (check-equal? (char-smr v) 12)                          ; char slot
    (check-equal? (wc-n (word-smr v)) 3)                    ; word slot: (define / x / y)
    (check-equal? (linecol-lines (linecol-smr v)) 1)        ; line/col slot
    (check-equal? (linecol-cols  (linecol-smr v)) 2)
    (check-equal? (sexp-smr v) (sexp-smr "(define x\ny)"))) ; sexp slot (what navigation reads)

  ;; --- the buffer bundle on a ROPE: a component smr reads its slot straight off it ---
  ;; (coerce's rope arm: (char-smr r) = (char-smr (rope-summary r)) = the char slot)
  (let ([r ((make-rope buffer-smr) "(define x\ny)")])
    (check-equal? (char-smr r) 12)                          ; char slot off the bundle rope
    (check-equal? (wc-n (word-smr r)) 3)                    ; word slot
    (check-equal? (sexp-smr r) (sexp-smr "(define x\ny)")))) ; sexp slot
