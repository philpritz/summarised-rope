#lang racket

;; Summaries: the general summary combinators plus the concrete summary algebras.
;; The general piece is `bundle` (a product of summaries -- see below), then a group
;; of plain-text metrics -- `char-smr` (the offset axis), the `count-where` family,
;; `word-smr` (seam-aware word count), and `linecol-smr` (line/column).  The bulk of
;; the file is the sexp instance: its monoid (`sexp-smr`) AND the navigation read
;; interface `sand-spines`, which reads a cut as the front/back spines the sexp
;; layer compares against (sexp-edit.rkt).  Two highlighting seeds follow it: a naive
;; `str-smr` (a quote count) and `strsexp-smr`, which reuses the sexp algebra gated by
;; string state so parens inside strings are discounted, producing index spines like
;; `sand-spines` does.  Last, `buffer-smr` bundles `sexp-smr` with the three metrics
;; into one editor-buffer product.  The summary *protocol* (make-summary and the gen:summary-part
;; extension point) lives in rope-core; this file builds on it.
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
         char-smr count-where      ; plain-text metrics: char offset; chars matching a predicate
         word-smr (struct-out wc)  ; word count (seam-aware); wc-n reads the count
         linecol-smr (struct-out linecol)   ; line/column at a cut: linecol-lines / linecol-cols
         buffer-smr                ; the editor-buffer bundle: sexp navigation + the metrics above
         sexp-smr                  ; the smr  -- (sexp-smr str), ((make-rope sexp-smr) ...)
         sand-spines               ; (sand-spines L R) -> (values front back): read a cut as spines
         paired-sexp-smr           ; SEPARATE kind-matching multi-bracket summary (sexp-smr stays ( )-only)
         paired-sand-spines        ; (paired-sand-spines L R) -> spines off a paired frontier
         front-kinds back-kinds    ; bracket glyph per spine level (openers / closers)
         same-kind? kind           ; bracket-kind predicate + family
         str-smr                   ; (str-smr str) -> quote count: the naive in-string seed
         strsexp-smr               ; sexp algebra gated by string state (parens in strings discounted)
         strsexp-spines            ; (strsexp-spines L R) -> spines, gated by the left's string parity
         strsexp-in-string?)       ; (strsexp-in-string? L) -> in a string at the cut after L?

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

;; ---------- plain-text metrics ----------
;; General (non-sexp) summaries, each a monoid over a text measure, read at a cut
;; off the all-left summary.  Two shapes recur (the law battery in summary-laws.rkt
;; checks both):
;;   pointwise   (make-summary measure +) -- the measure already distributes over
;;               ++, so the homomorphism is free (char-smr, count-where).
;;   seam-aware  a word can straddle a chunk boundary, so the value carries edge
;;               state and the combine reconciles the seam, like `sexp+` (word-smr;
;;               #f is the identity, short-circuited in the combine).

;; char count -- the offset axis.  string-length is O(1), so the measure is direct
;; (not (count-where (lambda (_) #t)), which would scan every char).
(define char-smr (make-summary string-length +))

;; count-where: chars satisfying `pred`.  A family -- str-smr is its quote instance
;; ((count-where (lambda (c) (char=? c #\"))); whitespace, digits, a search char are
;; others.  Pointwise: combine = +, identity (the empty count) = 0.
(define (count-where pred)
  (make-summary (lambda (s) (for/sum ([c (in-string s)] #:when (pred c)) 1)) +))

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
;; ---------- reading a cut as spines ----------
;; `sand-spines` is the summary's read interface for navigation -- the one reader
;; of the frontier fields it needs.  At a cut it reads the all-left `front` and
;; all-right `back` spines, innermost-first, with the ½ refinement on the HEAD
;; only: at a form start the head is the raw integer; mid-atom it is pushed
;; half-way into the atom (the one structurally invisible interior -- frames'
;; interiors are spine-visible as depth, atoms' are not); whitespace binds to the
;; previous form, leaning -½.  `front` slots are 0-based (the stored +1 drops at
;; the read), `back` as stored (-1 = after the last form).  The spine algebra that
;; compares against these lives in sexp-edit.rkt.

;; classify a cut by the two char-classes touching it: tail of L, head of R.
;;   start  a form begins here (atom or "(")        -- flush, no lean
;;   end    right before a ")" or the document end  -- flush, no lean
;;   mid    straddling an atom                       -- front -½, back +½
;;   lean   whitespace; binds to the previous form   -- front -½, back -½
(define (cut-kind L R)
  (case (sexp-head R)
    [(atom)  (if (eq? (sexp-tail L) 'atom) 'mid 'start)]
    [(open)  'start]
    [(close) 'end]
    [(ws)    'lean]
    [else    'end]))                       ; R empty: the document end

;; both full spines at a cut, innermost-first, ½ baked into the heads.
(define (sand-spines L R)
  (match-define (cons fh fr) (append (map sub1 (sexp-opens L)) (list (sexp-forms L))))
  (match-define (cons bh br) (append (sexp-closes R) (list (- (add1 (sexp-forms R))))))
  (case (cut-kind L R)
    [(start end) (values (cons fh fr)        (cons bh br))]
    [(mid)       (values (cons (- fh 1/2) fr) (cons (+ bh 1/2) br))]
    [(lean)      (values (cons (- fh 1/2) fr) (cons (- bh 1/2) br))]))

;; ---------- paired: kind-matching multi-bracket sexp summary ----------
;; A SEPARATE summary from `sexp-smr` (which stays bracket-blind, ( ) only).  Paired
;; recognizes ( ) [ ] { }, tags each level with its bracket kind, and matches a closer
;; to the innermost open OF ITS KIND (the HTML-style "pop to the matching bracket",
;; 2b), skipping wrong-kind opens.  Its frontier entries are (kind . count) pairs (the
;; bracket glyph + the signed slot), so it carries its OWN leaf / merge / bump / spine
;; reads -- it shares only the `frontier` struct, the #f-safe readers, and `cut-kind`.
;; On WELL-FORMED input each closer's match is innermost, so paired's structure equals
;; a bracket-aware nesting parse; the skip fires only on malformed input, where the
;; offset accounting is not yet guaranteed associative (skipped opens are dropped).
(define (opener? c) (memv c '(#\( #\[ #\{)))
(define (closer? c) (memv c '(#\) #\] #\})))
(define (kind c) (case c [(#\( #\)) 'round] [(#\[ #\]) 'square] [(#\{ #\}) 'curly] [else #f]))
(define (same-kind? a b) (eq? (kind a) (kind b)))            ; do two brackets pair?
(define (bracket-class c)                                    ; head/tail class, brackets included
  (cond [(opener? c) 'open] [(closer? c) 'close] [(char-whitespace? c) 'ws] [else 'atom]))

(define (paired-tokens s)                                    ; brackets and atom runs, in order
  (for/list ([t (in-list (regexp-match* #px"[][(){}]|[^][(){}\\s]+" s))])
    (define c (string-ref t 0))
    (if (eq? (bracket-class c) 'atom) 'atom c)))             ; -> a bracket char | 'atom

(define (paired-bump forms opens)                            ; bump the innermost open's count
  (if (null? opens)
      (values (add1 forms) opens)
      (match-let ([(cons k n) (car opens)]) (values forms (cons (cons k (add1 n)) (cdr opens))))))
(define (paired-add-inner opens n)
  (match-let ([(cons k m) (car opens)]) (cons (cons k (+ m n)) (cdr opens))))

(define (skip-match opens k)        ; pop wrong-kind opens; -> the stack just past the kind match
  (cond [(null? opens) opens]
        [(same-kind? (caar opens) k) (cdr opens)]
        [else (skip-match (cdr opens) k)]))
(define (has-kind? opens k) (for/or ([e (in-list opens)]) (same-kind? (car e) k)))

(define (paired-leaf s)
  (and (positive? (string-length s))
       (let-values
           ([(closes forms opens)
             (for/fold ([closes '()] [forms 0] [opens '()])
                       ([tok (in-list (paired-tokens s))])
               (cond
                 [(eq? tok 'atom)       (let-values ([(f o) (paired-bump forms opens)]) (values closes f o))]
                 [(opener? tok)         (values closes forms (cons (cons tok 1) opens))]
                 [(has-kind? opens tok) (let-values ([(f o) (paired-bump forms (skip-match opens tok))])  ; close its kind
                                          (values closes f o))]
                 [else                  (values (cons (cons tok (- (add1 forms))) closes) 1 opens)]))])    ; dangling
         (frontier (bracket-class (string-ref s 0))
                   (reverse closes) forms opens
                   (bracket-class (string-ref s (sub1 (string-length s))))))))

(define (paired-drop y)             ; the mid-atom continuation, on (kind . count) closes
  (match y
    [(frontier _ (cons (cons k cv) rest) _ _ _)
     (struct-copy frontier y [closes (cons (cons k (add1 cv)) rest)])]
    [_ (struct-copy frontier y [forms (sub1 (frontier-forms y))])]))

(define (merge-paired forms opens closes right-forms right-opens)
  (let loop ([forms forms] [stack opens] [closes closes] [out '()])
    (match closes
      ['()
       (if (null? stack)
           (values (reverse out) (+ forms right-forms) right-opens)
           (let ([stack (if (zero? right-forms) stack (paired-add-inner stack right-forms))])
             (values (reverse out) forms (append right-opens stack))))]
      [(cons (cons k cv) rest)
       (if (has-kind? stack k)
           (loop forms (skip-match stack k) rest out)                  ; matching opener -> cancel (drop orphans)
           (loop 0 stack rest (cons (cons k (- cv forms)) out)))])))   ; no match -> dangling

(define (paired+ x y)
  (or (and x y
           (let ([y (if (and (eq? (frontier-tail x) 'atom) (eq? (frontier-head y) 'atom))
                        (paired-drop y) y)])
             (match-let ([(frontier xh xc xf xo _) x] [(frontier _ yc yf yo yt) y])
               (define-values (closes forms opens) (merge-paired xf xo yc yf yo))
               (frontier xh (append xc closes) forms opens yt))))
      x y))
(define paired-sexp-smr (make-summary paired-leaf paired+))

;; paired's spine reads (entries are (kind . count); slot = cdr, glyph = car).  cut-kind
;; is shared -- it reads only the head/tail classes, which `bracket-class` supplies.
(define (paired-sand-spines L R)
  (match-define (cons fh fr) (append (map (lambda (e) (sub1 (cdr e))) (sexp-opens L)) (list (sexp-forms L))))
  (match-define (cons bh br) (append (map cdr (sexp-closes R)) (list (- (add1 (sexp-forms R))))))
  (case (cut-kind L R)
    [(start end) (values (cons fh fr)        (cons bh br))]
    [(mid)       (values (cons (- fh 1/2) fr) (cons (+ bh 1/2) br))]
    [(lean)      (values (cons (- fh 1/2) fr) (cons (- bh 1/2) br))]))
(define (front-kinds L) (map car (sexp-opens L)))   ; open brackets, innermost-first
(define (back-kinds  R) (map car (sexp-closes R)))  ; close brackets, innermost-first

;; ---------- string summary (naive) ----------
;; The seed of syntax highlighting in the summary: in-string state as a raw quote
;; count.  The value is the number of " in a fragment; at a cut, (str-smr L) is the
;; quote count to the left, and the in/out-of-string reading (a separate step) is its
;; parity.  Naive on purpose -- it counts EVERY ", so escapes (\"), quotes inside
;; comments, #\" char literals, and |...| symbols are not yet discounted.
(define (str-leaf s)
  (for/sum ([c (in-string s)] #:when (char=? c #\")) 1))
(define str-smr (make-summary str-leaf +))   ; combine = +, identity (str-leaf "") = 0

;; ---------- string + sexp summary (gated) ----------
;; Reuses the sexp algebra, gated by string state: a " toggles in/out of a string,
;; and sexp tokens (parens AND atoms) inside a string are inert -- the whole string
;; collapses to ONE form, an atom with an opaque interior.  Since a fragment cannot
;; know whether it BEGINS inside a string (that depends on everything to its left),
;; the value carries the sexp frontier parsed under each entry mode -- entered in
;; code, and entered mid-string -- plus the quote count (its parity is the gate).
;; The combine reuses `sexp+`; the only new logic is selecting which of the right
;; operand's two frontiers to splice, by the left's parity.
;;
;; Each frontier is built by transforming the text to its code-equivalent: every
;; string becomes a single delimited placeholder atom ("~") with its interior
;; removed, so the existing tokenizer / fold / sand-spines treat the string exactly
;; like an atom -- one form, a spine slot, a ½-leaned interior.
(struct cs (quotes code string) #:transparent)   ; count + frontier-if-code + frontier-if-string

(define (transform s start-in-string?)            ; -> code-equivalent text (strings -> "~")
  (define out (open-output-string))
  (let loop ([chs (string->list s)] [in? start-in-string?])
    (cond
      [(null? chs) (get-output-string out)]
      [(char=? (car chs) #\")                     ; a quote toggles the mode
       (if in? (write-char #\space out) (write-string " ~" out))   ; close -> delimiter; open -> +placeholder
       (loop (cdr chs) (not in?))]
      [in?  (loop (cdr chs) in?)]                 ; string interior: drop it
      [else (write-char (car chs) out)            ; code char: keep
            (loop (cdr chs) in?)])))

(define (strsexp-leaf s)
  (cs (for/sum ([c (in-string s)] #:when (char=? c #\")) 1)
      (sexp-leaf (transform s #f))                ; frontier if entered in code
      (sexp-leaf (transform s #t))))              ; frontier if entered in a string

(define (strsexp+ a b)
  (match-define (cs qa ca sa) a)
  (match-define (cs qb cb sb) b)
  (define flip? (odd? qa))                         ; mode at the seam = entry XOR parity(a)
  (cs (+ qa qb)
      (sexp+ ca (if flip? sb cb))                  ; whole entered in code
      (sexp+ sa (if flip? cb sb))))                ; whole entered in a string

(define strsexp-smr (make-summary strsexp-leaf strsexp+))

;; reads at a cut: in-string is the left's quote parity; the spines reuse sand-spines
;; on L's code frontier and R's parity-selected frontier (R's entry mode = L's parity).
(define (strsexp-in-string? L) (odd? (cs-quotes L)))
(define (strsexp-spines L R)
  (sand-spines (cs-code L)
               (if (odd? (cs-quotes L)) (cs-string R) (cs-code R))))

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

  ;; --- head/tail char classes ---
  (check-eq? (sexp-tail (sexp-smr "(aa bb cc")) 'atom)
  (check-eq? (sexp-head (sexp-smr "(p q) cc)")) 'open)
  (check-eq? (sexp-head (sexp-smr ") cc)")) 'close)

  ;; --- sand-spines: read a cut as front/back spines (the navigation interface) ---
  (let-values ([(front back) (sand-spines (sexp-smr "(aa ") (sexp-smr "bb cc)"))])
    (check-equal? front '(1 0))      ; ^bb: child 1 at the top, slot 0 within
    (check-equal? back  '(-3 -2)))

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

  ;; --- paired-sexp-smr: a SEPARATE bracket-aware summary; ( [ { matched by kind.
  ;;     `sexp-smr` above stays bracket-blind ([] {} are atoms) -- brackets live here. ---
  (check-true  (same-kind? #\( #\)))  (check-false (same-kind? #\( #\]))
  (define (popens x) (map cdr (sexp-opens (paired-sexp-smr x))))   ; counts off paired (kind . n) entries
  (check-equal? (popens "[")     '(1))                             ; [ opens a level
  (check-equal? (popens "(a [b") '(2 2))                           ; nested, multi-kind
  (check-equal? (front-kinds (paired-sexp-smr "(a [b")) (list #\[ #\())   ; bracket per level, innermost-first
  ;; mismatch: the ] does not close the ( -- the ( stays open and the ] dangles
  (check-equal? (sexp-opens (paired-sexp-smr "(a]")) (list (cons #\( 2)))
  ;; associativity over WELL-FORMED multi-kind nesting (paired's safe domain)
  (define (paired-chunked str k)
    (apply paired-sexp-smr (for/list ([i (in-range 0 (string-length str) k)])
                             (substring str i (min (string-length str) (+ i k))))))
  (for* ([str (list "(a [b c] d)" "(let ([x 1] [y 2]) (+ x y))" "{a [b (c)] d}"
                    "([{}])" "(f [g {h}])" "()" "(aa (p q) cc)")]
         [k (in-range 1 6)])
    (check-equal? (paired-chunked str k) (paired-sexp-smr str) (format "paired chunk ~a of ~s" k str)))

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

  (check-summary-laws sexp-smr gen:sexp-doc #:corpus sexp-corpus)

  ;; --- plain-text metrics: worked values + the law battery ---
  (check-equal? (char-smr "hello") 5)
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
    (check-equal? (sexp-smr r) (sexp-smr "(define x\ny)"))) ; sexp slot

  ;; --- gated string + sexp summary: sexp tokens inside strings are inert ---
  (define (sx str) (strsexp-smr str))
  ;; the inner "(" does not open a level -- the string is one form, like an atom `~`
  (check-equal? (cs-code (sx "(a \"(\" b")) (sexp-smr "(a ~ b"))
  (check-equal? (cs-code (sx "(\"((((\")")) (sexp-smr "(~)"))   ; depth back to 0 after a parens-full string
  ;; in-string at a cut = quote parity to the left
  (check-true  (strsexp-in-string? (sx "(a \"")))
  (check-false (strsexp-in-string? (sx "(a \"x\" ")))
  ;; spines: the string occupies its own child slot (a=child 0, string=child 1, b=child 2)
  (check-equal? (let-values ([(f b) (strsexp-spines (sx "(a \"x\" ") (sx "b)"))]) f) '(2 0))
  ;; associativity: chunked == whole, over strings full of parens (and plain sexps too)
  (define (sx-chunked str k)
    (apply strsexp-smr (for/list ([i (in-range 0 (string-length str) k)])
                         (substring str i (min (string-length str) (+ i k))))))
  (for* ([str (list "(a \"(\" b)" "\"(\"" "(\"))((\")" "x \"y z\" w"
                    "(define s \"hi (there)\")" "tail \" mid ( \" end"
                    "()" "(aa (p q) cc)" "\"unclosed (")]
         [k (in-range 1 6)])
    (check-equal? (sx-chunked str k) (strsexp-smr str)
                  (format "strsexp chunk ~a of ~s" k str))))
