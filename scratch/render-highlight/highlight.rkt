#lang racket

;; WORKING COPY of scratch/keyword-highlight.rkt, derived from the frozen byte-for-byte
;; snapshot keyword-highlight.snapshot.rkt (md5 1AD969E84F1E5E90AF7C70908D016D05).
;;
;; The highlighting LOGIC below is the snapshot's, verbatim. The ONLY changes are
;; mechanical, so the renderer can import it:
;;   - require paths are ../../ (this file is one level deeper than the original);
;;   - a `provide` exposes the reusable pieces;
;;   - the demo moved under `(module+ main ...)` so requiring this runs no side effects;
;;   - `analyze-head` is factored out of `analyze` -- the span analysis on a head we
;;     ALREADY have (b m a), so the renderer feeds it the head it navigated to instead
;;     of re-cutting a window.
;;
;; Run standalone:  racket scratch/render-highlight/highlight.rkt   (reproduces the snapshot)

(require "../../rope-core.rkt"         ; make-summary make-rope multisect
         "../../summaries/summaries.rkt"     ; bundle char-smr
         "../../summaries/sexp-summary.rkt"  ; sexp-smr strsexp-smr strsexp-spines strsexp-in-string?
         "../../helper-algebras.rkt") ; on

(provide make-kw-smr kw-trail-text kw-lead-text
         segments str-spans-of head-kw-spans paren-spans
         openers closers atom-char? intify
         analyze-head analyze paint)

;; ---------- the alphabet ----------
(define openers (string->list "([{"))
(define closers (string->list ")]}"))
(define delims  (append openers closers (list #\")))
(define (atom-char? c) (not (or (char-whitespace? c) (memv c delims))))

;; ---------- the keyword boundary monoid ----------
(struct kw (lead all? trail) #:transparent)
(define (make-kw-smr keywords)
  (define cap (add1 (apply max 1 (map string-length keywords))))
  (define (capL s) (substring s 0 (min cap (string-length s))))
  (define (capR s) (substring s (max 0 (- (string-length s) cap))))
  (define (run s lo hi step)
    (let loop ([i lo]) (if (and (not (= i hi)) (atom-char? (string-ref s i))) (loop (+ i step)) i)))
  (define (kw-leaf s)
    (and (positive? (string-length s))
         (let* ([n (string-length s)] [lead (run s 0 n 1)] [trail (run s (sub1 n) -1 -1)])
           (kw (capL (substring s 0 lead)) (= lead n) (capR (substring s (add1 trail) n))))))
  (define (kw+ x y)
    (or (and x y
             (kw (if (kw-all? x) (capL (string-append (kw-lead x)  (kw-lead y)))  (kw-lead x))
                 (and (kw-all? x) (kw-all? y))
                 (if (kw-all? y) (capR (string-append (kw-trail x) (kw-trail y))) (kw-trail y))))
        x y))
  (make-summary kw-leaf kw+))
(define (kw-trail-text v) (if (kw? v) (kw-trail v) ""))
(define (kw-lead-text  v) (if (kw? v) (kw-lead  v) ""))

;; ---------- code/string segmentation ----------
(define (segments fr entry-in?)
  (define n (string-length fr))
  (define (close-from j) (cond [(>= j n) n] [(char=? (string-ref fr j) #\") (add1 j)] [else (close-from (add1 j))]))
  (define (open-from  j) (cond [(>= j n) n] [(char=? (string-ref fr j) #\") j]        [else (open-from  (add1 j))]))
  (let loop ([i    (if entry-in? (close-from 0) 0)]
             [segs (if entry-in? (list (list 'str 0 (close-from 0))) '())])
    (define op (open-from i))
    (cond
      [(>= op n) (reverse (if (< i n) (cons (list 'code i n) segs) segs))]
      [else (define cl (close-from (add1 op)))
            (loop cl (cons (list 'str op cl) (if (= op i) segs (cons (list 'code i op) segs))))])))

(define (str-spans-of segs)
  (for/list ([seg (in-list segs)] #:when (eq? (first seg) 'str)) (cons (second seg) (third seg))))

;; ---------- positional keyword spans ----------
;; `counts` is the per-level form-count stack (innermost-first, last entry = top level), so
;; (car counts) is the form count at the innermost open paren and (length counts) is depth+1.
;; A code atom is in HEAD position iff depth>=1 and that count is 0 -- it's the first form.
;; A completed form (atom, closed paren, or a whole string) bumps the innermost count.
(define (bump counts) (cons (add1 (car counts)) (cdr counts)))

(define (head-kw-spans keywords fr segs bs as entry-counts)
  (define n (string-length fr))
  (define kwset (list->set keywords))
  (let seg-loop ([segs segs] [counts entry-counts] [acc '()])
    (cond
      [(null? segs) (sort acc < #:key car)]
      [(eq? (first (car segs)) 'str) (seg-loop (cdr segs) (bump counts) acc)]   ; a string is one form
      [else
       (match-define (list _ a b) (car segs))
       (let ch-loop ([k a] [counts counts] [acc acc])
         (cond
           [(>= k b) (seg-loop (cdr segs) counts acc)]
           [(memv (string-ref fr k) openers) (ch-loop (add1 k) (cons 0 counts) acc)]
           [(memv (string-ref fr k) closers)
            (ch-loop (add1 k) (if (> (length counts) 1) (bump (cdr counts)) (bump counts)) acc)]
           [(atom-char? (string-ref fr k))
            (define e (let scan ([j k]) (if (and (< j b) (atom-char? (string-ref fr j))) (scan (add1 j)) j)))
            (define head? (and (> (length counts) 1) (= (car counts) 0)))
            (define left  (if (= k 0) (kw-trail-text bs) ""))
            (define right (if (= e n) (kw-lead-text  as) ""))
            (define hit?  (and head? (set-member? kwset (string-append left (substring fr k e) right))))
            (ch-loop e (bump counts) (if hit? (cons (cons k e) acc) acc))]
           [else (ch-loop (add1 k) counts acc)]))])))   ; whitespace

;; ---------- string-aware paren depth ----------
(define (paren-spans fr segs entry-depth)
  (let loop ([segs segs] [depth entry-depth] [acc '()])
    (cond
      [(null? segs) (sort acc < #:key car)]
      [(eq? (first (car segs)) 'str) (loop (cdr segs) depth acc)]
      [else
       (match-define (list _ a b) (car segs))
       (define-values (d2 acc2)
         (for/fold ([d depth] [out acc]) ([k (in-range a b)])
           (define c (string-ref fr k))
           (cond
             [(memv c openers) (values (add1 d)         (cons (cons k d)                out))]
             [(memv c closers) (values (max 0 (sub1 d)) (cons (cons k (max 0 (sub1 d))) out))]
             [else             (values d out)])))
       (loop (cdr segs) d2 acc2)])))

;; ---------- riding the rope ----------
(define (intify x) (inexact->exact (floor x)))   ; spine slots are integers at clean cuts

;; analyze-head: the span analysis on a head we ALREADY have -- b/m/a are the focus's
;; before, focus, and after, b/a as bundle values, m the focus rope. Slots are read
;; through the given kw-smr and the global strsexp-smr (eq?-keyed in the bundle). This
;; is `analyze` with the cut removed, so the renderer feeds heads it has navigated to.
(define (analyze-head keywords kw-smr b m a)
  (define sx-b (strsexp-smr b))
  (define sx-m (strsexp-smr m))
  (define entry-in?     (strsexp-in-string? sx-b))
  (define-values (front _) (strsexp-spines sx-b sx-m))
  (define entry-depth   (sub1 (length front)))
  (define entry-counts  (map intify front))     ; per-level form counts at the focus's start
  (define fr (~a m))
  (define segs (segments fr entry-in?))
  (values fr entry-in? entry-counts
          (head-kw-spans keywords fr segs (kw-smr b) (kw-smr a) entry-counts)
          (str-spans-of segs)
          (paren-spans fr segs entry-depth)))

;; analyze: the snapshot's standalone path -- cut a window [i,j) then analyze its head.
(define (analyze keywords doc i j)
  (define kw-smr (make-kw-smr keywords))
  (define buf    (bundle char-smr kw-smr strsexp-smr))
  (define rope   ((make-rope buf) doc))
  (define ((at n) L R) (cond [(< L n) 1] [(> L n) -1] [else 0]))
  (define-values (b m a)
    ((multisect buf (vector (on (at i) char-smr) (on (at j) char-smr))) rope))
  (analyze-head keywords kw-smr b m a))

;; ---------- display ----------
(define (paint fr kwS strS)       ; «kw»  ⟦str⟧
  (define tagged (sort (append (map (lambda (s) (list (car s) (cdr s) "«" "»")) kwS)
                               (map (lambda (s) (list (car s) (cdr s) "⟦" "⟧")) strS))
                       < #:key first))
  (let loop ([i 0] [ts tagged] [out '()])
    (match ts
      ['() (apply string-append (reverse (cons (substring fr i) out)))]
      [(cons (list a b o c) rest)
       (loop b rest (list* (string-append o (substring fr a b) c) (substring fr i a) out))])))

;; ---------- the snapshot demo (verbatim), now under main so `require` is side-effect-free ----------
(module+ main
  (define kws '("define" "lambda" "let" "if" "cond"))
  (printf "keywords: ~s\n\n" kws)

  (define (whole label doc)
    (let-values ([(fr in? cnt kwS strS pS) (analyze kws doc 0 (string-length doc))])
      (printf "~a  ~s\n   ~a\n   head-kw=~s str=~s parens=~s\n\n" label doc (paint fr kwS strS) kwS strS pS)))

  (whole "1. two heads      " "(define f (lambda (x) x))")
  (whole "2. same word, two positions" "(define define)")
  (whole "4. all together   " "(if (f x) \"define\" (define y))")

  (define S "(foo define)")
  (let-values ([(fr in? cnt kwS strS pS) (analyze kws S 5 (string-length S))])
    (printf "3a. window [5,~a) of  ~s  -- define is an ARGUMENT\n" (string-length S) S)
    (printf "    entry form-counts = ~s   (innermost = ~a -> head slot already taken by foo)\n" cnt (car cnt))
    (printf "    focus  : ~s\n    ~a\n    head-kw=~s\n\n" fr (paint fr kwS strS) kwS))

  (define S2 "(foo) (define x)")
  (let-values ([(fr in? cnt kwS strS pS) (analyze kws S2 7 (string-length S2))])
    (printf "3b. window [7,~a) of  ~s  -- define is the OPERATOR\n" (string-length S2) S2)
    (printf "    entry form-counts = ~s   (innermost = ~a -> head slot open, set by the ( in bs)\n" cnt (car cnt))
    (printf "    focus  : ~s\n    ~a\n    head-kw=~s\n" fr (paint fr kwS strS) kwS)))
