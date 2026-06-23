#lang racket

;; Integrate syntax highlighting into the line renderer.
;;
;; ONE persistent zipper over a buffer bundle. Per visible line we navigate the zipper
;; to that line's segment, read its head (before . focus . after), and paint the line by
;; TWO independent reads off that head:
;;   - syntax: `analyze-head` (highlight.rkt) derives keyword/string/paren-depth spans,
;;     seeding its lexer state (in-string?, paren depth, per-level form counts for
;;     head-position keywords) from `before` -- no rescan;
;;   - cursor: the single editing cursor's segment, measured once in (line,col) off its
;;     own head, marked ONCE (one ⟦…⟧ pair overall, not per line).
;;
;; Line splitting needs no leading-newline bit: each line is the segment
;; [bol L, bol (L+1)) cut by two col-0 guides (col 0 never clamps), and the trailing
;; newline is stripped for display. The horizontal-window clamp (and the bol? question)
;; only returns when columns [n,m) come in; deferred.
;;
;; Run:  racket scratch/render-highlight/renderer.rkt

(require racket/future
         "../../rope-core.rkt"        ; make-rope
         "../../summaries/summaries.rkt"     ; bundle char-smr linecol-smr (struct-out linecol)
         "../../summaries/sexp-summary.rkt"  ; strsexp-smr (a bundle component)
         "../../zipper-core.rkt"      ; start zipper-guide zipper-focus on-edges
         "highlight.rkt")             ; make-kw-smr analyze-head

;; exposed for the cost harness (costs.rkt)
(provide make-hl open-doc render render/par render/par-chunked line-head cursor-span col line-end (struct-out hl))

;; ---------- the line/column guide ----------
;; (col line k): the cut at column k on `line`, reading linecol off each bundle value.
;; At k=0 it never clamps (column 0 is always the line start), which is all the line
;; cut below needs.
(define ((col line k) L R)
  (match-define (linecol _ ll lc) (linecol-smr L))
  (cond [(not (= ll line)) (if (< ll line) 1 -1)]
        [(< lc k) 1]
        [(> lc k) -1]
        [else 0]))

;; (line-end line): start-of-`line`, but clamped to EOF so the LAST line's end (whose
;; target line doesn't exist) stops at the document end instead of running off it. EOF
;; is the cut whose right side is empty -- char-smr of R is 0.
(define ((line-end line) L R)
  (if (zero? (char-smr R)) 0 ((col line 0) L R)))

;; ---------- a highlighter + its buffer bundle ----------
(struct hl (keywords kw-smr buf) #:transparent)
(define (make-hl keywords)
  (define kw-smr (make-kw-smr keywords))
  (hl keywords kw-smr (bundle char-smr kw-smr strsexp-smr linecol-smr)))

(define (open-doc h doc)
  (start (hl-buf h) ((make-rope (hl-buf h)) doc) (vector (col 0 0) (col 0 0))))

;; ---------- reading heads off the one zipper ----------
;; line L's head: navigate to its segment [bol L, bol (L+1)); return before, focus, after.
(define (line-head z L)
  (define z* ((setter zipper-guide (vector (col L 0) (line-end (add1 L)))) z))
  (define m ((viewer zipper-focus) z*))
  (define-values (b a) ((on-edges values (lambda (bb _) bb) (lambda (_ aa) aa)) z*))
  (values b m a))

;; the editing cursor's segment, in (line,col): linecol off its head's two edges.
(define (cursor-span z)
  ((on-edges values
             (lambda (bb _) (linecol-smr bb))     ; start (line . col)
             (lambda (bm _) (linecol-smr bm)))    ; end   (line . col)
   z))

;; ---------- painting one line ----------
;; markers, by priority at a shared offset (outer cursor wraps syntax wraps depth tags):
;;   0 ⟦   1 « ‹ (opens)   2 paren-depth   3 » › (closes)   4 ⟧
(define supers "⁰¹²³⁴⁵⁶⁷⁸⁹")
(define (super n)
  (list->string (for/list ([c (in-string (number->string n))]) (string-ref supers (- (char->integer c) 48)))))

(define (splice s inserts)                ; inserts: (offset priority text)
  (define sorted (sort inserts (lambda (a b) (or (< (first a) (first b))
                                                 (and (= (first a) (first b)) (< (second a) (second b)))))))
  (let loop ([i 0] [ins sorted] [out '()])
    (match ins
      ['() (apply string-append (reverse (cons (substring s i) out)))]
      [(cons (list at _ mk) rest) (loop at rest (list* mk (substring s i at) out))])))

(define (mark-line text kwS strS pS L sl sc el ec)
  (define len (string-length text))
  (define (ins at pri s) (and (<= 0 at len) (list at pri s)))
  (define raw
    (append
     (append-map (lambda (s) (list (ins (car s) 2 "«") (ins (cdr s) 3 "»"))) kwS)
     (append-map (lambda (s) (list (ins (car s) 2 "‹") (ins (cdr s) 3 "›"))) strS)
     (map (lambda (p) (ins (add1 (car p)) 0 (super (cdr p)))) pS)   ; depth tag binds to its bracket
     (list (and (= L sl) (ins sc 1 "⟦"))
           (and (= L el) (ins ec 4 "⟧")))))
  (splice text (filter values raw)))

;; ---------- the per-line work, shared by every variant ----------
;; navigate to line L off the persistent z, analyze its head, paint. A PURE read --
;; nothing is mutated and z is immutable -- so it is safe to run in a future.
(define (render-line h z sl sc el ec L)
  (define-values (b m a) (line-head z L))
  (define-values (fr in? cnt kwS strS pS) (analyze-head (hl-keywords h) (hl-kw-smr h) b m a))
  (define text (string-trim fr "\n" #:left? #f))
  (list L in? (sub1 (length cnt)) (mark-line text kwS strS pS L sl sc el ec)))

;; measure the cursor ONCE, hand its (line,col) corners to `proc`.
(define (with-cursor z proc)
  (define-values (cs ce) (cursor-span z))
  (match-define (linecol _ sl sc) cs)
  (match-define (linecol _ el ec) ce)
  (proc sl sc el ec))

;; ---------- the renderer: sequential + two concurrent variants ----------
;; sequential: map the ONE zipper down the visible lines.
(define (render h z top rows)
  (with-cursor z (lambda (sl sc el ec)
    (for/list ([L (in-range top (+ top rows))]) (render-line h z sl sc el ec L)))))

;; pmap: each element in its own future (a parallel task over the SHARED persistent
;; zipper), collected in order. Spawn ALL, THEN touch -- touching in the loop serializes.
(define (pmap f xs)
  (define fs (for/list ([x (in-list xs)]) (future (lambda () (f x)))))
  (map touch fs))

;; per-line futures: the navigate map made concurrent, one future per line (as asked).
(define (render/par h z top rows)
  (with-cursor z (lambda (sl sc el ec)
    (pmap (lambda (L) (render-line h z sl sc el ec L)) (range top (+ top rows))))))

;; chunk a list into n contiguous parts (coarser-grained tasks).
(define (chunk xs n)
  (define size (max 1 (ceiling (/ (length xs) n))))
  (let loop ([xs xs] [acc '()])
    (cond [(null? xs) (reverse acc)]
          [else (define k (min size (length xs)))
                (loop (drop xs k) (cons (take xs k) acc))])))

;; chunked futures: one future per chunk (default processor-count), each chunk rendered
;; sequentially inside -- fewer/larger tasks, less spawn + scheduler overhead than per-line.
(define (render/par-chunked h z top rows [chunks (processor-count)])
  (with-cursor z (lambda (sl sc el ec)
    (append*
     (pmap (lambda (part) (map (lambda (L) (render-line h z sl sc el ec L)) part))
           (chunk (range top (+ top rows)) chunks))))))

;; ============================================================================
(module+ main
  (define h (make-hl '("define" "lambda" "let" "if" "cond")))
  (define doc (string-append
               "(define (f x)\n"
               "  (if (> x 0)\n"
               "      (list \"(define)\" x)\n"
               "      (g define)))"))

  ;; the single editing cursor: a selection from (line 1, col 2) to (line 2, col 22),
  ;; crossing the string -- ONE ⟦…⟧ pair spans lines 1-2.
  (define z ((setter zipper-guide (vector (col 1 2) (col 2 22))) (open-doc h doc)))

  (printf "keywords: ~s   cursor: (1,2)-(2,22)\n" (hl-keywords h))
  (printf "markers:  «kw»  ‹str›  paren·depth  ⟦cursor⟧\n\n")
  (printf "~a ~a ~a  ~a\n" (~a "ln" #:min-width 3) (~a "in?" #:min-width 5) (~a "depth" #:min-width 6) "rendered")
  (for ([row (in-list (render h z 0 4))])
    (match-define (list L in? depth text) row)
    (printf "~a ~a ~a  ~a\n"
            (~a L #:min-width 3) (~a in? #:min-width 5) (~a depth #:min-width 6) text))

  (printf "\nthe three `define`s, each read in its own context off `before`:\n")
  (printf "  line 0  define  -> operator (head slot open)       -> «define»  highlighted\n")
  (printf "  line 2  define  -> inside a string                 -> ‹\"(define)\"›  inert (no «», its ( ) get no depth)\n")
  (printf "  line 3  define  -> argument (head slot taken by g) -> plain\n"))

;; ============================================================================
;; concurrency benchmark: sequential vs per-line futures vs chunked futures.
(module+ main
  (printf "\n==== concurrency benchmark ====\n")
  (printf "cores: ~a   futures-enabled?: ~a\n" (processor-count) (futures-enabled?))
  (define hb (make-hl '("define" "lambda" "let" "if" "cond")))
  (define snippet (string-append
                   "(define (fib n)\n" "  (if (< n 2)\n" "      n\n"
                   "      (+ (fib (- n 1))\n" "         (fib (- n 2)))))\n"))
  (define copies 800)
  (define total-lines (* copies 5))
  (define zb   (open-doc hb (apply string-append (make-list copies snippet))))
  (define rows 1000)
  (define iters 6)
  (printf "doc: ~a lines   viewport: ~a rows   iters: ~a\n\n" total-lines rows iters)

  ;; all three variants must agree
  (define ref (render hb zb 0 60))
  (unless (and (equal? ref (render/par hb zb 0 60))
               (equal? ref (render/par-chunked hb zb 0 60)))
    (error "render variants disagree"))
  (printf "correctness: sequential == per-line == chunked  ok\n\n")

  (define (timed label thunk)
    (thunk)                                  ; warm up
    (collect-garbage)
    (define-values (_ cpu real gc)
      (time-apply (lambda () (for ([i (in-range iters)]) (thunk))) '()))
    (printf "~a  cpu ~a ms   real ~a ms   gc ~a ms\n"
            (~a label #:min-width 20) (~a cpu #:min-width 6) (~a real #:min-width 6) (~a gc #:min-width 5))
    real)

  (define rseq (timed "sequential"       (lambda () (render             hb zb 0 rows))))
  (define rpar (timed "per-line futures" (lambda () (render/par         hb zb 0 rows))))
  (define rchk (timed "chunked futures"  (lambda () (render/par-chunked hb zb 0 rows))))
  (printf "\nspeedup (real, vs sequential):  per-line ~ax   chunked ~ax\n"
          (~r (/ rseq (max 1 rpar)) #:precision 2) (~r (/ rseq (max 1 rchk)) #:precision 2))

  ;; --- why: trace one parallel run, count GC events + per-future park (block) events.
  ;; Localize the block by tracing the full render vs navigation only vs analyze only. ---
  (local-require future-visualizer/trace)
  (define (trace-par f xs)              ; run (pmap f xs) under tracing -> (values blocks gcs)
    (start-future-tracing!)
    (void (pmap f xs))
    (stop-future-tracing!)
    (define raw (map indexed-future-event-fevent (timeline-events)))
    (values (count (lambda (e) (and (future-event? e) (eq? (future-event-what e) 'block))) raw)
            (count (lambda (e) (not (future-event? e))) raw)))
  (define Ls (range 0 rows))
  ;; precompute heads sequentially so the analyze-only probe does no navigation
  (define heads (for/list ([L (in-list Ls)]) (call-with-values (lambda () (line-head zb L)) list)))
  (define-values (bf gf) (trace-par (lambda (L) (render-line hb zb 0 0 0 0 L)) Ls))           ; full
  (define-values (bn gn) (trace-par (lambda (L) (call-with-values (lambda () (line-head zb L)) list)) Ls))  ; nav only
  (define-values (ba ga) (trace-par (lambda (bma) (call-with-values
                                                   (lambda () (apply analyze-head (hl-keywords hb) (hl-kw-smr hb) bma)) list))
                                    heads)) ; analyze only
  (printf "\nlocalizing the block (~a futures each):\n" rows)
  (printf "  ~a  blocks ~a   gc ~a\n" (~a "full render-line" #:min-width 18) (~a bf #:min-width 5) gf)
  (printf "  ~a  blocks ~a   gc ~a\n" (~a "navigation only"  #:min-width 18) (~a bn #:min-width 5) gn)
  (printf "  ~a  blocks ~a   gc ~a\n" (~a "analyze only"     #:min-width 18) (~a ba #:min-width 5) ga))
