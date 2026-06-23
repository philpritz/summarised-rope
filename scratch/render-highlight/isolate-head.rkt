#lang racket

;; isolate-head: a crumb-free descent that isolates a WINDOW's head -- (before, focus, after)
;; between a start guide and an end guide -- folding the before/after sides into standalone
;; SUMMARIES (no crumbs, no rope rebuild of those sides) while materializing ONLY the focus
;; rope (the window itself).  Contrast multisect, which rebuilds all three sides.
;;
;; Plus `grid`: generate a file of `lines` lines x `cols` columns, to test large windows.
;;
;; Run:  racket scratch/render-highlight/isolate-head.rkt

(require racket/match
         "../../rope-core.rkt"               ; make-rope multisect
         "../../summaries/summaries.rkt"     ; bundle char-smr linecol-smr (struct-out linecol)
         "../../summaries/sexp-summary.rkt"  ; strsexp-smr
         (submod "../../rope-core.rkt" internal))   ; leaf? branch-left branch-right leaf-text

(define buf (bundle char-smr strsexp-smr linecol-smr))
(define mt ((make-rope buf) ""))
(define (join . rs) (apply (make-rope buf) rs))

(define ((col line k) L R)
  (match-define (linecol _ ll lc) (linecol-smr L))
  (cond [(not (= ll line)) (if (< ll line) 1 -1)] [(< lc k) 1] [(> lc k) -1] [else 0]))

;; binary-search the split point in a leaf where guide g (framed by before/after) is satisfied
(define (search g before s after)
  (let bs ([lo 0] [hi (string-length s)])
    (if (>= lo hi) lo
        (let* ([mid (quotient (+ lo hi) 2)]
               [d (g (buf before (substring s 0 mid)) (buf (substring s mid) after))])
          (if (positive? d) (bs (add1 mid) hi) (bs lo mid))))))

;; fold left-of-cut into `before`; materialize right-of-cut as a rope. (left context = before)
(define (mat-right g before t)
  (cond
    [(leaf? t) (define s (leaf-text t)) (define k (search g before s mt))
     (values (buf before (substring s 0 k)) (join (substring s k)))]
    [else (define l (branch-left t)) (define r (branch-right t))
     (if (positive? (g (buf before l) (buf r)))
         (mat-right g (buf before l) r)                 ; cut in r: l -> before
         (let-values ([(bs lf) (mat-right g before l)]) ; cut in l: focus = (l from cut) ++ r
           (values bs (join lf r))))]))

;; materialize left-of-cut as a rope; fold right-of-cut into `after`. (left context threads in)
(define (mat-left g lctx t after)
  (cond
    [(leaf? t) (define s (leaf-text t)) (define k (search g lctx s after))
     (values (join (substring s 0 k)) (buf (substring s k) after))]
    [else (define l (branch-left t)) (define r (branch-right t))
     (if (negative? (g (buf lctx l) (buf r after)))
         (let-values ([(lf as) (mat-left g lctx l (buf r after))])  ; cut in l: r -> after
           (values lf as))
         (let-values ([(rf as) (mat-left g (buf lctx l) r after)])  ; cut in r: focus = l ++ (r to cut)
           (values (join l rf) as)))]))

;; the window head: before-summary, focus-rope, after-summary
(define (isolate-head gs ge rope)
  (let descend ([before (buf "")] [t rope] [after (buf "")])
    (cond
      [(leaf? t)
       (define s (leaf-text t))
       (define k1 (search gs before s after)) (define k2 (search ge before s after))
       (values (buf before (substring s 0 k1)) (join (substring s k1 k2)) (buf (substring s k2) after))]
      [else
       (define l (branch-left t)) (define r (branch-right t))
       (define Ls (buf before l)) (define Rs (buf r after))
       (define ds (gs Ls Rs)) (define de (ge Ls Rs))
       (cond
         [(and (positive? ds) (positive? de)) (descend Ls r after)]   ; window entirely in r
         [(and (negative? ds) (negative? de)) (descend before l Rs)]  ; window entirely in l
         [else                                                        ; window straddles the seam
          (define-values (bs lf) (mat-right gs before l))
          (define-values (rf as) (mat-left  ge (buf before l) r after))
          (values bs (join lf rf) as)])])))

;; ---------- the file generator ----------
(define (grid lines cols)
  (define row (string-append (make-string cols #\x) "\n"))
  (apply string-append (make-list lines row)))

;; ============================================================================
(module+ main
  (require rackunit)

  ;; ---------- correctness: every window of small grids matches multisect's 3 pieces ----------
  (define (check lines cols)
    (define rope ((make-rope buf) (grid lines cols)))
    (for* ([top (in-range 0 lines)] [h (in-range 0 (add1 (- lines top)))])
      (define gs (col top 0)) (define ge (col (+ top h) 0))
      (define-values (pre foc post) ((multisect buf (vector gs ge)) rope))
      (define-values (bs fr as)     (isolate-head gs ge rope))
      (check-equal? (~a fr) (~a foc) (format "focus ~ax~a top=~a h=~a" lines cols top h))
      (check-equal? (char-smr bs) (char-smr pre) (format "bs ~a/~a" top h))
      (check-equal? (char-smr as) (char-smr post) (format "as ~a/~a" top h))
      (check-equal? (strsexp-smr bs) (strsexp-smr pre) "bs sx")))
  (check 8 3) (check 6 10) (check 10 1)
  (printf "correctness: isolate-head == multisect (before/focus/after) over all windows  ok\n\n")

  ;; ---------- large windows: cost vs window height (before/after stay cheap) ----------
  (define LINES 200000) (define COLS 40)
  (define rope ((make-rope buf) (grid LINES COLS)))
  (printf "file: ~a lines x ~a cols (~a chars), rope height ~a\n\n"
          LINES COLS (* LINES (add1 COLS)) (let-values ([(_ a) (values 0 0)]) "n/a"))
  (define (ns label iters thunk)
    (thunk) (collect-garbage)
    (define-values (_ c r gc) (time-apply (lambda () (for ([i (in-range iters)]) (thunk))) '()))
    (printf "  ~a ~a ms/window\n" (~a label #:min-width 34) (~a (~r (/ (exact->inexact r) iters) #:precision 3) #:min-width 8 #:align 'right)))
  (define top 100000)
  (for ([h (in-list '(50 500 5000 50000))])
    (define gs (col top 0)) (define ge (col (+ top h) 0))
    (printf "window of ~a lines at line ~a:\n" h top)
    (ns "isolate-head (fold bs/as, mat focus)" 200 (lambda () (isolate-head gs ge rope)))
    (ns "multisect (rebuild all 3)"            200 (lambda () ((multisect buf (vector gs ge)) rope)))))
