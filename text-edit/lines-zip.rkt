#lang racket

;; Lines zip: a zipper over the document's lines with a scrolling window. The
;; document lives in a summarised-rope zipper; a SLACK buffer [N,M) holds the
;; visible window [n,m) plus overscan s on each side, so scrolling within the slack
;; is index math (no navigation) and only crossing the buffer edge refills. The
;; scroll path stays on the linecol metric -- carrying the sexp/lisp summary through
;; navigation is the expensive part, so the syntax layer (lisp-view) colours the
;; visible lines it hands back, rather than riding every navigation join.
;;   open-lines-zip   z n0 h s -> a lines-zip: window of h lines at row n0, slack s
;;   scroll           lz dr    -> shift the window dr rows; refill only at the edge
;;   lines-zip-window lz       -> the visible line ropes
;;   lines-zip-row / lines-zip-height
;; The frame guide `row-at` is a linecol row-start cut CLAMPED to the document end
;; (a row past the last line snaps to the end rather than erroring "end follows the
;; focus" -- the clamp the frame-end guide always needed).

(require "../rope-core.rkt"
         (submod "../rope-core.rkt" experimental)             ; multisect*
         "../zipper-core.rkt"                                 ; start, zipper-guide/-focus, to-root, opt ops
         "../summaries/summaries.rkt"                         ; linecol-smr, linecol, linecol-lines
         (submod "../summaries/summaries.rkt" experimental))  ; newline-guide*

(provide open-lines-zip scroll lines-zip-window lines-zip-row lines-zip-height)

;; ---------- the frame guide ----------
;; the cut at row r's start, clamped to the document end.
(define ((row-at r) L R)
  (match-define (linecol _ l k) (linecol-smr L))
  (match-define (linecol h2 l2 c2) (linecol-smr R))
  (cond [(> l r) -1]
        [(and (= l r) (> k 0)) -1]
        [(= l r) 0]
        [(and (= h2 0) (= l2 0) (= c2 0)) 0]                 ; l < r but the doc ends here
        [else 1]))

;; navigate z to rows [a, b) -> (values z* line-ropes); empty range -> no lines.
(define (fetch z a b)
  (if (>= a b) (values z '())
      (let ([z* (((opt-set zipper-guide) (list (row-at a) (row-at b))) z)])
        (values z* ((multisect* newline-guide*) ((opt-get zipper-focus) z*))))))

(define (take-range lst a b)                                 ; [a, b) of lst, indices clamped
  (let* ([len (length lst)] [a* (max 0 (min a len))] [b* (max a* (min b len))])
    (take (drop lst a*) (- b* a*))))

;; ---------- the lines-zip ----------
(struct lines-zip (z buf N M n m s total) #:transparent)
;  z     : the document zipper (focus = the buffer band)
;  buf   : the buffer's line ropes = document lines [N, M)
;  N M   : the buffer's line range;  n m : the visible window, N <= n, m <= M
;  s     : slack (overscan rows per side);  total : document line count

;; open a window of h lines at row n0, with slack s
(define (open-lines-zip z n0 h s)
  (define total (add1 (linecol-lines (linecol-smr ((opt-get zipper-focus) (to-root z))))))
  (define N (max 0 (- n0 s)))
  (define M (min total (+ n0 h s)))
  (define-values (z* buf) (fetch z N M))
  (lines-zip z* buf N M n0 (+ n0 h) s total))

;; scroll by dr rows (down +, up -): shift the window; refill only at the buffer edge
(define (scroll lz dr)
  (match-define (lines-zip z buf N M n m s total) lz)
  (define h (- m n))
  (define n* (max 0 (min (- total h) (+ n dr))))             ; window clamped to the doc
  (define m* (+ n* h))
  (if (and (>= n* N) (<= m* M))
      (lines-zip z buf N M n* m* s total)                    ; inside the slack: O(1), no navigation
      (let ([N* (max 0 (- n* s))] [M* (min total (+ m* s))])
        (let-values ([(z* buf*) (fetch z N* M*)])            ; refill: re-navigate, re-center
          (lines-zip z* buf* N* M* n* m* s total)))))

(define (lines-zip-window lz)
  (take-range (lines-zip-buf lz) (- (lines-zip-n lz) (lines-zip-N lz)) (- (lines-zip-m lz) (lines-zip-N lz))))
(define (lines-zip-row    lz) (lines-zip-n lz))
(define (lines-zip-height lz) (- (lines-zip-m lz) (lines-zip-n lz)))

;; ============================================================================
(module+ test
  (require rackunit racket/format)
  (define doc ((make-rope linecol-smr)
               (string-join (for/list ([i 20]) (format "(line ~a)" i)) "\n")))
  (define (rows lz) (map (lambda (p) (string-trim (~a p) "\n" #:left? #f)) (lines-zip-window lz)))
  (define z (start linecol-smr doc (row-at 0) (row-at 0)))

  ;; open at the top: window [0,4), slack fills below only
  (define v0 (open-lines-zip z 0 4 3))
  (check-equal? (rows v0) '("(line 0)" "(line 1)" "(line 2)" "(line 3)"))
  (check-equal? (lines-zip-row v0) 0)
  (check-equal? (lines-zip-height v0) 4)

  ;; scroll down within the slack: no refill (buffer already holds these)
  (define v1 (scroll v0 1))
  (check-equal? (rows v1) '("(line 1)" "(line 2)" "(line 3)" "(line 4)"))
  (check-equal? (lines-zip-row v1) 1)

  ;; a jump past the slack: refills, still correct
  (define v2 (scroll v1 9))
  (check-equal? (rows v2) '("(line 10)" "(line 11)" "(line 12)" "(line 13)"))

  ;; scroll up back across the buffer edge
  (define v3 (scroll v2 -1))
  (check-equal? (rows v3) '("(line 9)" "(line 10)" "(line 11)" "(line 12)"))

  ;; clamp at the top
  (check-equal? (lines-zip-row (scroll v0 -5)) 0)

  ;; clamp at the bottom (window can't pass the last line)
  (define vb (scroll (open-lines-zip z 0 4 3) 100))
  (check-equal? (rows vb) '("(line 16)" "(line 17)" "(line 18)" "(line 19)"))
  (check-equal? (lines-zip-row vb) 16))
