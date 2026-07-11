#lang racket

;; Lines zip: a zipper over the document's lines with a scrolling window. The
;; document lives in a summarised-rope zipper; a SLACK buffer holds the visible
;; window plus overscan on each side, so scrolling within the slack is index math
;; (no navigation) and only crossing the buffer edge refills. The scroll path stays
;; on the linecol metric -- carrying the sexp/lisp summary through navigation is
;; the expensive part, so the syntax layer (lisp-view) colours the visible lines it
;; hands back, rather than riding every navigation join. Both bands are row SPANS,
;; (lo . n) pairs; the slack is per-call policy (default: the window height), not
;; state -- open-lines-zip is the one place buffers are placed, scroll's refill
;; reopens through it.
;;   open-lines-zip   z n0 h [s] -> a lines-zip: window of h lines at row n0, the
;;                    buffer padded s rows each side (default: h)
;;   scroll           lz dr [s]  -> shift the window dr rows; only a refill (past
;;                    the buffer edge) reopens, padded s (default: the height)
;;   lines-zip-window lz         -> the visible line ropes
;;   lines-zip-row / lines-zip-height
;;   guide-row        lz g       -> the row containing the cut g names, measured
;;                    by sending the carried zipper to the gap focus (g g)
;;   focus-line-at    lz g k     -> reopen with g's line pinned at window offset k
;;   focus-top / focus-bottom    -> k = 0 / k = height-1
;;   focus-frame      lz g0 g1   -> the window sized to the region's lines: the
;;                    retraction after a navigation to (g0 g1)
;;   lines-spl        the zipper <-> lines-zip SPLITTING, a canonical VALUE (the
;;                    section rebuilds row guides at the window; the retraction
;;                    expands the focus to whole lines and forgets the guides --
;;                    e = the expansion)
;;   lines-zip=?      the spl law's equality: window + frame, the buffer is cache
;; The frame guide `row-at` is a linecol row-start cut CLAMPED to the document end
;; (a row past the last line snaps to the end rather than erroring "end follows the
;; focus" -- the clamp the frame-end guide always needed).

(require racket/match
         "../rope-core.rkt"
         (submod "../rope-core.rkt" experimental)             ; multisect*
         "../zipper-core.rkt"                                 ; start, zipper-guide/-focus, edge-view, to-root, opt ops
         "../summaries/summaries.rkt"                         ; linecol-smr, linecol, linecol-lines
         (submod "../summaries/summaries.rkt" experimental)   ; newline-guide*
         "../toolbox/main.rkt")                               ; spl

(provide open-lines-zip scroll lines-zip-window lines-zip-row lines-zip-height
         guide-row focus-line-at focus-top focus-bottom focus-frame
         lines-spl lines-zip->zipper zipper->lines-zip lines-zip=?)

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

;; ---------- row spans: (lo . n) ----------
(define (span-lo s) (car s))
(define (span-n  s) (cdr s))
(define (span-hi s) (+ (car s) (cdr s)))
(define (span-within? a b)                                   ; a inside b
  (and (>= (span-lo a) (span-lo b)) (<= (span-hi a) (span-hi b))))

;; ---------- the lines-zip ----------
(struct lines-zip (zip lines buf win rows) #:transparent)
;  zip   : the document zipper (focus = the buffer band)
;  lines : the buffered line ropes = document rows in buf
;  buf   : (lo . n) the buffer band;  win : (lo . n) the visible window, within buf
;  rows  : total document rows

;; open a window of h lines at row n0; the buffer pads s rows each side
(define (open-lines-zip z n0 h [s h])
  (define rows (add1 (linecol-lines (linecol-smr ((opt-get zipper-focus) (to-root z))))))
  (define buf-lo (max 0 (- n0 s)))
  (define buf-hi (min rows (+ n0 h s)))
  (define-values (z* lines) (fetch z buf-lo buf-hi))
  (lines-zip z* lines (cons buf-lo (- buf-hi buf-lo)) (cons n0 h) rows))

;; scroll by dr rows (down +, up -): shift the window; refill only at the buffer edge
(define (scroll lz dr [s (lines-zip-height lz)])
  (match-define (lines-zip zip _ buf (cons lo n) rows) lz)
  (define lo* (max 0 (min (- rows n) (+ lo dr))))            ; window clamped to the doc
  (if (span-within? (cons lo* n) buf)
      (struct-copy lines-zip lz [win (cons lo* n)])          ; inside the slack: O(1), no navigation
      (open-lines-zip zip lo* n s)))                         ; past the edge: reopen there, padded s

(define (lines-zip-window lz)
  (define off (- (span-lo (lines-zip-win lz)) (span-lo (lines-zip-buf lz))))
  (take-range (lines-zip-lines lz) off (+ off (span-n (lines-zip-win lz)))))
(define (lines-zip-row    lz) (span-lo (lines-zip-win lz)))
(define (lines-zip-height lz) (span-n  (lines-zip-win lz)))

;; ---------- navigation by guide ----------
;; A guide is judged absolutely (framed against the whole document), so ANY guide
;; the document's algebra can judge names a place to go; relative specifications
;; ("5 past g") are resolved by measuring first, then naming rows absolutely.

;; the document row containing the cut g names: send the carried zipper to the
;; gap focus (g g), read the row off the before flank -- (values row z*), the
;; navigated zipper returned so a caller reopens from nearby, not from cold
(define (guide-row lz g)
  (define z* (((opt-set zipper-guide) (list g g)) (lines-zip-zip lz)))
  (define-values (bs _r) ((edge-view 0) z*))
  (values (linecol-lines (linecol-smr bs)) z*))

;; reopen the viewport with g's line pinned at window offset k (0 = the top row,
;; height-1 = the bottom); the document-edge clamps win over the pin, as in scroll
(define (focus-line-at lz g k)
  (define n (lines-zip-height lz))
  (define-values (r z*) (guide-row lz g))
  (open-lines-zip z* (max 0 (min (- (lines-zip-rows lz) n) (- r k))) n))

(define (focus-top    lz g) (focus-line-at lz g 0))
(define (focus-bottom lz g) (focus-line-at lz g (sub1 (lines-zip-height lz))))

;; navigate to the region [g0, g1): its first line at the top, the window sized to
;; every line the region touches -- the retraction does the rounding-out
(define (focus-frame lz g0 g1)
  (zipper->lines-zip (((opt-set zipper-guide) (list g0 g1)) (lines-zip-zip lz))))

;; ---------- the spl: zipper <-> lines-zip ----------
;; The lines-zip is a RETRACT of the zipper. The section loses nothing -- its image
;; is the line-guided zippers; the retraction forgets the guide identity, and only
;; that. e = to . from is the EXPANSION: the focus rounds outward to whole lines
;; (a mid-line edge to its line's edges, newline included; a point cursor to its
;; containing line); idempotent. The retraction reopens with the DEFAULT slack (the
;; height), so the pair is one canonical value. The spl law holds on the QUOTIENT
;; lines-zip=? reads -- same window, same frame; the slack buffer is cache, and a
;; round trip may recenter it.

;; the section: re-guide the carried zipper to the WINDOW's rows
(define (lines-zip->zipper lz)
  (define n (lines-zip-row lz))
  (((opt-set zipper-guide) (list (row-at n) (row-at (+ n (lines-zip-height lz)))))
   (lines-zip-zip lz)))

;; the retraction: the cursor's row extent off the edge summaries, expanded
;; outward to whole lines, opened as a viewport -- the guides forgotten here
(define (zipper->lines-zip z)
  (define-values (bs _r0) ((edge-view 0) z))              ; before the focus
  (define-values (bf _r1) ((edge-view 1) z))              ; before + focus
  (define n (linecol-lines (linecol-smr bs)))             ; row of the focus start
  (match-define (linecol _ le ce) (linecol-smr bf))       ; row/col of the focus end
  (define m (max (add1 n) (if (> ce 0) (add1 le) le)))    ; round the end out
  (open-lines-zip z n (- m n)))

(define lines-spl (spl lines-zip->zipper zipper->lines-zip))

;; observational equality -- the buffer-placement quotient
(define (lines-zip=? a b)
  (and (= (lines-zip-row a) (lines-zip-row b))
       (= (lines-zip-height a) (lines-zip-height b))
       (equal? (map ~a (lines-zip-window a)) (map ~a (lines-zip-window b)))))

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

  ;; the default slack is the height; a custom one pads as told
  (check-equal? (lines-zip-buf (open-lines-zip z 5 2))   '(3 . 6))
  (check-equal? (lines-zip-buf (open-lines-zip z 5 2 4)) '(1 . 10))

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

  ;; a refill takes the per-call slack; within-slack scrolling never reads it
  (check-equal? (lines-zip-buf (scroll (open-lines-zip z 0 4) 10 1)) '(9 . 6))

  ;; clamp at the top
  (check-equal? (lines-zip-row (scroll v0 -5)) 0)

  ;; clamp at the bottom (window can't pass the last line)
  (define vb (scroll (open-lines-zip z 0 4 3) 100))
  (check-equal? (rows vb) '("(line 16)" "(line 17)" "(line 18)" "(line 19)"))
  (check-equal? (lines-zip-row vb) 16)

  ;; --- navigation by guide ---
  (define ((at-rowcol r c) L R)                    ; a cut at (row, col) -- NOT a line guide
    (match-define (linecol _ l k) (linecol-smr L))
    (cond [(or (> l r) (and (= l r) (> k c))) -1]
          [(and (= l r) (= k c)) 0]
          [else 1]))

  ;; guide-row: the row of the cut, a mid-line cut reading its containing line
  (let-values ([(r _z) (guide-row v0 (at-rowcol 7 3))])
    (check-equal? r 7))
  (let-values ([(r _z) (guide-row v0 (at-rowcol 0 0))])
    (check-equal? r 0))

  (define lz (open-lines-zip z 5 4 3))

  ;; focus-top / focus-bottom / an interior pin: same height, the line at k
  (define ft (focus-top lz (at-rowcol 7 3)))
  (check-equal? (lines-zip-row ft) 7)
  (check-equal? (lines-zip-height ft) 4)
  (check-equal? (car (rows ft)) "(line 7)")
  (define fb (focus-bottom lz (at-rowcol 7 3)))
  (check-equal? (lines-zip-row fb) 4)
  (check-equal? (last (rows fb)) "(line 7)")
  (check-equal? (lines-zip-row (focus-line-at lz (at-rowcol 7 3) 2)) 5)

  ;; the document-edge clamps win over the pin, as in scroll
  (check-equal? (lines-zip-row (focus-bottom lz (at-rowcol 1 0))) 0)
  (check-equal? (lines-zip-row (focus-top    lz (at-rowcol 19 0))) 16)

  ;; focus-frame: the window sized to the region's lines, mid-line ends rounded out
  (define ff (focus-frame lz (at-rowcol 7 2) (at-rowcol 9 4)))
  (check-equal? (lines-zip-row ff) 7)
  (check-equal? (lines-zip-height ff) 3)
  (check-equal? (rows ff) '("(line 7)" "(line 8)" "(line 9)"))
  (check-equal? (rows (focus-frame lz (at-rowcol 7 2) (at-rowcol 7 2)))
                '("(line 7)"))                     ; a point covers its line

  ;; --- the spl ---
  (define lr lines-spl)
  (define (focus-text z) (~a ((opt-get zipper-focus) z)))

  ;; the spl law: from . to = id, on the lines-zip=? quotient -- including after a
  ;; within-slack scroll, where the round trip recenters only the buffer (cache)
  (check-true (lines-zip=? ((spl-from lr) ((spl-to lr) lz)) lz))
  (define lz* (scroll lz 2))
  (check-true (lines-zip=? ((spl-from lr) ((spl-to lr) lz*)) lz*))
  (check-false (= (span-lo (lines-zip-buf ((spl-from lr) ((spl-to lr) lz*))))
                  (span-lo (lines-zip-buf lz*))))             ; the cache DID move

  ;; the section lands on clean line cuts
  (check-equal? (focus-text ((spl-to lr) lz)) "(line 5)\n(line 6)\n(line 7)\n(line 8)\n")

  ;; e = to . from: the expansion -- outward to whole lines, idempotent; a point
  ;; cursor covers its containing line
  (define e (spl-normalize lr))
  (define z-mid (((opt-set zipper-guide) (list (at-rowcol 7 2) (at-rowcol 8 4))) z))
  (check-equal? (focus-text z-mid) "ine 7)\n(lin")
  (check-equal? (focus-text (e z-mid)) "(line 7)\n(line 8)\n")
  (check-equal? (focus-text (e (e z-mid))) (focus-text (e z-mid)))
  (check-equal? (focus-text (e (((opt-set zipper-guide) (list (at-rowcol 7 2) (at-rowcol 7 2))) z)))
                "(line 7)\n")

  ;; worn as an opt: world = zipper, focus = lines-zip -- scrolling as an EDIT,
  ;; PutGet exact, a bare run applies e
  (define ro (spl->opt lr))
  (check-equal? (rows ((opt-get ro) z-mid)) '("(line 7)" "(line 8)"))
  (check-equal? (focus-text ((opt-update ro (lambda (v) (scroll v 5))) z-mid))
                "(line 12)\n(line 13)\n")
  (check-true (lines-zip=? ((opt-get ro) (((opt-set ro) lz) z-mid)) lz))
  (check-equal? (focus-text (ro z-mid)) "(line 7)\n(line 8)\n"))
