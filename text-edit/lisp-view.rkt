#lang racket

;; Lisp view: the document as a display matrix, read off the zipper. The tower:
;;   view-into    ((view-into z) r0 r1 c0 c1) -> (view i j) -- the zero-config
;;                reader, z curried first. The algebra is DERIVED from the zipper's
;;                own values (any bundle carrying lisp-smr and linecol-smr; checked,
;;                a missing slot errors by name), so it always operates in the
;;                document's own algebra. A matrix read is two index ops: (values
;;                char class) inside the window, (values #f #f) past a row's text
;;                and outside the frame alike; i j view-local, 0-indexed at the
;;                window's top-left corner.
;;   open-view    the window opt with the display attached (attach-viewer of the
;;                matrix viewer onto view-runs): get z -> (values rows types view),
;;                put view-runs' own -- so a transform can consult the display
;;                while editing the data.
;;   view-runs    the addressable window as ONE opt on the zipper -- move into place
;;                (fused into get AND put, so a write re-finds its band by its row
;;                coordinates), split the band into lines, crop each line to the
;;                column window, split + label each window by lisp-smr's runs:
;;                view (values rows types), parallel lists of lists.
;;   linecol-at   the cut at (row, col) -- a guide off the linecol metric.
;;   leftmost-guide rightmost-guide
;;                guide combinators: {-1,0,1} outputs are Kleene logic (1 = the cut
;;                is right of here; and = min, or = max), so these name the leftmost
;;                / rightmost cut of a family. Min/max of antitone guides is antitone,
;;                so a combination is again a lawful guide.
;; Rows dominate columns in the (row, col) order, so the hull of per-line guides
;; frames WHOLE lines and all column work is one local crop per line. Every stage
;; threads the flanking summaries (scanl/scanr), so labels are context-true even
;; mid-string / mid-comment at a window edge. Read-only for now: view-runs is a
;; full opt (its put rejoins and re-navigates), but the matrix exposes only the get.

(require racket/match
         racket/format
         "lisp-edit.rkt"                                  ; indexed zipper-focus/zipper-guide,
                                                          ;   split-runs, label-runs, multisect*,
                                                          ;   lisp-smr, rope-core re-exports
         "../summaries/summaries.rkt"                        ; bundle, linecol-smr, linecol
         (submod "../summaries/summaries.rkt" experimental)  ; newline-guide*
         "../toolbox/main.rkt")                           ; opt, opt-list, pure, on, pass, arg,
                                                          ;   scanl, scanr, memoize

(provide view-smr
         linecol-at leftmost-guide rightmost-guide
         view-runs open-view view-into)

;; the default view algebra: the lisp runs + the (row, col) metric + the content
;; fingerprint, the whole bundle WORN with an LRU memo keyed by the fingerprint --
;; a join of previously-seen content is a table hit, so re-reads over unchanged
;; text (flank scans, scrolls, re-navigations) stop paying the lisp joins; the
;; chain re-hits (a hit returns the identical cached value, so the next join's key
;; is identical too). The key fn is the component itself: applying hash-smr to a
;; bundle value / rope / string IS the O(1) projection to its fp.
(define view-smr
  (memoize (bundle lisp-smr linecol-smr hash-smr)
           #:key (on list hash-smr)))

;; ---------- guides ----------
;; the cut at (r, c), 0-indexed, read lexicographically off the all-left value. A
;; position past the row's text or the document never reads 0, so the guide snaps
;; at its sign flip -- the clamp is free.
(define ((linecol-at r c) L R)
  (match-define (linecol _ l k) (linecol-smr L))
  (cond [(< l r) 1] [(> l r) -1]
        [(< k c) 1] [(> k c) -1] [else 0]))

;; ---------- guide combinators ----------
(define ((leftmost-guide  . gs) L R) (apply (on min (pass L R)) gs))   ; Kleene and
(define ((rightmost-guide . gs) L R) (apply (on max (pass L R)) gs))   ; Kleene or

;; ---------- the window opt ----------
(define (view-runs smr r0 r1 c0 c1)
  (define build (make-rope smr))

  ;; -- guides local to the window --
  (define (line-start-at r) (linecol-at r 0))
  ;; end of row r's TEXT, before its newline -- two-sided: the newline is seen from
  ;; the right ((linecol-head R) = 0 means \n is next, or the document ends)
  (define ((line-end-at r) L R)
    (match-define (linecol _ l _) (linecol-smr L))
    (cond [(< l r) 1] [(> l r) -1]
          [(zero? (linecol-head (linecol-smr R))) 0]
          [else 1]))
  ;; the column window's cuts, LINE-LOCAL (a line piece's text is its row 0; the
  ;; row check is the fence at its trailing newline), clamped to the text end
  (define (cut c) (leftmost-guide (linecol-at 0 c) (line-end-at 0)))
  (define-values (s e) (values (cut c0) (cut c1)))

  ;; -- move into place: the hull of the per-line guide families --
  (define (goto-frame starts ends)
    (opt-update zipper-guide
                (pure (list (apply leftmost-guide  starts)
                            (apply rightmost-guide ends)))))
  (define into-band
    (goto-frame (for/list ([r (in-range r0 (add1 r1))]) (line-start-at r))
                (for/list ([r (in-range r0 (add1 r1))]) (line-end-at r))))

  ;; navigation fused with the indexed focus: the get moves then reads, the put
  ;; moves then writes -- a write re-finds the band on the current document
  (define in-place
    (opt-from-peek
     (lambda (z)
       (define zb (into-band z))                   ; ONE navigation serves read and write
       (call-with-values (lambda () ((opt-get zipper-focus) zb))
         (lambda (fr bs as)
           (values (lambda (new . _) (((opt-set zipper-focus) new) zb))
                   fr bs as))))))

  ;; -- the cutting --
  ;; band -> lines, each with flanking summaries: the flanks are the two scans
  (define split-lines
    (opt-from-peek
     (lambda (fr bs as)
       (define frs ((multisect* newline-guide*) fr))
       (values (lambda (frs* . _) (apply build frs*))
               frs
               (drop-right (scanl smr bs frs) 1)
               (cdr       (scanr smr as frs))))))
  ;; crop a line to [c0, c1): guides judged locally, trimmings fuse into the flanks
  (define crop
    (opt-from-peek
     (lambda (fr bs as)
       (define-values (l m r) ((multisect smr s e) fr))
       (values (lambda (m* . _) (build l m* r))
               m (smr bs l) (smr r as)))))

  (compose-opt in-place                            ; z -> (fr bs as)
               split-lines                         ;   -> (lines bss ass)
               (opt-list                           ;   per line:
                (compose-opt crop                  ;     the visible window
                             (split-runs smr)      ;     its runs, context-true
                             label-runs))))        ;     with their classes

;; ---------- the display ----------
;; matrix: the display VIEWER -- a pure render of the window opt's view. Compiles
;; rows+types once into a table; the result reads a cell in two bounds checks and
;; two index ops.
(define (matrix rows types)
  (define table                                    ; row -> text + per-char classes
    (for/vector ([runs (in-list rows)] [cls (in-list types)])
      (cons (apply string-append (map ~a runs))
            (for*/vector ([(p c) (in-parallel runs cls)]
                          [_ (in-string (~a p))])
              c))))
  (lambda (i j)
    (cond [(not (< -1 i (vector-length table))) (values #f #f)]
          [else (match-define (cons str cs) (vector-ref table i))
                (if (< -1 j (string-length str))
                    (values (string-ref str j) (vector-ref cs j))
                    (values #f #f))])))

;; open-view: the window opt with the display attached -- the render at the
;; identity stage, where its world IS view-runs' view (rows types), so `matrix`
;; reads what it always read. The widened read is opt-get*:
;;   get* : z -> (values rows types (view i j))    put : view-runs' own
(define (open-view smr r0 r1 c0 c1)
  (compose-opt (view-runs smr r0 r1 c0 c1)
               (attach-viewer (compose-opt) matrix)))

;; view-into: the zero-config reader, z curried first. The operating algebra is
;; read off the zipper's own values -- the focus rope's algebra field, NOT the
;; flank's bundle-val-owner: the owner thunk closes over the raw bundle, so it
;; cannot answer with an algebra worn from OUTSIDE (view-smr's memo chaperone);
;; the rope field carries whatever the document was built with, worn included.
;; Checked to bundle the two components the pipeline reads; so reads AND any
;; future writes run in the document's own algebra, never a private stand-in.
(define ((view-into z) r0 r1 c0 c1)
  (define-values (fr bs _as) ((opt-get zipper-focus) z))
  (unless (bundle-val? bs)
    (error 'view-into "the zipper's summary must bundle lisp-smr and linecol-smr, got ~v" bs))
  (for ([c (list lisp-smr linecol-smr)] [n '(lisp-smr linecol-smr)])
    (unless (hash-has-key? (bundle-val-slots bs) c)
      (error 'view-into "the zipper's bundle lacks ~a: ~v" n bs)))
  (define smr (rope-algebra fr))
  ((compose matrix (opt-get (view-runs smr r0 r1 c0 c1))) z))   ; one navigation: get, render

;; ============================================================================
(module+ test
  (require rackunit)
  (define (vals f . args) (call-with-values (lambda () (apply f args)) list))

  ;; a string literal spanning a newline, a comment, four lines:
  ;;   row 0: (a "x      row 2: (c ; t
  ;;   row 1: y" b)     row 3:  d)
  (define doc ((make-rope view-smr) "(a \"x\ny\" b)\n(c ; t\n d)"))
  (define z (start view-smr doc (linecol-at 0 0) (linecol-at 0 0)))

  ;; --- linecol-at: the (row, col) guide ---
  (let-values ([(L R) ((multisect view-smr (linecol-at 1 2)) doc)])
    (check-equal? (~a L) "(a \"x\ny\""))
  (let-values ([(L R) ((multisect view-smr (linecol-at 9 0)) doc)])   ; past the document
    (check-equal? (~a R) ""))                                          ; snaps to its end

  ;; --- the combinators: leftmost / rightmost of an unordered family ---
  (let ([g1 (linecol-at 1 2)] [g2 (linecol-at 0 1)])
    (let-values ([(L R) ((multisect view-smr (leftmost-guide g1 g2)) doc)])
      (check-equal? (~a L) "("))
    (let-values ([(L R) ((multisect view-smr (rightmost-guide g1 g2)) doc)])
      (check-equal? (~a L) "(a \"x\ny\"")))

  ;; --- view-runs, full width: rows of run pieces + parallel classes ---
  (define-values (rows types) ((opt-get (view-runs view-smr 0 3 0 99)) z))
  (check-equal? (for/list ([r (in-list rows)]) (map ~a r))
                '(("(a " "\"x") ("y\"" " b)") ("(c " "; t") (" d)")))
  (check-equal? types '((code string) (string code) (code comment) (code)))

  ;; --- a band opening MID-STRING: labels stay context-true ---
  (define-values (rows1 types1) ((opt-get (view-runs view-smr 1 1 0 99)) z))
  (check-equal? (map ~a (car rows1)) '("y\"" " b)"))
  (check-equal? types1 '((string code)))

  ;; --- the column window: cropped at [1, 4), clamped, newline never swallowed ---
  (define-values (rows2 types2) ((opt-get (view-runs view-smr 0 0 1 4)) z))
  (check-equal? (map ~a (car rows2)) '("a " "\""))
  (check-equal? types2 '((code string)))

  ;; --- the matrix via view-into: (values char class); #f #f off-window and past-text ---
  (define view ((view-into z) 0 3 0 99))
  (check-equal? (vals view 0 0) '(#\( code))
  (check-equal? (vals view 0 3) '(#\" string))
  (check-equal? (vals view 1 0) '(#\y string))
  (check-equal? (vals view 2 3) '(#\; comment))
  (check-equal? (vals view 3 1) '(#\d code))
  (check-equal? (vals view 0 5) '(#f #f))          ; past row 0's text
  (check-equal? (vals view 9 0) '(#f #f))          ; outside the band

  ;; --- view-local indexing: the window's corner is (0 0) ---
  (define view2 ((view-into z) 2 3 3 99))
  (check-equal? (vals view2 0 0) '(#\; comment))   ; document (2, 3)
  (check-equal? (vals view2 1 0) '(#f #f))         ; row 3 is narrower than the window

  ;; --- open-view: the attached opt -- data AND display in one widened read ---
  (let-values ([(rs ts v) ((opt-get* (open-view view-smr 0 3 0 99)) z)])
    (check-equal? ts types)                        ; the data channels are view-runs'
    (check-equal? (vals v 1 0) '(#\y string)))     ; the render rides last

  ;; --- view-smr: the worn bundle -- observationally the algebra, cache live ---
  (check-true (memo? view-smr))
  (check-equal? (hash-smr (view-smr "(a)")) (hash-smr "(a)"))  ; the fp slot rides along
  (check-equal? (view-smr "(a " "b)") (view-smr "(a b)"))      ; still the same monoid
  (let* ([v  (view-smr "(a ")]
         [w  (view-smr "b)")]
         [n1 (begin (view-smr v w) (memo-size view-smr))])
    (view-smr v w)                                             ; the same join again...
    (check-equal? (memo-size view-smr) n1))                    ; ...is a hit, not an entry

  ;; --- view-into derives the algebra: a RICHER bundle works as itself ---
  (let* ([big  (bundle lisp-smr linecol-smr char-smr)]
         [zb   (start big ((make-rope big) "(a \"x\ny\" b)") (linecol-at 0 0) (linecol-at 0 0))])
    (check-equal? (vals ((view-into zb) 1 1 0 99) 0 0) '(#\y string)))

  ;; --- and checks what it needs: a missing slot errors by name ---
  (let ([zl (start lisp-smr ((make-rope lisp-smr) "(a)") (linecol-at 0 0) (linecol-at 0 0))])
    (check-exn #rx"must bundle" (lambda () ((view-into zl) 0 0 0 9))))
  (let* ([nl (bundle linecol-smr char-smr)]
         [zn (start nl ((make-rope nl) "(a)") (linecol-at 0 0) (linecol-at 0 0))])
    (check-exn #rx"lacks lisp-smr" (lambda () ((view-into zn) 0 0 0 9)))))
