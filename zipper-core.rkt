#lang racket

(require racket/match
         "rope-core.rkt")

(provide
 (struct-out zipper)
 (struct-out gap)
 (struct-out seg)
 (struct-out opened-left)
 (struct-out opened-right)
 (struct-out guide)
 make-guide
 start
 move/update-index
 gap->seg
 insert
 seg->gap
 left-bound-gap
 right-bound-gap
 delete
 replace
 navigate
 gap-document-ropes
 gap-document-strings
 seg-document-ropes
 seg-document-strings)

;; The zipper carries one active guide.
;;
;; Public navigation should go through `move/update-index` so the zipper head
;; and installed guide index stay linked. The open/rise/split operations below
;; are structural navigation machinery; exposing them as user-facing movement
;; risks desynchronising the guide from the gap/seg head.
;;
;; TODO/API-boundary:
;; Revisit whether the active guide belongs inside `zipper`, in a wrapper
;; cursor/editor state, or in a richer navigation-mode abstraction.

(struct zipper (sys head before-summary after-summary crumbs guide)
  #:transparent
  #:property prop:custom-write
  (lambda (z out _mode)
    (print-zipper z out)))

(struct gap (left right) #:transparent)
(struct seg (left middle right) #:transparent)

;; A gap crumb takes the current collapsed subtree and pairs it with the
;; stashed sibling to form the parent gap's left/right.
;; opened-left: sibling on the right (we descended left).
;; opened-right: sibling on the left (we descended right).
(struct opened-left (before-summary right after-summary)
  #:transparent
  #:property prop:procedure
  (lambda (self _sys subtree)
    (match-define (opened-left before sibling after) self)
    (values subtree sibling before after)))

(struct opened-right (before-summary left after-summary)
  #:transparent
  #:property prop:procedure
  (lambda (self _sys subtree)
    (match-define (opened-right before sibling after) self)
    (values sibling subtree before after)))

;; One guide carries both head strategies. `seg-decide` and `gap-decide` are
;; each curried index-first: `(decide index)` is a 2-arg function reading the
;; two selector-projected summaries and returning the search sign(s). `navigate`
;; picks `as-seg` or `as-gap` by the head shape, so the same guide drives a seg
;; or a gap without changing instance; movement just swaps `index` by struct-copy
;; (the decide closures are index-independent, so no rebuild step is needed).
(struct guide (gap-decide seg-decide selector index) #:transparent)

;; Apply a guide as a gap / seg navigation function: curry in the current index,
;; then feed the two selector-projected summaries. `as-gap` returns a sign
;; (-1/0/1) for the point search; `as-seg` returns the centered value the seg
;; splitter offsets by ±1.
(define ((as-gap g) l r)
  (((guide-gap-decide g) (guide-index g))
   ((guide-selector g) l)
   ((guide-selector g) r)))

(define ((as-seg g) l r)
  (((guide-seg-decide g) (guide-index g))
   ((guide-selector g) l)
   ((guide-selector g) r)))

;; Build a guide from a curried, index-first `seg-decide`. The gap defaults to
;; the segment's left edge via `left-boundary`; pass `#:gap-decide` (also curried
;; index-first) to override with independent gap behaviour.
(define (make-guide seg-decide selector index
                    #:gap-decide [gap-decide (lambda (i) (left-boundary (seg-decide i)))])
  (guide gap-decide seg-decide selector index))

;; ---------- shape transforms ----------

(define (gap->seg z decompose)
  (match-define (gap left right) (zipper-head z))
  (define-values (l m r) (decompose left right))
  (struct-copy zipper z [head (seg l m r)]))

(define (insert z content)
  (gap->seg z (lambda (l r) (values l content r))))

(define (seg->gap z combine)
  (match-define (seg l m r) (zipper-head z))
  (define-values (left right) (combine l m r))
  (struct-copy zipper z [head (gap left right)]))

(define (left-bound-gap z)
  (define sys (zipper-sys z))
  (seg->gap z (lambda (l m r) (values l ((concat-rope sys) m r)))))

(define (right-bound-gap z)
  (define sys (zipper-sys z))
  (seg->gap z (lambda (l m r) (values ((concat-rope sys) l m) r))))

(define (delete z)
  (seg->gap z (lambda (l _m r) (values l r))))

(define (replace z content)
  (insert (delete z) content))

(define (ensure-gap z)
  (match z
    [(zipper _ (gap _ _) _ _ _ _) z]
    [(zipper _ (seg _ _ _) _ _ _ _) (left-bound-gap z)]
    [_ (raise-argument-error 'ensure-gap "zipper?" z)]))

(define (up z combine)
  (match-define (zipper sys (gap left right) _ _ crumbs _) z)
  (match crumbs
    ['() z]
    [(cons crumb rest)
     (define-values (pl pr pb pa) (crumb sys (combine left right)))
     (struct-copy zipper z
       [head (gap pl pr)]
       [before-summary pb]
       [after-summary pa]
       [crumbs rest])]))

(define (root z)
  (let loop ([z (ensure-gap z)])
    (if (null? (zipper-crumbs z))
        z
        (loop (up z (concat-rope (zipper-sys z)))))))

;; ---------- start ----------

(define ((start sys guide) rope)
  (zipper sys
          (gap (empty-rope sys) rope)
          (empty-summary sys)
          (empty-summary sys)
          '()
          guide))

;; ---------- structural opens ----------

(define (open-left z)
  (match-define (zipper sys (gap left right) before after crumbs _) z)
  (match left
    [(branch cl cr _)
     (struct-copy zipper z
       [head (gap cl cr)]
       [after-summary (summary+ sys (rope-summary right) after)]
       [crumbs (cons (opened-left before right after) crumbs)])]
    [(or (leaf _ _) (leaf-range _ _ _ _))
     (if (<= (leaf-piece-length left) 1)
         ;; Atomic/empty: push across at the same level, no crumb.
         (struct-copy zipper z
           [head (gap (empty-rope sys) ((concat-rope sys) left right))])
         (let-values ([(nl nr) (split-leaf-piece sys 'open-left left)])
           (struct-copy zipper z
             [head (gap nl nr)]
             [after-summary (summary+ sys (rope-summary right) after)]
             [crumbs (cons (opened-left before right after) crumbs)])))]))

(define (open-right z)
  (match-define (zipper sys (gap left right) before after crumbs _) z)
  (match right
    [(branch cl cr _)
     (struct-copy zipper z
       [head (gap cl cr)]
       [before-summary (summary+ sys before (rope-summary left))]
       [crumbs (cons (opened-right before left after) crumbs)])]
    [(or (leaf _ _) (leaf-range _ _ _ _))
     (if (<= (leaf-piece-length right) 1)
         ;; Atomic/empty: push across at the same level, no crumb.
         (struct-copy zipper z
           [head (gap ((concat-rope sys) left right) (empty-rope sys))])
         (let-values ([(nl nr) (split-leaf-piece sys 'open-right right)])
           (struct-copy zipper z
             [head (gap nl nr)]
             [before-summary (summary+ sys before (rope-summary left))]
             [crumbs (cons (opened-right before left after) crumbs)])))]))

(define (open-split-left z g)
  (match-define (zipper sys (gap left right) before after crumbs _) z)
  (let*-values ([(child-after) (summary+ sys (rope-summary right) after)]
                [(nl nr)       ((split-rope sys g) left before child-after)])
    (struct-copy zipper z
      [head (gap nl nr)]
      [after-summary child-after]
      [crumbs (cons (opened-left before right after) crumbs)])))

(define (open-split-right z g)
  (match-define (zipper sys (gap left right) before after crumbs _) z)
  (let*-values ([(child-before) (summary+ sys before (rope-summary left))]
                [(nl nr)        ((split-rope sys g) right child-before after)])
    (struct-copy zipper z
      [head (gap nl nr)]
      [before-summary child-before]
      [crumbs (cons (opened-right before left after) crumbs)])))

(define (open-seg-left z g)
  (match-define (zipper sys (gap left right) before after crumbs _) z)
  (let*-values ([(child-after) (summary+ sys (rope-summary right) after)]
                [(l m r)       ((seg-split-rope sys g) left before child-after)])
    (struct-copy zipper z
      [head (seg l m r)]
      [after-summary child-after]
      [crumbs (cons (opened-left before right after) crumbs)])))

(define (open-seg-right z g)
  (match-define (zipper sys (gap left right) before after crumbs _) z)
  (let*-values ([(child-before) (summary+ sys before (rope-summary left))]
                [(l m r)        ((seg-split-rope sys g) right child-before after)])
    (struct-copy zipper z
      [head (seg l m r)]
      [before-summary child-before]
      [crumbs (cons (opened-right before left after) crumbs)])))

(define (open-straddling z g)
  (match-define (zipper sys (gap left right) before after _ _) z)
  (define-values (l m r)
    ((seg-split-rope sys g) ((concat-rope sys) left right) before after))
  (struct-copy zipper z [head (seg l m r)]))

;; ---------- navigation ----------

;; Navigate preserves the current head shape: a gap head moves to a gap, a seg
;; head to a seg. Shape changes are the job of the transform verbs, not of
;; movement. The guide supplies both strategies; the head picks which.
(define (navigate z g)
  (match (zipper-head z)
    [(gap _ _)   (navigate-gap z (as-gap g))]
    [(seg _ _ _) (navigate-seg z (as-seg g))]
    [_ (raise-argument-error 'navigate "zipper with a gap or seg head" z)]))

(define (navigate-gap z g)
  (define z0 (ensure-gap z))
  (define sys (zipper-sys z0))
  (let rise ([z z0])
    (match-define (zipper _ (gap left right) before after _ _) z)
    (define local (summary+ sys (rope-summary left) (rope-summary right)))
    (cond
      [(and (not (null? (zipper-crumbs z)))
            (or (negative? (g before (summary+ sys local after)))
                (positive? (g (summary+ sys before local) after))))
       (rise (up z (concat-rope sys)))]
      [else
       (let search ([z z])
         (match-define (zipper _ (gap L R) B A _ _) z)
         (case (g (summary+ sys B (rope-summary L))
                  (summary+ sys (rope-summary R) A))
           [(0) z]
           [(-1) (search (open-split-left z g))]
           [(1) (search (open-split-right z g))]
           [else (error 'navigate-gap "guide must return -1, 0, or 1")]))])))

;; Segment navigation deliberately starts from the root gap and carves the
;; selected segment out of the whole rope. The lower-level `open-seg-*`
;; operations remain as internal machinery for a later smarter local/rise path.
(define (navigate-seg z g)
  (open-straddling (root z) g))

;; ---------- linked public movement ----------

(define (move/update-index z update-index)
  (define old-guide (zipper-guide z))
  (define new-guide
    (struct-copy guide old-guide
                 [index (update-index (guide-index old-guide))]))

  (define z/target
    (struct-copy zipper z [guide new-guide]))

  (define moved (navigate z/target new-guide))

  ;; Preserve the exact guide instance used to navigate, keeping the head and
  ;; installed guide index linked by construction.
  (struct-copy zipper moved [guide new-guide]))

;; ---------- document views ----------

;; Reconstruct the whole-document ropes around the cursor by walking crumbs
;; outward while preserving the current gap/seg boundary. Rising with `root`
;; would not necessarily preserve that boundary, so these rebuild the document
;; sides directly from the stashed siblings.

(define (gap-document-ropes z)
  (match-define (zipper sys (gap left right) _ _ crumbs _) z)
  (let loop ([left left] [right right] [crumbs crumbs])
    (match crumbs
      ['() (values left right)]
      [(cons (opened-left _ sibling _) rest)
       (loop left ((concat-rope sys) right sibling) rest)]
      [(cons (opened-right _ sibling _) rest)
       (loop ((concat-rope sys) sibling left) right rest)])))

(define (seg-document-ropes z)
  (match-define (zipper sys (seg left middle right) _ _ crumbs _) z)
  (let loop ([left left] [right right] [crumbs crumbs])
    (match crumbs
      ['() (values left middle right)]
      [(cons (opened-left _ sibling _) rest)
       (loop left ((concat-rope sys) right sibling) rest)]
      [(cons (opened-right _ sibling _) rest)
       (loop ((concat-rope sys) sibling left) right rest)])))

(define (gap-document-strings z)
  (define-values (l r) (gap-document-ropes z))
  (values (rope->string l) (rope->string r)))

(define (seg-document-strings z)
  (define-values (l m r) (seg-document-ropes z))
  (values (rope->string l) (rope->string m) (rope->string r)))

;; ---------- printer ----------

;; Show the whole document with the cursor inline: `^` marks a gap, `⟦…⟧`
;; brackets a selected segment. Long outer context is clipped with `…`.

(define preview-width 60)

(define (clip s side)
  (define n (string-length s))
  (cond
    [(<= n preview-width) s]
    [(eq? side 'left)  (string-append "…" (substring s (- n preview-width)))]
    [else              (string-append (substring s 0 preview-width) "…")]))

(define (print-zipper z out)
  (match (zipper-head z)
    [(gap _ _)
     (define-values (l r) (gap-document-strings z))
     (fprintf out "~a^~a" (clip l 'left) (clip r 'right))]
    [(seg _ _ _)
     (define-values (l m r) (seg-document-strings z))
     (fprintf out "~a⟦~a⟧~a" (clip l 'left) m (clip r 'right))]))
