#lang racket

(require racket/match
         "rope-core.rkt")

(provide
 (struct-out zipper)
 (struct-out gap)
 (struct-out seg)
 (struct-out opened-left)
 (struct-out opened-right)
 (struct-out opened-leaf-left)
 (struct-out opened-leaf-right)
 gap->seg
 insert
 seg->gap
 left-bound-gap
 right-bound-gap
 delete
 up
 start
 open-left
 open-right
 open-seg-left
 open-seg-right
 open-straddling
 rise-from-left
 rise-from-centre
 rise-from-right)

(struct zipper (sys head before-summary after-summary crumbs)
  #:transparent)

(struct gap (left right) #:transparent)
(struct seg (left middle right) #:transparent)

;; A gap crumb takes the current head's collapsed subtree and pairs it
;; with the stashed sibling to form the parent gap's left/right.
;; opened-*-left variants put the sibling on the right (we descended left);
;; opened-*-right variants put the sibling on the left (we descended right).
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

(struct opened-leaf-left (before-summary right after-summary)
  #:transparent
  #:property prop:procedure
  (lambda (self _sys subtree)
    (match-define (opened-leaf-left before sibling after) self)
    (values subtree sibling before after)))

(struct opened-leaf-right (before-summary left after-summary)
  #:transparent
  #:property prop:procedure
  (lambda (self _sys subtree)
    (match-define (opened-leaf-right before sibling after) self)
    (values sibling subtree before after)))

(define (gap->seg z decompose)
  (match-define (zipper sys (gap left right) before after crumbs) z)
  (define-values (l m r) (decompose left right))
  (zipper sys (seg l m r) before after crumbs))

(define (insert z content)
  (gap->seg z (lambda (l r) (values l content r))))

(define (seg->gap z combine)
  (match-define (zipper sys (seg l m r) before after crumbs) z)
  (define-values (left right) (combine l m r))
  (zipper sys (gap left right) before after crumbs))

(define (left-bound-gap z)
  (define sys (zipper-sys z))
  (seg->gap z (lambda (l m r) (values l ((concat-rope sys) m r)))))

(define (right-bound-gap z)
  (define sys (zipper-sys z))
  (seg->gap z (lambda (l m r) (values ((concat-rope sys) l m) r))))

(define (delete z)
  (seg->gap z (lambda (l m r) (values l r))))

(define (up z combine)
  (match-define (zipper sys (gap left right) before after crumbs) z)
  (match crumbs
    ['() z]
    [(cons crumb rest)
     (define subtree (combine left right))
     (define-values (pl pr pb pa) (crumb sys subtree))
     (zipper sys (gap pl pr) pb pa rest)]))

;; ---------- start ----------

(define ((start sys) rope)
  (zipper sys
          (gap (empty-rope sys) rope)
          (empty-summary sys)
          (empty-summary sys)
          '()))

;; ---------- open-* ----------
;;
;; open-left  : descend into the current gap's left side using a gap guide.
;;              Splits the left rope at the guide's target, then makes a new
;;              gap head whose two halves are the split. The current right
;;              side gets stashed in the crumb as the sibling.
;;
;; open-right : symmetric on the right side.
;;
;; The seg variants (open-seg-left / open-seg-right) are the same shape but
;; use seg-split-rope, producing a seg head with the carved-out segment as m.

(define (open-left z guide)
  (match-define (zipper sys (gap left right) before after crumbs) z)
  (define child-after (summary+ sys (rope-summary right) after))
  (define-values (nl nr)
    ((split-rope sys guide) left before child-after))
  (define crumb (opened-left before right after))
  (zipper sys (gap nl nr) before child-after (cons crumb crumbs)))

(define (open-right z guide)
  (match-define (zipper sys (gap left right) before after crumbs) z)
  (define child-before (summary+ sys before (rope-summary left)))
  (define-values (nl nr)
    ((split-rope sys guide) right child-before after))
  (define crumb (opened-right before left after))
  (zipper sys (gap nl nr) child-before after (cons crumb crumbs)))

(define (open-seg-left z seg-guide)
  (match-define (zipper sys (gap left right) before after crumbs) z)
  (define child-after (summary+ sys (rope-summary right) after))
  (define-values (l m r)
    ((seg-split-rope sys seg-guide) left before child-after))
  (define crumb (opened-left before right after))
  (zipper sys (seg l m r) before child-after (cons crumb crumbs)))

(define (open-seg-right z seg-guide)
  (match-define (zipper sys (gap left right) before after crumbs) z)
  (define child-before (summary+ sys before (rope-summary left)))
  (define-values (l m r)
    ((seg-split-rope sys seg-guide) right child-before after))
  (define crumb (opened-right before left after))
  (zipper sys (seg l m r) child-before after (cons crumb crumbs)))

;; open-straddling : the segment crosses the current gap but stays within
;; (left + right). Combine the two sides, carve out m via seg-split-rope,
;; produce a seg head at the same level (no crumb push, no level change).

(define (open-straddling z seg-guide)
  (match-define (zipper sys (gap left right) before after crumbs) z)
  (define combined ((concat-rope sys) left right))
  (define-values (l m r)
    ((seg-split-rope sys seg-guide) combined before after))
  (zipper sys (seg l m r) before after crumbs))

;; ---------- rise-from-* ----------
;;
;; Each rise variant ascends with `up` (using concat-rope as the combine)
;; until the segment is contained in the current local subtree (i.e. the
;; seg guide stops asking us to look further outside).
;;
;; Convention reminder: seg guide returns the segment's position relative
;; to the cursor: +2 segment far right, +1 left edge here, 0 inside,
;; -1 right edge here, -2 segment far left.
;;
;; "rise-from-left"   - cursor starts in `a` (seg = +2). Rise until the
;;                      segment's right edge falls within our local subtree.
;; "rise-from-right"  - cursor starts in `c` (seg = -2). Mirror.
;; "rise-from-centre" - cursor starts inside the segment (seg = 0). Rise
;;                      until both edges are within our local subtree.

(define (rise-from-left z seg-guide)
  (define sys (zipper-sys z))
  (let loop ([z z])
    (match-define (zipper _ (gap left right) before after _) z)
    (define right-edge-total
      (summary+ sys (summary+ sys before (rope-summary left)) (rope-summary right)))
    (case (seg-guide right-edge-total after)
      [(-2 -1) z]            ; segment fully inside our subtree on the left
      [(0 1 2) (loop (up z (concat-rope sys)))])))

(define (rise-from-right z seg-guide)
  (define sys (zipper-sys z))
  (let loop ([z z])
    (match-define (zipper _ (gap left right) before after _) z)
    (define left-edge-total before)
    (case (seg-guide left-edge-total
                     (summary+ sys (summary+ sys (rope-summary left) (rope-summary right)) after))
      [(1 2) z]              ; segment fully inside our subtree on the right
      [(-2 -1 0) (loop (up z (concat-rope sys)))])))

(define (rise-from-centre z seg-guide)
  (define sys (zipper-sys z))
  (let loop ([z z])
    (match-define (zipper _ (gap left right) before after _) z)
    (define left-edge-total before)
    (define right-edge-total
      (summary+ sys (summary+ sys before (rope-summary left)) (rope-summary right)))
    (define left-answer (seg-guide left-edge-total
                                   (summary+ sys (summary+ sys (rope-summary left) (rope-summary right)) after)))
    (define right-answer (seg-guide right-edge-total after))
    (cond
      ;; both edges of subtree see the segment as internal -> contained
      [(and (member left-answer '(0 1 2))
            (member right-answer '(-2 -1 0)))
       z]
      [else (loop (up z (concat-rope sys)))])))
