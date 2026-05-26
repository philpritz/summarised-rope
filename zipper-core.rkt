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
 (struct-out opened-leaf-right))

(struct zipper (sys head before-summary after-summary crumbs)
  #:transparent)

(struct gap (left right) #:transparent)
(struct seg (left middle right) #:transparent)

;; A gap crumb is a parent context with the current gap as its hole.
;; Applying one to a child gap rebuilds the same gap at the parent level.
(struct opened-left (before-summary right after-summary)
  #:transparent
  #:property prop:procedure
  (lambda (self sys left right)
    (match-define (opened-left before sibling after) self)
    (values left
            ((concat-rope sys) right sibling)
            before
            after)))

(struct opened-right (before-summary left after-summary)
  #:transparent
  #:property prop:procedure
  (lambda (self sys left right)
    (match-define (opened-right before sibling after) self)
    (values ((concat-rope sys) sibling left)
            right
            before
            after)))

(struct opened-leaf-left (before-summary right after-summary)
  #:transparent
  #:property prop:procedure
  (lambda (self sys left right)
    (match-define (opened-leaf-left before sibling after) self)
    (values (piece->rope sys left)
            (leaf-join sys right sibling)
            before
            after)))

(struct opened-leaf-right (before-summary left after-summary)
  #:transparent
  #:property prop:procedure
  (lambda (self sys left right)
    (match-define (opened-leaf-right before sibling after) self)
    (values (leaf-join sys sibling left)
            (piece->rope sys right)
            before
            after)))
