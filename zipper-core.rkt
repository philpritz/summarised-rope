#lang racket

(require "rope-core.rkt")

(provide
 (struct-out zipper)
 (struct-out gap)
 (struct-out seg))

(struct zipper (sys head before-summary after-summary crumbs)
  #:transparent)

(struct gap (left right) #:transparent)
(struct seg (left middle right) #:transparent)
