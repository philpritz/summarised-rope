#lang racket

(require racket/match)

(provide
 (struct-out summary-algebra)
 (struct-out leaf)
 (struct-out branch)
 (struct-out zipper)
 system
 rope?
 rope-summary
 leaf-rope
 empty-rope
 empty-rope?
 branch-rope
 concat-rope
 rope-chunks
 rope->string
 string->rope
 start
 gap-summary
 zipper-split
 zipper->rope
 open-left
 open-right
 up
 root
 choose
 search
 split-rope)

(struct summary-algebra (empty leaf append) #:transparent)

(struct leaf (text summary) #:transparent)
(struct leaf-range (text start end summary) #:transparent)
(struct branch (left right summary) #:transparent)

;; A zipper is the cursor and the main interface to the text. The rope stores
;; persistent content; the zipper carries the current gap, its summary context,
;; and the path needed to move, inspect, and edit around that gap.
(struct zipper (sys left right before-summary after-summary crumbs) #:transparent)

;; A gap crumb is a parent context with the current gap as its hole.
;; Applying one to a child gap rebuilds the same gap at the parent level.
(struct opened-left (before-summary right after-summary)
  #:transparent
  #:property prop:procedure
  (lambda (self sys left right)
    (match-define (opened-left before sibling after) self)
    (values left
            (((concat-rope sys) right) sibling)
            before
            after)))

(struct opened-right (before-summary left after-summary)
  #:transparent
  #:property prop:procedure
  (lambda (self sys left right)
    (match-define (opened-right before sibling after) self)
    (values (((concat-rope sys) sibling) left)
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

(define (system algebra)
  (unless (summary-algebra? algebra)
    (raise-argument-error 'system "summary-algebra?" algebra))
  (unless (procedure? (summary-algebra-leaf algebra))
    (raise-argument-error 'system "summary algebra with a leaf procedure" algebra))
  (unless (procedure? (summary-algebra-append algebra))
    (raise-argument-error 'system "summary algebra with an append procedure" algebra))
  algebra)

(define (empty-summary sys)
  (summary-algebra-empty sys))

(define (summary+ sys left right)
  ((summary-algebra-append sys) left right))

(define (rope? value)
  (or (leaf? value) (leaf-range? value) (branch? value)))

(define (rope-summary rope)
  (match rope
    [(leaf _ summary) summary]
    [(leaf-range _ _ _ summary) summary]
    [(branch _ _ summary) summary]
    [_
     (raise-argument-error 'rope-summary "rope?" rope)]))

(define ((leaf-rope sys) text)
  (unless (string? text)
    (raise-argument-error 'leaf-rope "string?" text))
  (leaf text ((summary-algebra-leaf sys) text)))

(define (make-leaf-range sys text start end)
  (leaf-range text start end
              ((summary-algebra-leaf sys)
               (substring text start end))))

(define (piece-text piece)
  (match piece
    [(leaf text _) text]
    [(leaf-range text start end _) (substring text start end)]
    [_
     (raise-argument-error 'piece-text "leaf or leaf-range?" piece)]))

(define (piece->rope sys piece)
  (if (leaf-range? piece)
      ((leaf-rope sys) (piece-text piece))
      piece))

(define (leaf-compatible? left right)
  (match* (left right)
    [((leaf-range text left-start left-end _)
      (leaf-range same-text right-start right-end _))
     (and (eq? text same-text)
          (= left-end right-start))]
    [(_ _) #f]))

(define (leaf-join sys left right)
  (if (leaf-compatible? left right)
      (normalize-piece sys (make-leaf-range sys
                                            (leaf-range-text left)
                                            (leaf-range-start left)
                                            (leaf-range-end right)))
      (((concat-rope sys) (piece->rope sys left))
       (piece->rope sys right))))

(define (normalize-piece sys piece)
  ((leaf-rope sys) (piece-text piece)))

(define (empty-rope sys)
  (leaf "" (empty-summary sys)))

(define (empty-rope? rope)
  (and (leaf? rope)
       (string=? "" (leaf-text rope))))

(define (((branch-rope sys) left) right)
  (unless (and (rope? left) (rope? right))
    (raise-argument-error 'branch-rope "two ropes" (list left right)))
  (branch left right (summary+ sys (rope-summary left) (rope-summary right))))

(define (((concat-rope sys) left) right)
  (cond
    [(empty-rope? left) right]
    [(empty-rope? right) left]
    [else (((branch-rope sys) left) right)]))

(define (rope-chunks rope)
  (match rope
    [(leaf text _)
     (if (string=? "" text) '() (list text))]
    [(leaf-range text start end _)
     (define slice (substring text start end))
     (if (string=? "" slice) '() (list slice))]
    [(branch left right _)
     (append (rope-chunks left) (rope-chunks right))]
    [_
     (raise-argument-error 'rope-chunks "rope?" rope)]))

(define (rope->string rope)
  (apply string-append (rope-chunks rope)))

(define (chunk-string text chunk-size)
  (for/list ([start (in-range 0 (string-length text) chunk-size)])
    (substring text start
               (min (string-length text) (+ start chunk-size)))))

(define ((build-balanced sys) ropes)
  (define join (concat-rope sys))
  (define parts (list->vector ropes))
  (define (build start end)
    (define count (- end start))
    (cond
      [(zero? count) (empty-rope sys)]
      [(= count 1) (vector-ref parts start)]
      [else
       (define middle (+ start (quotient count 2)))
       ((join (build start middle))
        (build middle end))]))
  (build 0 (vector-length parts)))

(define ((string->rope sys) text #:chunk-size [chunk-size 1024])
  (unless (string? text)
    (raise-argument-error 'string->rope "string?" text))
  (unless (exact-positive-integer? chunk-size)
    (raise-argument-error 'string->rope "exact-positive-integer?" chunk-size))
  ((build-balanced sys)
   (map (leaf-rope sys) (chunk-string text chunk-size))))

(define ((start sys) rope)
  (unless (rope? rope)
    (raise-argument-error 'start "rope?" rope))
  (zipper sys
          (empty-rope sys)
          rope
          (empty-summary sys)
          (empty-summary sys)
          '()))

(define (gap-summary z k)
  (match-define (zipper _sys left right before after _crumbs) z)
  (k before
     (rope-summary left)
     (rope-summary right)
     after))

(define (zipper-system z)
  (match z
    [(zipper sys _ _ _ _ _) sys]
    [_
     (raise-argument-error 'zipper-system "zipper?" z)]))

(define ((gap-choice sys guide [select identity]) before left right after)
  (guide (select (summary+ sys before left))
         (select (summary+ sys right after))))

(define (leaf-piece-bounds piece)
  (match piece
    [(leaf text _) (values text 0 (string-length text))]
    [(leaf-range text start end _) (values text start end)]
    [_
     (raise-argument-error 'leaf-piece-bounds "leaf or leaf-range?" piece)]))

(define (leaf-piece-length piece)
  (define-values (_text start end) (leaf-piece-bounds piece))
  (- end start))

(define (split-leaf-piece sys who piece)
  (define-values (text start end) (leaf-piece-bounds piece))
  (define len (- end start))
  (cond
    [(zero? len)
     (error who "cannot open an empty leaf")]
    [(= len 1)
     (error who "cannot open atomic leaf: ~v" (substring text start end))]
    [else
     (define mid (+ start (quotient len 2)))
     (values (make-leaf-range sys text start mid)
             (make-leaf-range sys text mid end))]))

(define (open-left z)
  (match-define (zipper sys left right before after crumbs) z)
  (define-values (_before _left-summary right-summary _after)
    (gap-summary z values))
  (define child-after (summary+ sys right-summary after))
  (match left
    [(branch child-left child-right _)
     (define crumb (opened-left before right after))
     (zipper sys child-left child-right before child-after (cons crumb crumbs))]
    [(or (leaf _ _) (leaf-range _ _ _ _))
     (case (leaf-piece-length left)
       [(0)
        (error 'open-left "cannot open before the start of the rope")]
       [(1)
        (zipper sys
                (empty-rope sys)
                (leaf-join sys left right)
                before
                after
                crumbs)]
       [else
        (define crumb (opened-leaf-left before right after))
        (define-values (child-left child-right)
          (split-leaf-piece sys 'open-left left))
        (zipper sys child-left child-right before child-after (cons crumb crumbs))])]))

(define (open-right z)
  (match-define (zipper sys left right before after crumbs) z)
  (define-values (_before left-summary _right-summary _after)
    (gap-summary z values))
  (define child-before (summary+ sys before left-summary))
  (match right
    [(branch child-left child-right _)
     (define crumb (opened-right before left after))
     (zipper sys child-left child-right child-before after (cons crumb crumbs))]
    [(or (leaf _ _) (leaf-range _ _ _ _))
     (case (leaf-piece-length right)
       [(0)
        (error 'open-right "cannot open past the end of the rope")]
       [(1)
        (zipper sys
                (leaf-join sys left right)
                (empty-rope sys)
                before
                after
                crumbs)]
       [else
        (define crumb (opened-leaf-right before left after))
        (define-values (child-left child-right)
          (split-leaf-piece sys 'open-right right))
        (zipper sys child-left child-right child-before after (cons crumb crumbs))])]))

(define ((arrived? guide [select identity]) z)
  (define sys (zipper-system z))
  (unless (procedure? guide)
    (raise-argument-error 'arrived? "procedure?" guide))
  (zero? (gap-summary z (gap-choice sys guide select))))

(define ((choose guide [select identity]) z)
  (define sys (zipper-system z))
  (unless (procedure? guide)
    (raise-argument-error 'choose "procedure?" guide))
  (case (gap-summary z (gap-choice sys guide select))
    [(-1) (open-left z)]
    [(0) z]
    [(1) (open-right z)]
    [else
     (error 'choose "guide must return -1, 0, or 1")]))

(define ((search guide [select identity]) z)
  (let ([step (choose guide select)]
        [done? (arrived? guide select)])
    (call/ec
     (lambda (k)
       (for/fold ([current z])
                 ([_ (in-naturals)])
         (if (done? current)
             (k current)
             (step current)))))))

(define (up z)
  (match-define (zipper sys left right _before _after crumbs) z)
  (match crumbs
    ['() z]
    [(cons crumb rest)
     (define-values (parent-left parent-right parent-before parent-after)
       (crumb sys left right))
     (zipper sys parent-left parent-right parent-before parent-after rest)]))

(define (root z)
  (let loop ([z z])
    (if (null? (zipper-crumbs z))
        z
        (loop (up z)))))

(define (zipper-split z k)
  (match-define (zipper _sys left right _before _after _crumbs)
    (root z))
  (k left right))

(define (zipper->rope z)
  (let ([sys (zipper-system z)])
    (zipper-split z
                  (lambda (left right)
                    (((concat-rope sys) left) right)))))

(define ((split-rope sys guide [select identity]) rope)
  (zipper-split
   ((search guide select) ((start sys) rope))
   values))
