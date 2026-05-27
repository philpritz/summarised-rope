#lang racket

(require racket/match
         racket/string
         "rope-core.rkt")

(provide
 (struct-out zipper)
 start
 gap-summary
 gap-sides
 zipper-split
 zipper->rope
 zipper->cursor-string
 zipper->debug-string
 insert-left
 insert-right
 open-left
 open-right
 shift-left
 shift-right
 up
 root
 navigate
 choose
 search
 split-rope)

;; A zipper is the cursor and the main interface to the text. The rope stores
;; persistent content; the zipper carries the current gap, its summary context,
;; and the path needed to move, inspect, and edit around that gap.
(struct zipper (sys left right before-summary after-summary crumbs)
  #:transparent
  #:property prop:custom-write
  (lambda (z out _mode)
    (display (zipper->debug-string z) out)))

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

(define ((gap->sides sys k) before left right after)
  (k (summary+ sys before left)
     (summary+ sys right after)))

(define (gap-sides z k)
  (gap-summary z
               (gap->sides (zipper-system z) k)))

(define (guide-choice guide select left-total right-total)
  (case (guide (select left-total) (select right-total))
    [(-1) -1]
    [(0) 0]
    [(1) 1]
    [else
     (error 'guide "must return -1, 0, or 1")]))

(define ((guide-search sys guide [select identity]) before left right after)
  ((gap->sides
    sys
    (lambda (left-total right-total)
      (guide-choice guide select left-total right-total)))
   before left right after))

(define ((guide-navigate sys guide [select identity]) before left right after)
  (define left-total (summary+ sys before left))
  (define right-total (summary+ sys right after))
  (case (guide-choice guide select left-total right-total)
    [(0) 0]
    [(-1)
     (case (guide-choice guide
                         select
                         before
                         (summary+ sys left right-total))
       [(1) -1]
       [(-1 0) -2])]
    [(1)
     (case (guide-choice guide
                         select
                         (summary+ sys left-total right)
                         after)
       [(-1) 1]
       [(0 1) 2])]))

(define ((search-step guide [select identity]) z left-k stay-k right-k)
  (define sys (zipper-system z))
  (unless (procedure? guide)
    (raise-argument-error 'search-step "procedure?" guide))
  (case (gap-summary z (guide-search sys guide select))
    [(-1) (left-k (open-left z))]
    [(0) (stay-k z)]
    [(1) (right-k (open-right z))]))

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
     (if (= (leaf-piece-length left) 1)
         (zipper sys
                 (empty-rope sys)
                 (leaf-join sys left right)
                 before
                 after
                 crumbs)
         (let-values ([(child-left child-right)
                       (split-leaf-piece sys 'open-left left)])
           (define crumb (opened-leaf-left before right after))
           (zipper sys child-left child-right before child-after (cons crumb crumbs))))]))

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
     (if (= (leaf-piece-length right) 1)
         (zipper sys
                 (leaf-join sys left right)
                 (empty-rope sys)
                 before
                 after
                 crumbs)
         (let-values ([(child-left child-right)
                       (split-leaf-piece sys 'open-right right)])
           (define crumb (opened-leaf-right before left after))
           (zipper sys child-left child-right child-before after (cons crumb crumbs))))]))

(define ((arrived? guide [select identity]) z)
  (unless (procedure? guide)
    (raise-argument-error 'arrived? "procedure?" guide))
  (zero? (gap-sides z
                    (lambda (left-total right-total)
                      (guide-choice guide select left-total right-total)))))

(define (choose guide z [select identity])
  ((search-step guide select) z identity identity identity))

(define ((search guide [select identity]) z)
  (let loop ([current z])
    ((search-step guide select) current loop identity loop)))

(define ((navigate guide [select identity]) z)
  ((navigation-step guide select)
   z
   (navigate guide select)
   (search guide select)
   identity
   (search guide select)
   (navigate guide select)))

(define ((navigation-step guide [select identity])
         z
         before-k left-k stay-k right-k after-k)
  (define sys (zipper-system z))
  (unless (procedure? guide)
    (raise-argument-error 'navigation-step "procedure?" guide))
  (case (gap-summary z (guide-navigate sys guide select))
    [(-2) (before-k (shift-left z))]
    [(-1) (left-k (open-left z))]
    [(0) (stay-k z)]
    [(1) (right-k (open-right z))]
    [(2) (after-k (shift-right z))]))

(define (left-context-crumb? crumb)
  (or (opened-right? crumb)
      (opened-leaf-right? crumb)))

(define (right-context-crumb? crumb)
  (or (opened-left? crumb)
      (opened-leaf-left? crumb)))

(define (gap->rope sys crumb left right)
  (if (or (opened-leaf-left? crumb)
          (opened-leaf-right? crumb))
      (leaf-join sys left right)
      ((concat-rope sys) (piece->rope sys left) (piece->rope sys right))))

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
                    ((concat-rope sys) left right)))))

(define (zipper->cursor-string z [marker "|"])
  (zipper-split z
                (lambda (left right)
                  (string-append (rope->string left)
                                 marker
                                 (rope->string right)))))

(define (zipper->debug-string z)
  (define (text-gap label left right)
    (string-join
     (list (format "~a-left:  ~v ^" label left)
           (format "~a-right: ~v" label right))
     "\n"))
  (define cursor
    (zipper-split z
                  (lambda (left right)
                    (text-gap "cursor"
                              (rope->string left)
                              (rope->string right)))))
  (define (totals-gap left right)
    (string-join
     (list (format "left-total-summary:  ~v ^" left)
           (format "right-total-summary: ~v" right))
     "\n"))
  (define totals
    (gap-sides z
               (lambda (left right)
                 (totals-gap left right))))
  (string-join
   (list cursor
         totals)
   "\n"))

(define/contract (insert-left z inserted)
  (-> zipper? rope? zipper?)
  (match-let ([(zipper sys left _ _ _ _) z])
    (struct-copy zipper z
                 [left ((concat-rope sys) left inserted)])))

(define/contract (insert-right z inserted)
  (-> zipper? rope? zipper?)
  (match-let ([(zipper sys _ right _ _ _) z])
    (struct-copy zipper z
                 [right ((concat-rope sys) inserted right)])))

(define (shift-left z)
  (let loop ([current z])
    (match-define (zipper sys left right _before _after crumbs) current)
    (match crumbs
      ['()
       ((start (zipper-system current)) (zipper->rope current))]
      [(cons crumb _rest)
       (if (left-context-crumb? crumb)
           (match crumb
             [(opened-right before sibling after)
              (zipper sys sibling (gap->rope sys crumb left right) before after _rest)]
             [(opened-leaf-right before sibling after)
              (zipper sys sibling (gap->rope sys crumb left right) before after _rest)])
           (loop (up current)))])))

(define (shift-right z)
  (let loop ([current z])
    (match-define (zipper sys left right _before _after crumbs) current)
    (match crumbs
      ['()
       (zipper sys
               (zipper->rope current)
               (empty-rope sys)
               (empty-summary sys)
               (empty-summary sys)
               '())]
      [(cons crumb _rest)
       (if (right-context-crumb? crumb)
           (match crumb
             [(opened-left before sibling after)
              (zipper sys (gap->rope sys crumb left right) sibling before after _rest)]
             [(opened-leaf-left before sibling after)
              (zipper sys (gap->rope sys crumb left right) sibling before after _rest)])
           (loop (up current)))])))

(define ((split-rope sys guide [select identity]) rope)
  (zipper-split
   ((navigate guide select) ((start sys) rope))
   values))

(module+ test
  (require rackunit)

  (define test-sys (system (summary-algebra 0 string-length +)))

  (define (test-rope text)
    ((string->rope test-sys) text #:chunk-size 2))

  (define ((position-guide count) left _right)
    (cond
      [(< left count) 1]
      [(> left count) -1]
      [else 0]))

  (define (check-split z before after)
    (zipper-split z
                  (lambda (left right)
                    (check-equal? (rope->string left) before)
                    (check-equal? (rope->string right) after))))

  (define (split-strings z)
    (zipper-split z
                  (lambda (left right)
                    (list (rope->string left) (rope->string right)))))

  (define (tagged tag)
    (lambda (next)
      (cons tag (split-strings next))))

  (test-case "search-step picks one of three continuations"
    (define z (zipper test-sys (test-rope "ab") (test-rope "cd") 0 0 '()))
    (check-equal? ((search-step (position-guide 1))
                   z
                   (tagged 'left)
                   (tagged 'stay)
                   (tagged 'right))
                  '(left "a" "bcd"))
    (check-equal? ((search-step (position-guide 2))
                   z
                   (tagged 'left)
                   (tagged 'stay)
                   (tagged 'right))
                  '(stay "ab" "cd"))
    (check-equal? ((search-step (position-guide 3))
                   z
                   (tagged 'left)
                   (tagged 'stay)
                   (tagged 'right))
                  '(right "abc" "d")))

  (test-case "navigation-step picks one of five continuations"
    (define (navigation-choice count)
      (define z (zipper test-sys (test-rope "ab") (test-rope "cd") 0 0 '()))
      ((navigation-step (position-guide count))
       z
       (tagged 'before)
       (tagged 'left)
       (tagged 'stay)
       (tagged 'right)
       (tagged 'after)))
    (check-equal? (navigation-choice 0) '(before "" "abcd"))
    (check-equal? (navigation-choice 1) '(left "a" "bcd"))
    (check-equal? (navigation-choice 2) '(stay "ab" "cd"))
    (check-equal? (navigation-choice 3) '(right "abc" "d"))
    (check-equal? (navigation-choice 4) '(after "abcd" "")))

  (test-case "guide-navigate adapts a guide to five directions"
    (define (navigation-choice count)
      ((guide-navigate test-sys (position-guide count))
       0 2 2 0))
    (check-equal? (navigation-choice 0) -2)
    (check-equal? (navigation-choice 1) -1)
    (check-equal? (navigation-choice 2) 0)
    (check-equal? (navigation-choice 3) 1)
    (check-equal? (navigation-choice 4) 2))

  (test-case "navigate applies the guide-derived navigator"
    (define z (zipper test-sys (test-rope "ab") (test-rope "cd") 0 0 '()))
    (check-equal? (split-strings ((navigate (position-guide 1)) z))
                  '("a" "bcd"))
    (check-equal? ((navigation-step (position-guide 4))
                   z
                   (tagged 'before)
                   (tagged 'left)
                   (tagged 'stay)
                   (tagged 'right)
                   (tagged 'after))
                  '(after "abcd" "")))

  (test-case "open-left discovers an empty boundary gap"
    (define z ((start test-sys) (test-rope "abc")))
    (define opened (open-left z))
    (check-equal? (rope->string (zipper-left opened)) "")
    (check-equal? (rope->string (zipper-right opened)) "")
    (check-equal? (zipper-before-summary opened) 0)
    (check-equal? (zipper-after-summary opened) 3)
    (check-equal? (rope->string (zipper->rope opened)) "abc")
    (check-equal? (length (zipper-crumbs opened)) 1)
    (check-equal? (rope->string (zipper-left (up opened))) "")
    (check-equal? (rope->string (zipper-right (up opened))) "abc"))

  (test-case "open-right discovers an empty boundary gap"
    (define z (shift-right ((start test-sys) (test-rope "abc"))))
    (define opened (open-right z))
    (check-equal? (rope->string (zipper-left opened)) "")
    (check-equal? (rope->string (zipper-right opened)) "")
    (check-equal? (zipper-before-summary opened) 3)
    (check-equal? (zipper-after-summary opened) 0)
    (check-equal? (rope->string (zipper->rope opened)) "abc")
    (check-equal? (length (zipper-crumbs opened)) 1)
    (check-equal? (rope->string (zipper-left (up opened))) "abc")
    (check-equal? (rope->string (zipper-right (up opened))) ""))

  (test-case "insert-left inserts before the gap and leaves cursor after insertion"
    (define z ((navigate (position-guide 2)) ((start test-sys) (test-rope "abcd"))))
    (define inserted (test-rope "XY"))
    (define edited (insert-left z inserted))
    (check-split edited "abXY" "cd")
    (check-equal? (rope->string (zipper->rope edited)) "abXYcd"))

  (test-case "insert-right inserts after the gap and leaves cursor before insertion"
    (define z ((navigate (position-guide 2)) ((start test-sys) (test-rope "abcd"))))
    (define inserted (test-rope "XY"))
    (define edited (insert-right z inserted))
    (check-split edited "ab" "XYcd")
    (check-equal? (rope->string (zipper->rope edited)) "abXYcd"))

  (test-case "zipper cursor strings show the cursor and local gap"
    (define z ((navigate (position-guide 2)) ((start test-sys) (test-rope "abcd"))))
    (check-equal? (zipper->cursor-string z "^") "ab^cd")
    (check-true (string-contains? (zipper->debug-string z)
                                  "cursor-left:  \"ab\""))
    (check-true (string-contains? (zipper->debug-string z)
                                  "cursor-right: \"cd\""))
    (check-false (string-contains? (zipper->debug-string z)
                                   "local-left")))

  (test-case "search descends through the right piece"
    (define z ((start test-sys) (test-rope "abc")))
    (check-split ((search (position-guide 1)) z) "a" "bc"))

  (test-case "search descends through the left piece"
    (define z (shift-right ((start test-sys) (test-rope "abc"))))
    (check-split ((search (position-guide 1)) z) "a" "bc"))

  (test-case "navigate searches from either side"
    (define z ((start test-sys) (test-rope "abcd")))
    (define end-z (shift-right z))
    (check-split ((navigate (position-guide 0)) z) "" "abcd")
    (check-split ((navigate (position-guide 2)) z) "ab" "cd")
    (check-split ((navigate (position-guide 4)) z) "abcd" "")
    (check-split ((navigate (position-guide 2)) end-z) "ab" "cd")))
