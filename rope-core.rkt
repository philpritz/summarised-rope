#lang racket

(require racket/match)

(provide
 (struct-out summary-algebra)
 (struct-out leaf)
 (struct-out leaf-range)
 (struct-out branch)
 system
 empty-summary
 summary+
 rope?
 rope-summary
 leaf-rope
 make-leaf-range
 empty-rope
 empty-rope?
 branch-rope
 concat-rope
 rope-chunks
 rope->string
 string->rope
 piece-text
 piece->rope
 leaf-join
 leaf-piece-bounds
 leaf-piece-length
 split-leaf-piece
 rope-atomic?
 rope-children
 split-rope
 split-whole-rope
 seg-split-rope
 seg-split-whole-rope)

(struct summary-algebra (empty leaf append) #:transparent)

(struct leaf (text summary) #:transparent)
(struct leaf-range (text start end summary) #:transparent)
(struct branch (left right summary) #:transparent)

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
  (if (= start end)
      (empty-rope sys)
      (leaf-range text start end
                  ((summary-algebra-leaf sys)
                   (substring text start end)))))

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
      ((concat-rope sys) (piece->rope sys left) (piece->rope sys right))))

(define (normalize-piece sys piece)
  ((leaf-rope sys) (piece-text piece)))

(define (empty-rope sys)
  (leaf "" (empty-summary sys)))

(define (empty-rope? rope)
  (and (leaf? rope)
       (string=? "" (leaf-text rope))))

(define ((branch-rope sys) left right)
  (unless (and (rope? left) (rope? right))
    (raise-argument-error 'branch-rope "two ropes" (list left right)))
  (branch left right (summary+ sys (rope-summary left) (rope-summary right))))

(define ((concat-rope sys) . ropes)
  (foldr (lambda (left right)
           (cond
             [(empty-rope? left) right]
             [(empty-rope? right) left]
             [else ((branch-rope sys) left right)]))
         (empty-rope sys)
         ropes))

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
  (define parts (list->vector ropes))
  (define (build start end)
    (define count (- end start))
    (cond
      [(zero? count) (empty-rope sys)]
      [(= count 1) (vector-ref parts start)]
      [else
       (define middle (+ start (quotient count 2)))
       ((concat-rope sys) (build start middle) (build middle end))]))
  (build 0 (vector-length parts)))

(define ((string->rope sys) text #:chunk-size [chunk-size 1024])
  (unless (string? text)
    (raise-argument-error 'string->rope "string?" text))
  (unless (exact-positive-integer? chunk-size)
    (raise-argument-error 'string->rope "exact-positive-integer?" chunk-size))
  ((build-balanced sys)
   (map (leaf-rope sys) (chunk-string text chunk-size))))

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
    [(= len 1)
     (error who "cannot open atomic leaf: ~v" (substring text start end))]
    [else
     (define mid (+ start (quotient len 2)))
     (values (make-leaf-range sys text start mid)
             (make-leaf-range sys text mid end))]))

;; A rope node is "atomic" when it cannot be divided any further: an empty or
;; single-character leaf. Everything else is a divisible node with two children.
(define (rope-atomic? rope)
  (and (not (branch? rope))
       (<= (leaf-piece-length rope) 1)))

;; The two children of a divisible node, hiding the leaf/branch distinction: a
;; branch yields its stored children, a multi-character leaf simulates a branch
;; by splitting into halves. Callers descend uniformly without matching on the
;; node kind. Must not be called on an atomic node.
(define (rope-children sys rope)
  (if (branch? rope)
      (values (branch-left rope) (branch-right rope))
      (split-leaf-piece sys 'rope-children rope)))

(define ((split-rope sys guide [select identity]) rope before after)
  (let walk ([rope rope] [before before] [after after])
    (define (decide L R)
      (case (guide (select (summary+ sys before (rope-summary L)))
                   (select (summary+ sys (rope-summary R) after)))
        [(0) (values L R)]
        [(-1)
         (define-values (ll lr)
           (walk L before (summary+ sys (rope-summary R) after)))
         (values ll ((concat-rope sys) lr R))]
        [(1)
         (define-values (rl rr)
           (walk R (summary+ sys before (rope-summary L)) after))
         (values ((concat-rope sys) L rl) rr)]
        [else (error 'split-rope "guide must return -1, 0, or 1")]))
    (cond
      [(rope-atomic? rope)
       (case (guide (select before)
                    (select (summary+ sys (rope-summary rope) after)))
         [(-1 0) (values (empty-rope sys) rope)]
         [(1) (values rope (empty-rope sys))]
         [else (error 'split-rope "guide must return -1, 0, or 1")])]
      [else
       (define-values (L R) (rope-children sys rope))
       (decide L R)])))

(define ((split-whole-rope sys guide [select identity]) rope)
  ((split-rope sys guide select) rope (empty-summary sys) (empty-summary sys)))

(define ((seg-split-rope sys seg-guide [select identity]) rope before after)
  (define ((boundary offset) selected-left selected-right)
    (sgn (+ (seg-guide selected-left selected-right) offset)))
  (define-values (l rest)
    ((split-rope sys (boundary -1) select) rope before after))
  (define-values (m r)
    ((split-rope sys (boundary 1) select)
     rest
     (summary+ sys before (rope-summary l))
     after))
  (values l m r))

(define ((seg-split-whole-rope sys seg-guide [select identity]) rope)
  ((seg-split-rope sys seg-guide select)
   rope
   (empty-summary sys)
   (empty-summary sys)))
