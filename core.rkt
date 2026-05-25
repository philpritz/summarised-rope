#lang racket

(provide
 (struct-out measure-algebra)
 (struct-out locator)
 (struct-out leaf)
 (struct-out branch)
 system
 rope?
 rope-measure
 leaf-rope
 empty-rope
 empty-rope?
 branch-rope
 concat-rope
 rope-chunks
 rope->string
 string->rope
 split-rope
 insert-rope
 delete-rope
 slice-rope
 replace-rope
 valid-rope?)

(struct measure-algebra (leaf-measure branch-measure) #:transparent)
(struct locator (inside? split-leaf) #:transparent)

(struct leaf (text measure) #:transparent)
(struct branch (left right measure) #:transparent)

(define (rope? value)
  (or (leaf? value) (branch? value)))

(define (rope-measure a-rope)
  (cond
    [(leaf? a-rope) (leaf-measure a-rope)]
    [(branch? a-rope) (branch-measure a-rope)]
    [else
     (raise-argument-error 'rope-measure "rope?" a-rope)]))

(define (system an-algebra)
  (unless (measure-algebra? an-algebra)
    (raise-argument-error 'system "measure-algebra?" an-algebra))
  (unless (procedure? (measure-algebra-leaf-measure an-algebra))
    (raise-argument-error 'system "measure algebra with a leaf procedure" an-algebra))
  (unless (procedure? (measure-algebra-branch-measure an-algebra))
    (raise-argument-error 'system "measure algebra with a branch procedure" an-algebra))
  an-algebra)

(define (leaf-rope sys)
  (define leaf-measure (measure-algebra-leaf-measure sys))
  (lambda (text)
    (unless (string? text)
      (raise-argument-error 'leaf-rope "string?" text))
    (leaf text (leaf-measure text))))

(define (empty-rope sys)
  ((leaf-rope sys) ""))

(define (empty-rope? a-rope)
  (and (leaf? a-rope)
       (zero? (string-length (leaf-text a-rope)))))

(define (branch-rope sys)
  (define branch-measure-proc (measure-algebra-branch-measure sys))
  (lambda (left right)
    (unless (and (rope? left) (rope? right))
      (raise-argument-error 'branch-rope "two ropes" (list left right)))
    (branch left right
            (branch-measure-proc (rope-measure left)
                                 (rope-measure right)))))

(define (concat-rope sys)
  (define make-branch (branch-rope sys))
  (lambda (left right)
    (cond
      [(empty-rope? left) right]
      [(empty-rope? right) left]
      [else (make-branch left right)])))

(define (rope-chunks a-rope)
  (cond
    [(leaf? a-rope)
     (if (empty-rope? a-rope)
         '()
         (list (leaf-text a-rope)))]
    [(branch? a-rope)
     (append (rope-chunks (branch-left a-rope))
             (rope-chunks (branch-right a-rope)))]
    [else
     (raise-argument-error 'rope-chunks "rope?" a-rope)]))

(define (rope->string a-rope)
  (apply string-append (rope-chunks a-rope)))

(define (chunk-string text chunk-size)
  (for/list ([start (in-range 0 (string-length text) chunk-size)])
    (substring text start
               (min (string-length text) (+ start chunk-size)))))

(define (build-balanced sys ropes)
  (define join (concat-rope sys))
  (define parts (list->vector ropes))
  (define (build start end)
    (define part-count (- end start))
    (cond
      [(zero? part-count) (empty-rope sys)]
      [(= part-count 1) (vector-ref parts start)]
      [else
       (define middle (+ start (quotient part-count 2)))
       (join (build start middle)
             (build middle end))]))
  (build 0 (vector-length parts)))

(define (string->rope sys)
  (define make-leaf (leaf-rope sys))
  (lambda (text #:chunk-size [chunk-size 1024])
    (unless (string? text)
      (raise-argument-error 'string->rope "string?" text))
    (unless (exact-positive-integer? chunk-size)
      (raise-argument-error 'string->rope "exact-positive-integer?" chunk-size))
    (build-balanced sys
                    (map make-leaf (chunk-string text chunk-size)))))

(define (check-locator a-locator)
  (unless (locator? a-locator)
    (raise-argument-error 'split-rope "locator?" a-locator))
  (unless (procedure? (locator-inside? a-locator))
    (raise-argument-error 'split-rope "locator with an inside procedure" a-locator))
  (unless (procedure? (locator-split-leaf a-locator))
    (raise-argument-error 'split-rope "locator with a split-leaf procedure" a-locator))
  a-locator)

(define (split-rope sys)
  (define make-leaf (leaf-rope sys))
  (define join (concat-rope sys))
  (lambda (a-rope a-locator)
    (unless (rope? a-rope)
      (raise-argument-error 'split-rope "rope?" a-rope))
    (define checked-locator (check-locator a-locator))
    (define inside? (locator-inside? checked-locator))
    (define split-leaf (locator-split-leaf checked-locator))
    (define (inside-rope? before middle after)
      (inside? (rope-measure before)
               (rope-measure middle)
               (rope-measure after)))
    (let loop ([before (empty-rope sys)]
               [focus a-rope]
               [after (empty-rope sys)])
      (cond
        [(leaf? focus)
         (define-values (left-text right-text)
           (split-leaf (rope-measure before)
                       (leaf-text focus)
                       (rope-measure after)))
         (unless (and (string? left-text) (string? right-text))
           (error 'split-rope "locator split-leaf must return two strings"))
         (values (join before (make-leaf left-text))
                 (join (make-leaf right-text) after))]
        [(branch? focus)
         (define left (branch-left focus))
         (define right (branch-right focus))
         (define after-if-left (join right after))
         (define before-if-right (join before left))
         (if (inside-rope? before left after-if-left)
             (loop before left after-if-left)
             (loop before-if-right right after))]
        [else
         (error 'split-rope "invalid rope node: ~a" focus)]))))

(define (insert-rope sys)
  (define split-at (split-rope sys))
  (define join (concat-rope sys))
  (lambda (a-rope a-locator new-rope)
    (define-values (before after) (split-at a-rope a-locator))
    (join before (join new-rope after))))

(define (delete-rope sys)
  (define split-at (split-rope sys))
  (define join (concat-rope sys))
  (lambda (a-rope start-locator extent-locator)
    (define-values (before rest) (split-at a-rope start-locator))
    (define-values (_removed after) (split-at rest extent-locator))
    (join before after)))

(define (slice-rope sys)
  (define split-at (split-rope sys))
  (lambda (a-rope start-locator extent-locator)
    (define-values (_before rest) (split-at a-rope start-locator))
    (define-values (wanted _after) (split-at rest extent-locator))
    wanted))

(define (replace-rope sys)
  (define split-at (split-rope sys))
  (define join (concat-rope sys))
  (lambda (a-rope start-locator extent-locator replacement)
    (define-values (before rest) (split-at a-rope start-locator))
    (define-values (_removed after) (split-at rest extent-locator))
    (join before (join replacement after))))

(define (valid-rope? sys)
  (define leaf-measure-proc (measure-algebra-leaf-measure sys))
  (define branch-measure-proc (measure-algebra-branch-measure sys))
  (letrec ([valid?
            (lambda (a-rope)
              (cond
                [(leaf? a-rope)
                 (equal? (rope-measure a-rope)
                         (leaf-measure-proc (leaf-text a-rope)))]
                [(branch? a-rope)
                 (and (valid? (branch-left a-rope))
                      (valid? (branch-right a-rope))
                      (equal? (rope-measure a-rope)
                              (branch-measure-proc
                               (rope-measure (branch-left a-rope))
                               (rope-measure (branch-right a-rope)))))]
                [else #f]))])
    valid?))
