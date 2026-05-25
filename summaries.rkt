#lang racket

(require "core.rkt")

(provide
 editor-algebra
 vowel-algebra
 sexp-path-algebra
 parens-balanced?
 char-position
 position-from-end
 line-column
 sexp-path)

(define (utf8-byte-count text)
  (bytes-length (string->bytes/utf-8 text)))

(define (split-at-characters text count)
  (define split-index
    (max 0 (min count (string-length text))))
  (values (substring text 0 split-index)
          (substring text split-index)))

(define (word-char? ch)
  (or (char-alphabetic? ch)
      (char-numeric? ch)))

(define (vowel? ch)
  (not (false? (member ch '(#\a #\e #\i #\o #\u
                            #\A #\E #\I #\O #\U)))))

(define (measure-ref a-measure key)
  (hash-ref a-measure key))

(define (editor-leaf-measure text)
  (let loop ([index 0]
             [chars 0]
             [newlines 0]
             [current-line-chars 0]
             [first-line-chars #f]
             [words 0]
             [in-word? #f]
             [starts-word? #f]
             [saw-first-char? #f]
             [ends-word? #f]
             [vowels 0]
             [paren-depth 0]
             [paren-min 0]
             [paren-max 0])
    (if (= index (string-length text))
        (hash 'bytes (utf8-byte-count text)
              'chars chars
              'newlines newlines
              'first-line-chars (or first-line-chars current-line-chars)
              'last-line-chars current-line-chars
              'words words
              'starts-word? (and starts-word? #t)
              'ends-word? ends-word?
              'vowels vowels
              'paren-net paren-depth
              'paren-min paren-min
              'paren-max paren-max)
        (let* ([ch (string-ref text index)]
               [newline? (char=? ch #\newline)]
               [word? (word-char? ch)]
               [next-starts-word?
                (if saw-first-char? starts-word? word?)]
               [next-words
                (if (and word? (not in-word?)) (add1 words) words)]
               [next-vowels
                (if (vowel? ch) (add1 vowels) vowels)]
               [next-depth
                (cond
                  [(char=? ch #\() (add1 paren-depth)]
                  [(char=? ch #\)) (sub1 paren-depth)]
                  [else paren-depth])])
          (loop (add1 index)
                (add1 chars)
                (if newline? (add1 newlines) newlines)
                (if newline? 0 (add1 current-line-chars))
                (if (and newline? (not first-line-chars))
                    current-line-chars
                    first-line-chars)
                next-words
                word?
                next-starts-word?
                #t
                word?
                next-vowels
                next-depth
                (min paren-min next-depth)
                (max paren-max next-depth))))))

(define (editor-branch-measure left right)
  (define left-empty? (zero? (measure-ref left 'chars)))
  (define right-empty? (zero? (measure-ref right 'chars)))
  (define first-line-chars
    (if (positive? (measure-ref left 'newlines))
        (measure-ref left 'first-line-chars)
        (+ (measure-ref left 'chars)
           (measure-ref right 'first-line-chars))))
  (define last-line-chars
    (if (positive? (measure-ref right 'newlines))
        (measure-ref right 'last-line-chars)
        (+ (measure-ref left 'last-line-chars)
           (measure-ref right 'chars))))
  (define boundary-word?
    (and (measure-ref left 'ends-word?)
         (measure-ref right 'starts-word?)))
  (define words
    (- (+ (measure-ref left 'words)
          (measure-ref right 'words))
       (if boundary-word? 1 0)))
  (define starts-word?
    (if left-empty?
        (measure-ref right 'starts-word?)
        (measure-ref left 'starts-word?)))
  (define ends-word?
    (if right-empty?
        (measure-ref left 'ends-word?)
        (measure-ref right 'ends-word?)))
  (define paren-net
    (+ (measure-ref left 'paren-net)
       (measure-ref right 'paren-net)))
  (define paren-min
    (min (measure-ref left 'paren-min)
         (+ (measure-ref left 'paren-net)
            (measure-ref right 'paren-min))))
  (define paren-max
    (max (measure-ref left 'paren-max)
         (+ (measure-ref left 'paren-net)
            (measure-ref right 'paren-max))))
  (hash 'bytes (+ (measure-ref left 'bytes)
                  (measure-ref right 'bytes))
        'chars (+ (measure-ref left 'chars)
                  (measure-ref right 'chars))
        'newlines (+ (measure-ref left 'newlines)
                     (measure-ref right 'newlines))
        'first-line-chars first-line-chars
        'last-line-chars last-line-chars
        'words words
        'starts-word? starts-word?
        'ends-word? ends-word?
        'vowels (+ (measure-ref left 'vowels)
                   (measure-ref right 'vowels))
        'paren-net paren-net
        'paren-min paren-min
        'paren-max paren-max))

(define editor-algebra
  (measure-algebra editor-leaf-measure editor-branch-measure))

(define (parens-balanced? a-measure)
  (and (zero? (measure-ref a-measure 'paren-net))
       (not (negative? (measure-ref a-measure 'paren-min)))))

(define (char-position count)
  (unless (exact-nonnegative-integer? count)
    (raise-argument-error 'char-position "exact-nonnegative-integer?" count))
  (locator
   (lambda (before middle _after)
     (define start (measure-ref before 'chars))
     (define end (+ start (measure-ref middle 'chars)))
     (<= start count end))
   (lambda (before text _after)
     (split-at-characters text
                          (- count (measure-ref before 'chars))))))

(define (position-from-end count)
  (unless (exact-nonnegative-integer? count)
    (raise-argument-error 'position-from-end "exact-nonnegative-integer?" count))
  (locator
   (lambda (_before middle after)
     (define end (measure-ref after 'chars))
     (define start (+ (measure-ref middle 'chars) end))
     (<= end count start))
   (lambda (_before text after)
     (define right-chars (- count (measure-ref after 'chars)))
     (define left-chars (- (string-length text) right-chars))
     (split-at-characters text left-chars))))

(define (line-column-inside? line column before middle)
  (define start-line (measure-ref before 'newlines))
  (define start-column (measure-ref before 'last-line-chars))
  (define middle-lines (measure-ref middle 'newlines))
  (define end-line (+ start-line middle-lines))
  (define end-column
    (if (zero? middle-lines)
        (+ start-column (measure-ref middle 'chars))
        (measure-ref middle 'last-line-chars)))
  (define first-line-end-column
    (+ start-column (measure-ref middle 'first-line-chars)))
  (cond
    [(< line start-line) #f]
    [(> line end-line) #f]
    [(= start-line end-line line)
     (<= start-column column end-column)]
    [(= line start-line)
     (<= start-column column first-line-end-column)]
    [(= line end-line)
     (<= 0 column end-column)]
    [else #t]))

(define (split-index-at-line-column before text target-line target-column)
  (let loop ([index 0]
             [line (measure-ref before 'newlines)]
             [column (measure-ref before 'last-line-chars)])
    (cond
      [(>= index (string-length text)) index]
      [(> line target-line) index]
      [(and (= line target-line) (>= column target-column)) index]
      [else
       (define ch (string-ref text index))
       (if (char=? ch #\newline)
           (if (= line target-line)
               index
               (loop (add1 index) (add1 line) 0))
           (loop (add1 index) line (add1 column)))])))

(define (line-column line column)
  (unless (exact-nonnegative-integer? line)
    (raise-argument-error 'line-column "exact-nonnegative-integer?" line))
  (unless (exact-nonnegative-integer? column)
    (raise-argument-error 'line-column "exact-nonnegative-integer?" column))
  (locator
   (lambda (before middle _after)
     (line-column-inside? line column before middle))
   (lambda (before text _after)
     (define index
       (split-index-at-line-column before text line column))
     (values (substring text 0 index)
             (substring text index)))))

(define (count-vowels text)
  (for/sum ([ch (in-string text)]
            #:when (vowel? ch))
    1))

(define vowel-algebra
  (measure-algebra count-vowels +))

;; A S-expression measure is a tree of local parser events for one rope segment.
;;
;; A leaf measure holds a vector of events derived only from that leaf's text:
;; 'open, 'close, 'datum, 'space, 'quote, 'escape, and comment/newline events.
;;
;; A branch measure holds the left and right child measures in text order.
;; Combining branches this way is cheap: we keep the event tree instead of
;; flattening every child event vector whenever two rope segments are joined.
;; Locators traverse this measure tree when they need structural context.
(struct sexp-leaf-events (events) #:transparent)
(struct sexp-branch-events (left right) #:transparent)

(struct sexp-frame (path next-child) #:transparent)
(struct sexp-scan (frames mode escaped? target found?) #:transparent)

(define (valid-sexp-path? path)
  (and (list? path)
       (pair? path)
       (andmap exact-nonnegative-integer? path)))

(define (sexp-start state)
  (define current-frame (first (sexp-scan-frames state)))
  (define path
    (append (sexp-frame-path current-frame)
            (list (sexp-frame-next-child current-frame))))
  (define next-frame
    (struct-copy sexp-frame current-frame
                 [next-child (add1 (sexp-frame-next-child current-frame))]))
  (values
   path
   (struct-copy sexp-scan state
                [frames (cons next-frame
                              (rest (sexp-scan-frames state)))]
                [found? (or (sexp-scan-found? state)
                            (equal? path (sexp-scan-target state)))])))

(define (sexp-open-list state)
  (define-values (path next-state) (sexp-start state))
  (struct-copy sexp-scan next-state
               [frames (cons (sexp-frame path 0)
                             (sexp-scan-frames next-state))]
               [mode 'gap]
               [escaped? #f]))

(define (sexp-close-list state)
  (define frames (sexp-scan-frames state))
  (struct-copy sexp-scan state
               [frames (if (pair? (rest frames))
                           (rest frames)
                           frames)]
               [mode 'gap]
               [escaped? #f]))

(define (sexp-start-atom state)
  (define-values (_path next-state) (sexp-start state))
  (struct-copy sexp-scan next-state
               [mode 'atom]
               [escaped? #f]))

(define (sexp-start-string state)
  (define-values (_path next-state) (sexp-start state))
  (struct-copy sexp-scan next-state
               [mode 'string]
               [escaped? #f]))

(define (sexp-event ch)
  (cond
    [(char=? ch #\newline) 'newline]
    [(char-whitespace? ch) 'space]
    [(char=? ch #\;) 'line-comment]
    [(char=? ch #\() 'open]
    [(char=? ch #\)) 'close]
    [(char=? ch #\") 'quote]
    [(char=? ch #\\) 'escape]
    [else 'datum]))

(define (advance-sexp-scan state event)
  (case (sexp-scan-mode state)
    [(line-comment)
     (if (eq? event 'newline)
         (struct-copy sexp-scan state [mode 'gap])
         state)]
    [(string)
     (cond
       [(sexp-scan-escaped? state)
        (struct-copy sexp-scan state [escaped? #f])]
       [(eq? event 'escape)
        (struct-copy sexp-scan state [escaped? #t])]
       [(eq? event 'quote)
        (struct-copy sexp-scan state [mode 'gap])]
       [else state])]
    [(atom)
     (cond
       [(or (eq? event 'space)
            (eq? event 'newline))
        (struct-copy sexp-scan state [mode 'gap])]
       [(eq? event 'line-comment)
        (struct-copy sexp-scan state [mode 'line-comment])]
       [(eq? event 'open)
        (sexp-open-list (struct-copy sexp-scan state [mode 'gap]))]
       [(eq? event 'close)
        (sexp-close-list state)]
       [else state])]
    [else
     (cond
       [(or (eq? event 'space)
            (eq? event 'newline))
        state]
       [(eq? event 'line-comment)
        (struct-copy sexp-scan state [mode 'line-comment])]
       [(eq? event 'open)
        (sexp-open-list state)]
       [(eq? event 'close)
        (sexp-close-list state)]
       [(eq? event 'quote)
        (sexp-start-string state)]
       [else
        (sexp-start-atom state)])]))

(define (scan-sexp-events state events)
  (for/fold ([next-state state])
            ([event (in-vector events)])
    (advance-sexp-scan next-state event)))

(define (scan-sexp-measure state a-measure)
  (cond
    [(sexp-leaf-events? a-measure)
     (scan-sexp-events state (sexp-leaf-events-events a-measure))]
    [(sexp-branch-events? a-measure)
     (scan-sexp-measure
      (scan-sexp-measure state (sexp-branch-events-left a-measure))
      (sexp-branch-events-right a-measure))]
    [else
     (raise-argument-error 'scan-sexp-measure
                           "(or/c sexp-leaf-events? sexp-branch-events?)"
                           a-measure)]))

(define (sexp-leaf-measure text)
  ;; This measure describes only this leaf's text. It does not know what came
  ;; before the leaf; the locator supplies that context during descent.
  (define local-events
    (for/vector ([ch (in-string text)])
      (sexp-event ch)))
  (sexp-leaf-events local-events))

(define (sexp-branch-measure left right)
  ;; The branch's segment is the left segment followed by the right segment, so
  ;; its measure is the same ordering of the already-computed child measures.
  (define left-events left)
  (define right-events right)
  (sexp-branch-events left-events right-events))

(define sexp-path-algebra
  (measure-algebra sexp-leaf-measure sexp-branch-measure))

(define (initial-sexp-scan path)
  (sexp-scan (list (sexp-frame '() 0)) 'gap #f path #f))

(define (sexp-split-index before text path)
  (define before-scan
    (scan-sexp-measure (initial-sexp-scan path) before))
  (cond
    [(sexp-scan-found? before-scan) 0]
    [else
     (let loop ([index 0]
                [scan before-scan])
       (cond
         [(= index (string-length text)) #f]
         [else
          (define next-scan
            (advance-sexp-scan scan
                               (sexp-event (string-ref text index))))
          (if (and (not (sexp-scan-found? scan))
                   (sexp-scan-found? next-scan))
              index
              (loop (add1 index) next-scan))]))]))

(define (sexp-path path)
  (unless (valid-sexp-path? path)
    (raise-argument-error 'sexp-path
                          "non-empty list of exact nonnegative integers"
                          path))
  (locator
   (lambda (before middle _after)
     (define before-scan
       (scan-sexp-measure (initial-sexp-scan path) before))
     (define middle-scan
       (scan-sexp-measure before-scan middle))
     (and (not (sexp-scan-found? before-scan))
          (sexp-scan-found? middle-scan)))
   (lambda (before text _after)
     (define index
       (sexp-split-index before text path))
     (unless index
       (error 'sexp-path "could not find S-expression path: ~a" path))
     (values (substring text 0 index)
             (substring text index)))))
