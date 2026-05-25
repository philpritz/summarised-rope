# measured-rope for Racket

`measured-rope` is an immutable rope that caches user-defined measures at each
node. Leaves measure text. Branches combine their children's measures. Locators
read the measures before, inside, and after a candidate subtree to decide where
a split belongs.

## Count measures

Each measure algebra describes one measure. Character count is a tiny one:

```racket
(define chars
  (measure-algebra
   string-length
   +))
```

With a character locator, that measure can drive an edit:

```racket
(define sys (system chars))
(define from-string (string->rope sys))
(define insert-at (insert-rope sys))

(define r (from-string "measure ropes"))

(define r2
  (insert-at r
             (character 7)
             (from-string "d")))

(rope->string r2)
;; "measured ropes"
```

A measure algebra also selects its own component from that bundle. A locator
carries that selector:

```racket
(struct locator (measure inside? split-leaf) #:transparent)
```

`character` selects character counts:

```racket
(define (character n)
  (locator
   chars
   (lambda (before middle _after)
     (<= before n (+ before middle)))
   (lambda (before text _after)
     (define local-index
       (max 0 (min (string-length text) (- n before))))
     (values (substring text 0 local-index)
             (substring text local-index)))))
```

Before calling the two locator functions, the rope applies `chars` to the full
`before`, `middle`, and `after` bundles. Those functions therefore receive
plain character counts.

The first locator function is used while `split` walks down the rope. At each
candidate branch it asks whether the split point lies in one child:

```text
before | middle | after
```

`before` is the measure of all text before that candidate child. `middle` is
the candidate child's measure. `after` is everything after it. For a character
locator those selected measures are just counts, so:

```racket
(<= before n (+ before middle))
```

means:

```text
does character boundary n lie between the start and end of middle?
```

If it does, `split` descends into that child. Otherwise it descends into the
other child with updated `before` and `after` measures. The second locator
function runs only when descent reaches a leaf; it splits the leaf text at the
exact local position.

The rope does not know what a character means. The system combines measure
algebras into the node measure bundle; each locator selects the one measure it
needs and decides where to descend.

## A custom measure

Suppose a rope stores:

```racket
(define square
  (lambda (x)
    (* x x)))
```

We want to locate S-expressions by path:

```text
'(0)    the first top-level S-expression
'(0 0)  `define` inside it
'(0 1)  `square` inside it
'(0 2)  the `(lambda ...)` inside it
```

To extract one of those sibling S-expressions, slice from its start to the
end:

```racket
(define slice-at (slice-rope sys))

(rope->string
 (slice-at r
           (sexp-start '(0 0))
           (sexp-end '(0 0))))
;; "define"

(rope->string
 (slice-at r
           (sexp-start '(0 1))
           (sexp-end '(0 1))))
;; "square"

(rope->string
 (slice-at r
           (sexp-start '(0 2))
           (sexp-end '(0 2))))
;; "(lambda (x)\n    (* x x)))"
```

To support that without flattening the rope, we need a measure that describes
unfinished S-expression structure at the two edges of each segment.

For a simple parenthesized language with atoms and whitespace, one compact
shape is:

```racket
(struct sexp-measure
  (closes        ; unmatched `)` counts, left to right
   forms         ; complete top-level S-expressions in the segment
   opens         ; unmatched `(` counts, outer to inner
   starts-atom?  ; the segment begins with atom text
   ends-atom?)   ; the segment ends with atom text
  #:transparent)
```

`opens` and `closes` are lists of child counts. Atoms count as
S-expressions. A complete list also counts as one S-expression at its parent
level.

### Measure examples

This segment ends inside one open list:

```racket
"(define "
=> (sexp-measure '() 0 '(1) #f #f)
```

The open list has already started one child, `define`.

This segment ends inside nested open lists:

```racket
"(define () (square"
=> (sexp-measure '() 0 '(3 1) #f #t)
```

Read `'(3 1)` from outer to inner:

```text
outer list: define, (), and (square ... have started
inner list: square has started
```

So the still-open inner list is the third child of the first top-level
S-expression.

The close side records the opposite frontier:

```racket
" x))"
=> (sexp-measure '(1 0) 0 '() #f #f)
```

Read `'(1 0)` left to right:

```text
before the first unmatched `)`: x contributes one child
before the second unmatched `)`: nothing else is added
```

The atom flags are there for rope boundaries such as:

```text
"def" | "ine"
```

Those two pieces should count as one atom after their measures combine.

### Combining measures

Combine:

```racket
"(define () (square"
=> (sexp-measure '() 0 '(3 1) #f #t)

" x))"
=> (sexp-measure '(1 0) 0 '() #f #f)
```

The innermost open and first close match:

```text
(square        has already started 1 child: square
 x)            adds 1 child before closing: x
```

That closes `(square x)`. The next close then closes the outer list. The
combined segment is one complete top-level S-expression:

```racket
"(define () (square x))"
=> (sexp-measure '() 1 '() #f #f)
```

Another combination leaves complete forms beside the closed list:

```racket
"one (two"
=> (sexp-measure '() 1 '(1) #t #t)

" three) four"
=> (sexp-measure '(1) 1 '() #f #t)

"one (two three) four"
=> (sexp-measure '() 3 '() #t #t)
```

The branch operation does exactly this kind of frontier matching. It combines
the opens on the left with the closes on the right, then keeps any complete
forms and unmatched frontier counts that remain.

## Locating with it

```racket
#lang racket

(require "core.rkt"
         "measures.rkt")

(define sys (system sexp-path-algebra))
(define from-string (string->rope sys))
(define split-at (split-rope sys))

(define r
  (from-string "(define square\n  (lambda (x)\n    (* x x)))"))

(define-values (before at-square)
  (split-at r (sexp-start '(0 1))))

(map rope->string (list before at-square))
;; '("(define " "square\n  (lambda (x)\n    (* x x)))")
```

Paths are zero-based. The first `0` picks the first top-level S-expression. The
next `1` picks the second S-expression inside it, so `'(0 1)` lands at the
beginning of `square`.

The S-expression measure and its locators live in
[`measures.rkt`](./measures.rkt). Start at `sexp-path-algebra` for the measure
algebra and `sexp-path` for the current locator implementation.

## Shape

- `core.rkt` defines ropes, measure algebras, locators, split, and edits.
- `measures.rkt` provides example text measures and locators.
- `tests/core-test.rkt` exercises roundtrips, splitting, edits, and measures.

## Current measures

The S-expression frontier measure above is the motivating structural example:
it is small enough to combine at rope nodes and rich enough to navigate by
paths like `'(0 1)`. `editor-algebra` is a simpler example already in the
project: it stores a hash with character count, UTF-8 byte count, newline and
line-column data, word count, vowel count, and parenthesis balance data. A
locator reads only the measure it needs. For example, `char-position` looks at
`'chars`; `line-column` looks at line measures.

## Run tests

From this folder:

```powershell
& "C:\Program Files\Racket\raco.exe" test .\tests\core-test.rkt
```

This is still a small first version. It intentionally leaves balancing for the
future; the editing API already builds on `split-rope` and `concat-rope`.
