#lang scribble/manual
@(require scribble/example
          (for-label racket))

@(define ev (make-base-eval))
@examples[#:eval ev #:hidden (require "scribl/char-edit.rkt" racket/format)]

@title{Defining a summary system}

@margin-note{Section 2 of the @tt{summarised-rope} manual. Section 1 drives the
sexp system as shipped; here we build a new one. Section 3 is the internals.}

The rope and the zipper are @emph{guide-agnostic}: the rope caches whatever
summary you give it, and the cursor only ever @emph{calls} a guide, never naming
its kind. To make them do something you supply a summary and guides over it. This
section builds one end to end --- a @deftech{char-offset cursor}, where a position
is just a character offset --- then drives it. It is the sexp system in miniature,
and the whole thing rests on four small definitions.

@section{The summary}

A summary is built from a leaf measure and a way to combine. For char offsets the
measure of a string is its length, and lengths add:

@racketblock[
(define char-smr (make-summary string-length +))
]

That is the entire summary. @racket[make-summary] folds it over a rope, caching a
value at every node; the value here is a plain character count. It is a monoid by
inspection --- identity @racket[0] (the count of @racket[""]), combine @racket[+]
--- which is exactly what the rope relies on.

@section{The index is the guide}

A position is named by an @deftech{index}; a guide is a comparator @racket[(L R)
-> {-1 0 1}] that decides which side of a cut a target falls on (@racket[+1] = the
target is right of the cut). Rather than keep these as two things, we make the
index @emph{be} the guide: a struct carrying the index data, made callable with
@racket[prop:procedure].

@racketblock[
(define (cmp a b) (cond [(< a b) -1] [(> a b) 1] [else 0]))
(define (back? d) (negative? (car d)))
(define (guide-body d L R) (cmp (car d) (if (back? d) (- (add1 R)) L)))
(struct idx (data) #:property prop:procedure
  (lambda (self L R) (guide-body (idx-data self) L R)))
]

The index data is a head-led list --- for char offsets, just @racket[(list n)].
The head's sign is its @deftech{family}: a @deftech{front} index (@racket[>= 0])
counts from the left, a @deftech{back} index (@racket[<= -1]) from the right.
@racket[guide-body] compares the index's head against the cut's anchor on its
@emph{own} side: @racket[L] (chars to the left) for a front index, @racket[(-
(add1 R))] for a back one. Both name the same point now; under edits each holds to
its own side.

@section{The two anchors}

The two families' readings of a cut are its anchors. @racket[read-cut] hands back
both --- front @racket[(list L)], back @racket[(list (- (add1 R)))]:

@racketblock[
(define (read-cut L R) (values (list L) (list (- (add1 R)))))
]

The @racket[+1] is load-bearing: it puts the back family on @racket[<= -1] and the
front on @racket[>= 0], disjoint ranges, so a sign tells them apart with no
overlap at the document's ends. (Their difference, @racket[(+ L R 1)], is the
modulus --- the same @racket[+1] --- but covering never needs it.)

@section{Placement, covering, navigation}

Everything else is written straight on @racket[zipper-guide] (install/modify +
re-navigate) and @racket[on-edges] (read the cut at an edge). Placement starts a
fresh zipper with a two-vector of index-guides and navigates to them (one index =
a gap, two = a seg):

@racketblock[
(define (cursor rope s [e s])
  (let ([gs (vector (idx s) (idx e))]) ((zipper-guide gs) (start char-smr rope gs))))
]

@racket[cover] makes a cursor edit-stable: it reads the cut at the end edge and
re-anchors it onto the @tech{back} anchor, so the start holds the left boundary
and the end the right, and an edit between them stays wrapped.

@racketblock[
(define (edge-contexts z i)
  ((on-edges (lambda (e0 e1) (apply values (if (zero? i) e0 e1))) list list) z))
(define (anchors z i) (call-with-values (lambda () (edge-contexts z i)) read-cut))
(define (cover z)
  (define-values (front back) (anchors z 1))
  ((zipper-guide (lambda (gs) (vector (vector-ref gs 0) (idx back)))) z))
]

Navigation is just re-placement on the live zipper:

@racketblock[
(define (goto s [e s]) (lambda (z) ((zipper-guide (vector (idx s) (idx e))) z)))
]

That is the whole system: a summary, an index that is its own guide, the cut's
anchors, and three one-liners. Now we use it.

@section{Using it}

Build a rope under the summary, place a cursor, read the focus:

@examples[#:eval ev
  (define r ((make-rope char-smr) "the quick brown fox"))
  (cursor r '(4) '(9))
  (~a (zipper-focus (cursor r '(4) '(9))))]

The family chosen at placement decides which way an edit pushes the cursor. A
@tech{front} gap is pinned to the left, so an insert lands to its right; the
@tech{back} index for the same spot is pinned to the right, so an insert lands to
its left:

@examples[#:eval ev
  (define h ((make-rope char-smr) "hello world"))
  ((zipper-focus "big ") (cursor h '(6)))      (code:comment "front: insert to the right")
  ((zipper-focus "big ") (cursor h '(-6)))]    (code:comment "back:  insert to the left")

And a whole editing session, traced with @racket[chain] --- placement, @racket[cover],
edits, and @racket[goto] navigation interleaved, each row showing the command's
source beside the cursor it produced:

@examples[#:eval ev
  (chain (cursor r '(4) '(9)) cover
         (zipper-focus "nimble")
         (goto '(11) '(16)) cover
         (zipper-focus "red"))]

The covered seg keeps wrapping its contents as they change, and @racket[goto]
re-places the cursor between edits --- the same vocabulary as Section 1, on a
system we built from four definitions.

@section{The laws}

A summary must be lawful for the rope to fold it safely: the combine is an
associative monoid with @racket[(string-summary "")] as identity, and the leaf
measure a homomorphism from text concatenation. @racket[char-smr] is all three by
construction --- @racket[(natural, +, 0)] with @racket[string-length] --- so it
passes the conformance battery in @tt{summaries/summary-laws.rkt} that Section 3 describes.
A summary that fails a law (say @racket[max] for combine) is where guided
navigation silently goes wrong, which is why the battery exists.
