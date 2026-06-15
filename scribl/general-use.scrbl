#lang scribble/manual
@(require scribble/example
          (for-label racket))

@(define ev (make-base-eval))
@examples[#:eval ev #:hidden (require "sexp-edit.rkt" racket/format)]

@title{Editing s-expressions}

@margin-note{Section 1 of the @tt{summarised-rope} manual. Sections 2 (defining
custom guides and summaries) and 3 (internals) follow.}

This section drives the sexp rope as it ships: a summary algebra
(@racket[sexp-smr]) and an editing surface (@filepath{sexp-edit.rkt}) already
built over it. You supply a document and a sequence of cursor
operations; the goal here is everything you need to write such a sequence and
have the cursor land where you mean across every edit. One document carries the
whole section:

@examples[#:eval ev
  (define r ((make-rope sexp-smr) "(aa (p q) cc)"))
  r]

A rope is persistent text that prints as itself --- @racket[(make-rope sexp-smr)]
chunks and balances the string, and every node caches its sexp summary. Nothing
about editing mutates @racket[r]; each operation returns a new rope.

@section{The cursor and how it reads}

A @deftech{cursor} is a zipper carrying a focus and the guides that place it. It
prints as the document with the focus marked --- a caret @litchar{‸} for an
empty focus (a @deftech{gap}, a single point) and brackets @litchar{⟦}@litchar{⟧}
for a non-empty one (a @deftech{seg}, an interval):

@examples[#:eval ev
  (cursor r '(1 1 0))]

@racket[(zipper-focus z)] reads the focused rope; at a gap it is empty:

@examples[#:eval ev
  (define z (cursor r '(1 1 0)))
  (~a (zipper-focus z))]

@section{Addressing: spines}

A position is named by a @deftech{spine}: the list of slot numbers, one per
nesting level, @emph{innermost first}, each slot @emph{0-based}. Read
@racket['(1 1 0)] off the document from the inside out:

@verbatim|{
  (aa (p q) cc)
       ^q           slot 1 inside (p q)       -> 1   (p is 0, q is 1)
      (p q)         slot 1 of the outer form  -> 1   (aa is 0, (p q) is 1)
  (aa (p q) cc)     slot 0 of the document    -> 0
                                          spine = (1 1 0)
}|

So @racket['(1 1 0)] is the cut just before @racket[q]. A frame's @emph{end
slot} --- the point just before its @litchar{)} --- is a real target too: slot
@racket[2] of @racket[(p q)] is @racket['(2 1 0)], and slot @racket[2] of the
whole form is @racket['(2 0)], the cut before @racket[cc]. You hand-write a spine
to @emph{place} a cursor; you will not hand-write the right-anchored variants
below --- the cursor verbs produce those for you.

@section{Reading, then editing}

All content editing goes through @racket[zipper-focus]: hand it a string and it
replaces the focus, the empty string deletes, and a function transforms. At a
gap, a replace is an insert. The one rule that makes a sequence predictable:
@bold{every write re-navigates}. After a swap the cursor re-runs its guides on
the @emph{new} text and lands where they now point.

@examples[#:eval ev
  ((zipper-focus "x ") z)]

The insert worked --- the document is now @tt{(aa (p x q) cc)} --- but watch the
caret: it sits before @racket[x], not before @racket[q]. Insert again and the
slip compounds:

@examples[#:eval ev
  ((zipper-focus "y ") ((zipper-focus "x ") z))]

Each insert lands before the last. The cursor is tracking @emph{slot 1 counted
from the left}, and every insert changes what slot 1 is --- so it no longer
tracks the spot you cared about. This is the problem flipping solves.

@section{Why flip: families and stability}

A spine names its position by counting from one side, and that side is its
@deftech{family}. A @deftech{front} index counts forms from the text to its
@emph{left}; a @deftech{back} index counts from the text to its @emph{right}.
Both name the same point on the present text, but under edits each holds to its
own side: a front index keeps its distance from the left context, a back index
from the right. @racket[cursor] installs @emph{front} edges, which is why the gap
above counts from the left and drifts when an insert shifts that count.

@racket[cover] fixes a cursor in place by flipping its trailing edge onto its
@tech{back} anchor: the start then holds the left boundary, the end the right,
and whatever you put between them stays wrapped. The same gap, covered:

@examples[#:eval ev
  (define c (cover (cursor r '(1 1 0))))
  c]

Same spot --- but now an insert grows the cursor to @emph{cover} what you typed,
and the next write replaces that focus rather than landing beside it:

@examples[#:eval ev
  (define c1 ((zipper-focus "x ") c))     (code:comment "insert: cursor covers it")
  c1
  (define c2 ((zipper-focus "x y ") c1))  (code:comment "replace the covered focus")
  c2
  ((zipper-focus "") c2)]                 (code:comment "delete: back to the gap")

Across insert, grow, and clear, @racket[q] never moves --- the right edge is
anchored to it. @racket[cover] is the everyday recipe; underneath it,
@racket[anchors] reads both families at an edge, @racket[flip] swaps a spine
between them, and @racket[re-anchor] installs the swap. The arithmetic relating
the two families is in the internals section.

@section{Moving the cursor}

Two verbs reposition without editing. @racket[move] jumps to a gap at a whole new
spine --- it can change level, since it replaces the entire index:

@examples[#:eval ev
  ((move (lambda (ix) '(2 0))) z)]

@racket[spread] nudges each edge's innermost slot, leaving the path intact, so
the cursor stays in the same sexp. Widening the trailing edge by one turns the
gap before @racket[q] into a seg over it:

@examples[#:eval ev
  ((spread values add1) z)]

@section{A sequence, start to finish}

@racket[chain] threads a zipper through a list of commands and prints each
command beside the cursor it produces --- an editing sequence made legible:

@examples[#:eval ev
  (chain (cover (cursor r '(1 1 0)))
         (zipper-focus "x ")
         (zipper-focus "x y ")
         (zipper-focus ""))]
