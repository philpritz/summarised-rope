#lang scribble/manual

@(require scribble/example
          racket/sandbox
          racket/runtime-path
          (for-label racket
                     "../rope-core.rkt"
                     "../summaries/sexp-summary.rkt"
                     "../zipper-core.rkt"
                     "../text-edit/sexp-edit.rkt"))

@; A trusted sandbox so the example evaluator may require the local modules (the
@; default sandbox blocks reading files outside collects). rope-core supplies
@; make-rope; sexp-summary the sexp algebra and sand-spines; zipper-core the
@; cursor surface; sexp-edit the layer documented here.
@(define-runtime-path rope-core-path "../rope-core.rkt")
@(define-runtime-path sexp-summary-path "../summaries/sexp-summary.rkt")
@(define-runtime-path zipper-core-path "../zipper-core.rkt")
@(define-runtime-path sexp-edit-path "../text-edit/sexp-edit.rkt")
@(define ev
   (call-with-trusted-sandbox-configuration
    (lambda ()
      (define e (make-base-eval))
      (e `(require racket/format
                   (file ,(path->string rope-core-path))
                   (file ,(path->string sexp-summary-path))
                   (file ,(path->string zipper-core-path))
                   (file ,(path->string sexp-edit-path))))
      e)))

@; Shared setup for the examples: a small rope and the two indexes ^bb / ^cc, read
@; straight off string cuts the way the test suite does.
@(ev
  '(begin
     (define rope ((make-rope sexp-smr) "(aa bb cc)"))
     (define (sides str i)
       (values (sexp-smr (substring str 0 i)) (sexp-smr (substring str i))))
     (define (front-of str i)
       (let-values ([(L R) (sides str i)])
         (let-values ([(f b) (sand-spines L R)]) f)))
     (define ^bb (front-of "(aa bb cc)" 4))
     (define ^cc (front-of "(aa bb cc)" 7))
     (define (doc z) (~a ((viewer zipper-focus) (to-root z))))))

@title{sexp-edit}

S-expression navigation and editing on the summarised rope, as a layer over
@racket[zipper-core]. The public surface: the comparison @racket[spine-cmp]; the
guide @racket[slot-guide] / @racket[guide-index]; the re-basing @racket[modulus]
@racket[base-left] @racket[base-right] @racket[flip]; the cursor conveniences
@racket[cursor] @racket[sexp-guides] @racket[anchors] @racket[re-anchor]
@racket[cover] and the edge lenses @racket[edge-guide] / @racket[edge-index]; and
the edit verbs @racket[at] @racket[move] @racket[edge] @racket[each] @racket[both].
Everything that isn't a comparison over the spines @racket[sand-spines] reads
(@tt{summaries/sexp-summary.rkt}) is @racket[zipper-core].

@section{spine-cmp}

@defproc[(spine-cmp [front (listof real?)] [back (listof real?)] [index (listof real?)])
         (or/c -1 0 1)]{
Compares an @racket[index] against the cut named by the @racket[front]/@racket[back]
spine pair @racket[sand-spines] reads. Returns @racket[1] if the index's boundary lies
@emph{right} of the cut, @racket[-1] if left, @racket[0] if it @emph{is} the cut.
A single @racket[spine-cmp] against a fixed cut is one guide's verdict; sweeping it
over every cut yields the guide.

@examples[#:eval ev
  (define-values (L R) (sides "(aa bb cc)" 4))
  (define-values (front back) (sand-spines L R))
  (code:line (spine-cmp front back (front-of "(aa bb cc)" 1)) (code:comment "^aa: left of ^bb"))
  (code:line (spine-cmp front back ^bb)                       (code:comment "^bb: is the cut"))
  (code:line (spine-cmp front back ^cc)                       (code:comment "^cc: right of ^bb"))]

The comparison is lexicographic, outermost-first; see @secref["the-comparison"].

See also @racket[slot-guide].
}

@section{slot-guide and guide-index}

@defproc[(slot-guide [index (listof real?)]) guide?]{
Builds the guide for one @racket[index] --- a comparator @racket[(L R)] that runs
@racket[spine-cmp] against the cut @racket[sand-spines] reads at @racket[(L R)]. The
result is @emph{both} callable as that comparator and readable as its index (via
@racket[guide-index]), so the edit verbs reach the indices without re-deriving them
from the zipper.

@examples[#:eval ev
  (define g (slot-guide ^bb))
  (code:line (guide-index g)              (code:comment "readable as its index"))
  (define-values (L R) (sides "(aa bb cc)" 4))
  (code:line (g L R)                      (code:comment "callable: 0 at its own cut"))]

See also @racket[spine-cmp], @racket[cursor].
}

@defproc[(guide-index [g guide?]) (listof real?)]{
The index a @racket[slot-guide] carries.
}

@section{cursor and sexp-guides}

@defproc[(cursor [rope rope?] [s (listof real?)] [e (listof real?) s]) zipper?]{
Places a cursor on @racket[rope] at the indexes @racket[s] (start) and @racket[e]
(end), navigated to. With one index it is a @emph{gap} (an empty focus, a caret);
with two it is a @emph{segment} spanning the half-open span @tt{[s, e)}.

@examples[#:eval ev
  (code:line (cursor rope ^bb)        (code:comment "a gap before bb"))
  (code:line (cursor rope ^bb ^cc)    (code:comment "the segment [^bb ^cc)"))
  (code:line (doc ((setter zipper-focus "xx ") (cursor rope ^bb)))
             (code:comment "replace at a gap = insert"))
  (code:line (doc ((setter zipper-focus "XX ") (cursor rope ^bb ^cc)))
             (code:comment "replace the segment"))]

See also @racket[sexp-guides], @racket[anchors].
}

@defproc[(sexp-guides [s (listof real?)] [e (listof real?) s]) (list/c guide? guide?)]{
The two guides @racket[(list (slot-guide s) (slot-guide e))] --- the cursor pair
@racket[cursor] installs.
}

@section{anchors}

@defproc[(anchors [z zipper?] [i (or/c 0 1)]) any]{
Reads both anchor indexes of edge @racket[i] (@racket[0] = start, @racket[1] = end)
of @racket[z]'s focus, as two values. The two name the same position now but differ
in the @bold{head} only --- the path down to the cut is one left-based name shared by
both, and the head is anchored left (off the text before) or right (off the text
after). Under edits within the frame each head follows its own side.

@examples[#:eval ev
  (code:line (call-with-values (lambda () (anchors (cursor rope ^bb) 0)) list)
             (code:comment "front (1 0) and back (-3 0): same path, two heads"))]

See also @racket[modulus], @racket[re-anchor].
}

@section{modulus, base-left, base-right, flip}

@defproc[(modulus [L any/c] [R any/c]) exact-integer?]{
The re-basing constant of the cut @racket[sand-spines] reads at @racket[(L R)]:
front head minus back head, which is @racket[N+1] at the cut's own level for a frame
of @racket[N] forms. Only the head re-bases between the two anchorings, so this one
number is the whole of the flip data --- exactly what one index alone cannot know.

@examples[#:eval ev
  (code:line (let-values ([(L R) (sides "(aa bb cc)" 4)]) (modulus L R))
             (code:comment "3 forms in the frame, +1"))]
}

@defproc[(base-left [m exact-integer?]) (-> (listof real?) (listof real?))]{
Re-bases an index's head to the left-anchoring, given its cut's @racket[modulus]
@racket[m]. A back head (@racket[< -½]) shifts up by @racket[m]; a front head is
already left-based and passes through.
}

@defproc[(base-right [m exact-integer?]) (-> (listof real?) (listof real?))]{
The mirror: re-bases the head to the right-anchoring. A front head shifts down by
@racket[m]; a back head passes through.
}

@defproc[(flip [m exact-integer?]) (-> (listof real?) (listof real?))]{
The other anchoring of the same position --- @racket[base-right] on a front head,
@racket[base-left] on a back head. An involution: flipping twice returns the index.

@examples[#:eval ev
  (code:line ((flip 4) ^bb)             (code:comment "(1 0) -> (-3 0): right head, path untouched"))
  (code:line ((flip 4) ((flip 4) ^bb))  (code:comment "an involution"))]

See also @racket[anchors], @racket[modulus].
}

@section{re-anchor and cover}

@defproc[(re-anchor [z zipper?] [i (or/c 0 1)] [side (or/c 'front 'back)]) zipper?]{
Re-installs edge @racket[i]'s guide from the chosen @racket[side]'s anchor --- the
anchor flip as a cursor operation. Same position now; the family (@racket['front] |
@racket['back]) decides how the edge follows future edits.
}

@defproc[(cover [z zipper?]) zipper?]{
Re-anchors the @emph{end} edge onto its right side (the start already reads its left),
so an edit between the edges touches neither anchor's side and the cursor keeps
covering whatever replaces the focus.

@examples[#:eval ev
  (define zc (cover (cursor rope ^bb ^cc)))
  (code:line (~a ((viewer zipper-focus) zc))          (code:comment "the segment, covered"))
  (code:line (doc ((setter zipper-focus "b1 (b2 b3) ") zc))
             (code:comment "the cursor still wraps the replacement"))]

See also @racket[re-anchor].
}

@section{edge-guide and edge-index}

@defproc[(edge-guide [i (or/c 0 1)]) lens?]{
The lens onto edge @racket[i]'s guide --- @racket[zipper-core]'s @racket[zipper-edge].
}

@defproc[(edge-index [i (or/c 0 1)]) lens?]{
The lens one hop further, onto edge @racket[i]'s @emph{index} (the guide composed
with @racket[index-of], a guide @tt{<->} its index). A write through it re-derives the
guide and re-navigates once.
}

@section{The edit verbs}

@deftogether[(@defproc[(at   [ix (listof real?)]) (-> zipper? zipper?)]
              @defproc[(move [f (-> (listof real?) (listof real?))]) (-> zipper? zipper?)]
              @defproc[(edge [i (or/c 0 1)] [f (-> (listof real?) (listof real?))]) (-> zipper? zipper?)]
              @defproc[(each [fl (-> (listof real?) (listof real?))]
                             [fr (-> (listof real?) (listof real?))]) (-> zipper? zipper?)]
              @defproc[(both [f (-> (listof real?) (listof real?))]) (-> zipper? zipper?)])]{
Each is a @racket[zipper? -> zipper?] command that moves the cursor by rewriting its
indexes, reading them straight off the installed guides and re-navigating once:

@itemlist[
  @item{@racket[at] --- collapse to an absolute gap at @racket[ix];}
  @item{@racket[move] --- collapse to a gap, with @racket[f] applied to the current basis;}
  @item{@racket[edge] --- apply @racket[f] to one edge @racket[i] (@racket[0] start, @racket[1] end);}
  @item{@racket[each] --- a separate function per edge;}
  @item{@racket[both] --- one function over both edges.}
]

The index-level helper @racket[slot] maps the innermost slot and leaves the frame
(the @racket[cdr]) untouched, so each edge stays in its own sexp.

@examples[#:eval ev
  (define z (cursor rope '(1 0)))                    (code:comment "a gap before bb")
  (code:line (doc ((setter zipper-focus "xx ") ((at '(2 0)) z)))        (code:comment "absolute: before cc"))
  (code:line (doc ((setter zipper-focus "xx ") ((move (slot add1)) z))) (code:comment "advance one slot"))
  (code:line (~a ((viewer zipper-focus) ((each values (slot add1)) z))) (code:comment "open the gap to a seg"))
  (define zs (cursor rope '(1 0) '(2 0)))            (code:comment "the seg [bb cc)")
  (code:line (~a ((viewer zipper-focus) ((both (slot add1)) zs)))       (code:comment "shift both edges right"))]
}

@section{Internals}

These describe the layer behind the surface --- the index model, the comparison, and
the lens style the command vocabulary is written in.

@subsection{The signed-spine index model}

An index is a @deftech{spine}: the per-level position list, innermost-first, slots
0-based at every level. Each component picks the side it reads at its level ---
@racket[>= -½] against the all-left @racket[front] spine, @racket[<= -1] against the
all-right @racket[back] spine (the pair @racket[sand-spines] reads; @racket[-1] = after
the last form). A @racket['back] component sits at @racket[< -½], a @racket['front] one
at @racket[>= -½]; the half-step gap keeps the two families disjoint, so a leaned head
(whitespace just after an open paren reads @racket[-½]) still classes as @racket['front].

Targets land on form starts @emph{and} on a frame's @bold{end slot} --- slot @racket[N]
of an @racket[N]-child frame, the position tight after the last child where an append
goes. Both anchorings name it: @racket[front] @racket[N] or @racket[back] @racket[-1].

@subsection{The two anchors and re-basing}

A position has @emph{two} indexes naming it. The path down to the cut's frame is a
left-based name either way; the two differ in the @bold{head} only --- anchored left
(read off the text to its left) or right (off the text to its right). @racket[anchors]
returns exactly this pair, and re-deriving the other head at a cursor @emph{is} the
anchor flip.

Because only the head re-bases, the flip data is a single number: the cut's
@racket[modulus], front head minus back head, equal to @racket[N+1] over a frame of
@racket[N] forms (the uniformity bar, encoding the modulus over @racket[N] forms).
@racket[base-left] / @racket[base-right] / @racket[flip] dispatch on the head's family
and shift it by @racket[m]; the path components and the @racket[½] refinement carry
through untouched. This is what one index alone cannot recover --- it knows its own
head but not the modulus that would re-base it.

@subsection[#:tag "the-comparison"]{The lexicographic comparison}

@racket[spine-cmp] zips the two co-indexed spines into one cut --- each level a
@racket[(front . back)] pair --- then compares an index against it lexicographically,
outermost-first. At each level it reads the spine the index's component selects
(@racket[front] for @racket[>= -½], @racket[back] for @racket[<= -1]) and takes the
first non-zero componentwise verdict, with the index first so @racket[+1] means the
target is right of the cut.

The comparison is naively lexicographic because of how the sexp algebra counts
completion (a frame counts at its closer, not its opener; see the @tt{sexp-summary}
docs). Under that counting an open frame's interior is a @emph{prefix extension} of the
frame's own start slot, so a shorter spine sorts before a deeper one --- a bare spine
sits before everything deeper inside it. That ordering rule is the early exit in
@racket[lexicographic] (@tt{algebra.rkt}) and replaced an earlier
@racket[-inf] padding scheme.

Because each component carries its own family tag, all-left, all-right, and mixed
indexes (a right head over a left path) all resolve through the same comparison ---
the property the navigation relies on so a flipped index navigates to the same place
as the index it was flipped from.

@subsection{The lens command vocabulary}

The command verbs ride @racket[idxs] --- @racket[(compose zipper-guide (list-of
index-of))], the lens from a zipper to its index list @racket[(ix0 ix1)] --- then choose
a reach with a selector lens at the tail (all from @tt{algebra.rkt}):

@itemlist[
  @item{@racket[(ldiag i)] collapses to position @racket[i] --- the gap verbs
        @racket[at] and @racket[move], whose put broadcasts one index to both edges;}
  @item{@racket[(lref i)] singles one edge out --- @racket[edge];}
  @item{no selector keeps the whole list --- the segment verbs @racket[each] and
        @racket[both].}
]

Each verb is one @racket[setter] / @racket[updater] through the composed lens,
rebuilding in a single put that re-navigates once. The style is deliberately
point-free: an ad-hoc edit threads the zipper through the composed lens in place
rather than naming @racket[z0]/@racket[z1]/@racket[z2] intermediates --- @racket[idxs]
and the verbs are the only standing shorthands. @racket[index-of] is the lens making a
guide and its index interchangeable (its view is @racket[guide-index], its put
@racket[slot-guide]); the helper internals are documented in @tt{algebra.rkt}.

@subsection{Document isos (test scaffolding)}

The @racket[test] submodule carries three genuine isos over the document's states ---
@bold{A} shape @tt{<->} spines (structure), @bold{C} tree @tt{<->} pieces (content),
@bold{B} pieces @tt{<->} text (text) --- checked against @tt{algebra.rkt}'s
@racket[iso] battery. They are the free scaffolding the guide-driven bridge (spines
locating cuts in the text) gets checked against; they are not part of the export
surface.
