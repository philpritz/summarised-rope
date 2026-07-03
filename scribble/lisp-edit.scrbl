#lang scribble/manual

@(require (for-label racket
                     "../lisp-edit.rkt"
                     "../helper-algebras.rkt"))

@title{lisp-edit}

The editing layer over zipper-core and the lisp summary: the cursor's focus as its
labeled lexical runs. One pipeline of composed opts:

@verbatim{
  zipper-focus : z          <-> (values fr bs as)          the INDEXED focus
  split-runs   : (fr bs as) <-> (values frs bss ass)       the head triple, pluralized
  labeled-run  : (fr bs as) <-> (values fr class)          one piece, judged
  label-runs   = (opt-list labeled-run)
  typed-runs   = zipper-focus o split-runs o label-runs
  label        : bs fr as -> class                         the scalar judge
}

@section{The indexed optics}

This module re-exports zipper-core with two optics @emph{widened}:
@racket[zipper-focus] views @racket[(values fr bs as)] --- the focus rope flanked
by its two summaries --- and @racket[zipper-guide] views
@racket[(values guides Ls Rs)], the guide list with each edge's cut behind it,
aligned by edge. Both puts are zipper-core's own objects: the widening is get-side
only, every write path unchanged. Contexts are read-only by protocol --- the puts
consume the focal value alone.

Composed with the row lifts (helper-algebras):
@racket[(compose-opt zipper-guide (opt-lref 0 focal))] is edge 0's guide with its
cut in view; @racket[(opt-ldiag 1 focal)] as the inner stage collapses the cursor
onto edge 1.

@section{The pipeline}

@racket[split-runs] takes the smr the zipper's rope is built with (the put rejoins
through @racket[(make-rope smr)] --- ropes pass through untouched, structure
shared; strings coerce to leaves). Its cuts are context-true (@tt{frame-guide*}
bakes the flanks into the runs guide) and its scans give every piece its own
@tt{(bs fr as)}: a prefix scan for @tt{bss}, a suffix scan for @tt{ass}.

@racket[label] judges one contiguous run in context; an all-glue focus adopts the
class it adheres to, read through @tt{as}. @racket[labeled-run] wears it as an opt
(the put consumes a new piece alone); @racket[label-runs] is its elementwise lift
--- sequencing lives entirely in the split stage, judgment is pointwise.

A type-directed edit is one @racket[opt-update]: the transform sees
@tt{(frs labels)} and returns new pieces --- ropes or strings, mixed freely; the
untouched pieces re-enter the document as their existing trees.
