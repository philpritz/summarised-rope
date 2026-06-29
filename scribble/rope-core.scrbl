#lang scribble/manual

@(require scribble/example
          racket/sandbox
          racket/runtime-path
          (for-label racket
                     "../rope-core.rkt"
                     (submod "../rope-core.rkt" internal)))

@; A trusted sandbox so the example evaluator may require the local module and its
@; internals submodule (the default sandbox blocks reading files outside collects).
@(define-runtime-path rope-core-path "../rope-core.rkt")
@(define ev
   (call-with-trusted-sandbox-configuration
    (lambda ()
      (define e (make-base-eval))
      (e `(require racket/format
                   (file ,(path->string rope-core-path))
                   (submod (file ,(path->string rope-core-path)) internal)))
      e)))

@title{rope-core}

The public surface of the summarised rope: @racket[make-summary] @racket[make-rope]
@racket[multisect] @racket[frame].

@section{make-summary}

@defproc[(make-summary [string-summary (-> string? any/c)]
                       [combine (-> any/c any/c any/c)])
         smr]{
Returns a summary function @racket[smr] from a per-string measure and a binary
@racket[combine].
}

@defproc[(smr [part (or/c string? rope? summary-part? any/c)] ...) any/c]{
Folds @racket[combine] over its arguments, left to right from
@racket[(string-summary "")]. Each argument is first coerced to a summary value:
@itemlist[
  @item{a @racket[string?] --- @racket[(string-summary str)];}
  @item{a @racket[rope?] --- its cached summary;}
  @item{a @racket[summary-part?] --- its part;}
  @item{anything else --- taken to be a summary value already.}
]

@margin-note{A node caches its algebra (@racket[rope-algebra]), so @racket[make-summary]
could reject a rope built under a different summary --- but how to tell which summary
minted a given summary object is still unsolved: doing it generally would need that same
stamp on the objects themselves.}

@examples[#:eval ev
  (define len (make-summary string-length +))
  (define r ((make-rope len) "hello"))
  (code:line (len r)        (code:comment "a rope folds to its cached summary"))
  (code:line (len "ab" r 7) (code:comment "a string, a rope, and a summary value"))]

See also @racket[make-rope].
}

@section{make-rope}

@defproc[(make-rope [smr procedure?]) build]{
Returns a rope builder @racket[build] from a summary function @racket[smr].
}

@defproc[(build [part (or/c string? rope?)] ...) rope?]{
Joins its arguments into a single balanced rope under @racket[smr], left to right.
Each argument:
@itemlist[
  @item{a @racket[string?] --- chunked into @racket[max-leaf]-sized leaves;}
  @item{a @racket[rope?] --- spliced in unchanged.}
]

Adjacent leaves that fit one leaf are fused, a lopsided tree is rebalanced, and no
arguments gives the empty rope.

@examples[#:eval ev
  (define len (make-summary string-length +))
  (define build (make-rope len))
  (code:line (build "hello " "world")        (code:comment "strings chunked and joined"))
  (code:line (rope-leaves (build "ab" "cd")) (code:comment "two small leaves fuse into one"))
  (code:line (build "(" (build "ab") ")")    (code:comment "a rope spliced in unchanged"))]

@margin-note{@racket[rope-leaves] comes from @racket[(submod "rope-core.rkt" internal)]
--- the internals submodule, not the core API.}

See also @racket[make-summary].
}

@section{Guides}

@margin-note{Provisional --- a stub gathering the guide concept in one place, to be
fleshed out (with a defined @racket[guide] contract) later.}

A @deftech{guide} is a comparator on summaries, @racket[(-> any/c any/c (or/c -1 0 1))]:
@racket[(g L R)] judges a candidate cut from the summaries @racket[L] and @racket[R] to its
two sides, returning @racket[1] when the boundary it names lies right of the cut, @racket[-1]
when left, @racket[0] at it. Guides are the currency of cutting --- @racket[multisect] takes a
vector of them, @racket[frame] bakes outer context into one, the internal @racket[within-ratio]
is a weight guide for balancing, and the zipper navigates by them.

@section{multisect}

@defproc[(multisect [smr procedure?]
                    [guides (vectorof (-> any/c any/c (or/c -1 0 1))) #()])
         split]{
Returns a splitter @racket[split] from a summary function @racket[smr] and a vector
of @racket[guides].
}

@defproc[(split [t rope?]) any]{
Cuts @racket[t] at each guide's boundary, left to right, into n+1 pieces (as multiple
values) whose concatenation is @racket[t] (n is the number of guides). A guide is a
comparator on summaries --- @racket[(guide L R)] returns @racket[1] if its boundary
lies right of the candidate cut, @racket[-1] if left, @racket[0] at it. With no guides
(or @racket[#()]), @racket[split] is the balance halve, cutting @racket[t] into two
weight-even halves.

@examples[#:eval ev
  (define len (make-summary string-length +))
  (code:line (define at5 (lambda (L R) (cond [(< L 5) 1] [(> L 5) -1] [else 0])))
             (code:comment "a guide: boundary at char 5"))
  (define-values (l r) ((multisect len (vector at5)) ((make-rope len) "hello world")))
  (map ~a (list l r))]

See also @racket[make-rope].
}

@section{frame}

@defproc[(frame [combine procedure?] [b any/c] [a any/c]) (-> guide guide)]{
Bakes outer context into a guide --- @racket[((frame combine b a) g)] is
@racket[(lambda (l r) (g (combine b l) (combine r a)))]:
@itemlist[
  @item{@racket[b], @racket[a] --- the summaries left and right of the focus;}
  @item{@racket[combine] --- joins context to a side (ordinarily @racket[smr]).}
]

See also @racket[multisect].
}

@section{Internals}

These are implementation internals, reached with @racket[(require (submod "rope-core.rkt" internal))]
--- not the stable public API; they track the implementation and may change.

@subsection{The PART 1 / PART 2 boundary and its invariant}

PART 1 (nodes, construction, @racket[rope-join]) maintains one invariant that PART 2 --- the guided
and balanced descent --- leans on: @racket[(rope-join (rope-split t))] is @racket[t] for every
well-formed rope; equivalently, no seam carries a fusable adjacent leaf pair.

@bold{Why it matters.} It lets a single fusing @racket[rope-join] serve both jobs. A guided
midpoint-split produces @emph{fusable} fragments, which the join cleans back into
@racket[max-leaf]-sized leaves; balance's rejoins on the rise are the tree's own seams, which the
invariant guarantees are @emph{non-fusable}, so the same join leaves them --- and their leaf counts,
which balance reads --- untouched. Without it, PART 2 would need two joins: a fusing one for guided
fragments and a non-fusing one for balance.

@subsection{bisect: why the leaf isn't a special case}

It could have been: a structural descent over branches, @emph{plus} a separate binary search at the
leaf --- which a caller would have to drive by handing in a split-string procedure beside the guide.
Instead @racket[rope-split] is made total --- a leaf halves at its midpoint just as a branch halves at
its seam --- so one @racket[bisect] descent covers both, intra-leaf binary search falling out for free.
Removing that special case is exactly what keeps the cutting interface a @emph{guide} alone: with no
leaf path to feed, the guide itself --- comparing summaries as the descent halves into a leaf ---
locates the exact gap, no companion splitter required.

@subsection{frame is transient, never stored}

@racket[frame] bakes outer context into a guide so it judges within the focus, but the result is
an opaque closure --- a structured guide (a sexp slot-guide, say) is no longer readable as its
index through it. So the machine frames a guide only to judge a single cut and then discards it
(@racket[multisect] and the zipper's @racket[navigate] both do this); it never frames-and-stores.

@subsection{Contracts: smr/c and guide/c}

@racket[smr/c] is just @racket[procedure?], deliberately not @racket[(unconstrained-domain-> any/c)]:
a summary value is user-defined and opaque, so the range would be @racket[any/c] and the arrow would
check nothing the flat predicate doesn't --- it would only chaperone the @racket[smr], which is then
called on every measure. A flat check keeps it off the hot path. @racket[guide/c], by contrast, stays
higher-order (@racket[(-> any/c any/c (or/c -1 0 1))]): a guide's codomain @emph{is} checkable, so a
guide that returns a bad value is caught at the split rather than deep inside binary search.
