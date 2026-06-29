#lang scribble/manual

@(require scribble/example
          racket/sandbox
          racket/runtime-path
          (for-label racket
                     "../rope-core.rkt"
                     "../zipper-core.rkt"
                     (submod "../zipper-core.rkt" internal)))

@; A trusted sandbox so the example evaluator may require the local modules and the
@; zipper-core internals submodule (the default sandbox blocks reading files outside
@; collects). rope-core supplies make-summary / make-rope; zipper-core the surface;
@; the internal submodule the editing-trace tools.
@(define-runtime-path rope-core-path "../rope-core.rkt")
@(define-runtime-path zipper-core-path "../zipper-core.rkt")
@(define ev
   (call-with-trusted-sandbox-configuration
    (lambda ()
      (define e (make-base-eval))
      (e `(require racket/format
                   (file ,(path->string rope-core-path))
                   (file ,(path->string zipper-core-path))
                   (submod (file ,(path->string zipper-core-path)) internal)))
      e)))

@title{zipper-core}

Structured navigation and editing over a summarised rope, as a movable cursor. The
public surface: @racket[start] @racket[zipper-guide] @racket[zipper-focus]
@racket[to-root] @racket[on-edges]. The editing-trace tools @racket[chain] /
@racket[run-chain] are dev tooling in the @racket[internal] submodule (see
@secref["tracing"] below).

@section{start}

@defproc[(start [smr procedure?]
                [rope rope?]
                [cursor (vector/c procedure? procedure?)])
         zipper?]{
Returns a fresh zipper --- the whole @racket[rope] as focus, @racket[cursor]
installed but @emph{not yet navigated} (the first write or install navigates). A
cursor is a 2-vector of guides @racket[(vector start end)]; there is no guideless
zipper. A guide is a comparator @racket[(L R)] returning @racket[1], @racket[-1], or
@racket[0] --- naming one boundary by which side a candidate cut falls on.

@examples[#:eval ev
  (define cc (make-summary string-length +))
  (define build (make-rope cc))
  (code:line (define ((at n) L R) (cond [(< L n) 1] [(> L n) -1] [else 0]))
             (code:comment "a char guide: boundary at offset n"))
  (define z (start cc (build "hello world") (vector (at 0) (at 0))))
  z]

A zipper prints as its document with the cursor marked --- a caret @litchar{‸} at a
gap, brackets @litchar{⟦}…@litchar{⟧} around a segment.

See also @racket[zipper-guide].
}

@section{zipper-guide}

@defproc*[([(zipper-guide [z zipper?]) (vector/c procedure? procedure?)]
           [(zipper-guide [cursor (vector/c procedure? procedure?)]) (-> zipper? zipper?)]
           [(zipper-guide [f procedure?]) (-> zipper? zipper?)])]{
The navigation accessor, three faces dispatched by argument:
@itemlist[
  @item{a @racket[zipper?] --- reads the installed cursor;}
  @item{a cursor (a 2-vector) --- returns a command that installs it;}
  @item{a procedure @racket[f] --- returns a command that installs @racket[(f current)] (modify).}
]

Both write faces navigate: installing moves the focus to where the cursor's guides
point. When the two guides coincide the cursor is a @emph{gap} (an empty focus, a
caret); when the start guide sits left of the end guide it is a @emph{segment}. A
crossed cursor (end left of start) is rejected.

@examples[#:eval ev
  (define g (vector (at 0) (at 5)))
  (code:line (eq? (zipper-guide ((zipper-guide g) z)) g) (code:comment "read returns the installed pair"))
  (code:line ((zipper-guide (vector (at 0) (at 5))) z)   (code:comment "install a segment"))
  (code:line ((zipper-guide (vector (at 5) (at 5))) z)   (code:comment "install a gap"))
  (define z5 ((zipper-guide (vector (at 0) (at 5))) z))
  (code:line ((zipper-guide (lambda (gs) (vector (vector-ref gs 0) (at 11)))) z5)
             (code:comment "modify: keep the start, push the end"))]

See also @racket[zipper-focus], @racket[start].
}

@section{zipper-focus}

@defproc*[([(zipper-focus [z zipper?]) rope?]
           [(zipper-focus [content (or/c string? rope?)]) (-> zipper? zipper?)]
           [(zipper-focus [f procedure?]) (-> zipper? zipper?)])]{
The editing accessor, @racket[zipper-guide]'s twin --- three faces by argument:
@itemlist[
  @item{a @racket[zipper?] --- reads the focus rope;}
  @item{content (a @racket[string?] or @racket[rope?]) --- returns a command that swaps it in;}
  @item{a procedure @racket[f] --- returns a command that swaps in @racket[(f current)] (modify).}
]

Every write navigates, so after a swap the cursor stands where its guides point on
the @emph{new} text (with char guides, that re-resolves by offset). Delete is a swap
of @racket[""]; insert is a swap at a gap; wrapping is a modify; edits chain by
composition.

@examples[#:eval ev
  (code:line (zipper-focus z5)                  (code:comment "read the focus"))
  (code:line ((zipper-focus "HI") z5)           (code:comment "swap -- the cursor re-resolves on the new text"))
  (define zg ((zipper-guide (vector (at 5) (at 5))) z))
  (code:line ((zipper-focus "XYZ") zg)          (code:comment "insert at a gap"))
  (code:line (zipper-focus (to-root ((zipper-focus "") z5)))
             (code:comment "delete = swap empty string; read the whole document"))
  (code:line (zipper-focus (to-root ((zipper-focus (lambda (m) (build "[" m "]"))) z5)))
             (code:comment "wrap (modify)"))]

See also @racket[zipper-guide], @racket[to-root].
}

@section{to-root}

@defproc[(to-root [z zipper?]) zipper?]{
Folds every crumb back into the head --- the focus becomes the whole document.
Homing: deliberately @emph{outside} the navigation lift, so it never descends again;
the installed cursor survives for the next install. A command (@racket[(-> zipper? zipper?)]).

@examples[#:eval ev
  (code:line (zipper-focus z5)            (code:comment "the focus is the segment"))
  (code:line (zipper-focus (to-root z5))  (code:comment "homed -- the focus is the whole document"))
  (code:line (to-root z5)                 (code:comment "the cursor survives, so the print re-marks it"))]

See also @racket[start], @racket[zipper-focus].
}

@section{on-edges}

@defproc[(on-edges [c (procedure-arity-includes/c 2)]
                   [f (procedure-arity-includes/c 2)]
                   [g (procedure-arity-includes/c 2)])
         (-> zipper? any)]{
Returns a reader @racket[edges] over a zipper.
}

@defproc[(edges [z zipper?]) any]{
Reads the cursor's two edges as cuts and combines them. Each edge of the focus is a
cut on the document; the focus folds onto the side the edge doesn't face, with the
zipper's own summary. @racket[((on-edges c f g) z)] computes
@racket[(c (f b (smr m a)) (g (smr b m) a))], where @racket[b] · @racket[m] ·
@racket[a] is the before-summary, focus, and after-summary: @racket[f] reads the left
edge (before, then focus-plus-after), @racket[g] the right (before-plus-focus, then
after), and @racket[c] combines them. @racket[c] may return multiple values.

@examples[#:eval ev
  (define zworld ((zipper-guide (vector (at 6) (at 11))) z))
  (code:line ((on-edges list list list) zworld) (code:comment "focus world: b = 6, m = 5, a = 0"))
  (code:line ((on-edges + - -) zworld)          (code:comment "(+ (- 6 5) (- 11 0))"))]

See also @racket[zipper-guide].
}

@section[#:tag "tracing"]{Tracing (dev)}

@racket[chain] and @racket[run-chain] are a REPL tracing aid, not the
navigation/editing API --- they live in the @racket[internal] submodule, reached with
@racket[(require (submod "zipper-core.rkt" internal))].

@margin-note{@racket[chain] and @racket[run-chain] come from
@racket[(submod "zipper-core.rkt" internal)] --- the dev submodule, not the core
navigation/editing API.}

@defform[(chain z0 op ...)]{
Pipes @racket[z0] through the commands @racket[op ...] (each a
@racket[(-> zipper? zipper?)]), printing each command's source beside the zipper it
produces, and returns the final zipper. The macro captures each @racket[op]'s source
(only a macro can) and hands the labelled list to @racket[run-chain].

@examples[#:eval ev
  (chain z
    (zipper-guide (vector (at 0) (at 5)))
    (zipper-focus "HI")
    to-root)]
}

@defproc[(run-chain [z0 zipper?]
                    [steps (listof (cons/c any/c (-> zipper? zipper?)))])
         zipper?]{
The function @racket[chain] expands to: each step pairs a printable label with a
command, threaded left to right from @racket[z0].
}

@section{Internals}

These describe the machine behind the surface --- design internals, private to the
module (distinct from the @racket[internal] submodule's tracing aids above). The whole
file is @bold{guide-agnostic}: it only ever @emph{calls} a guide, never naming its kind,
so structural guides (sexp, char, …) live in their own files.

@subsection{The cursor as a stack machine}

A cursor's state is a @deftech{head} --- @racket[before] · @racket[focus] ·
@racket[after], the focus rope flanked by the summaries of everything outside it ---
together with a crumb stack, each crumb a closure @racket[(head -> head)] that rebuilds
the parent focus one level up. An @deftech{op} is
@racket[(smr guides -> ((head stack) -> (values head stack)))]; the zipper threads its
own summary and cursor into every op.

@racket[zipper-lift] composes a run of ops (rightmost first, like @racket[compose]) with
@racket[navigate] fixed as the permanent last op, then reseals into a zipper. So every
lifted run lands with the cursor standing where its guides point on the new state ---
@bold{every write navigates}. Installing a cursor and swapping content are both lifted;
an empty run, @racket[(zipper-lift)], is plain re-navigation. Because navigation is
re-derived from the guides each time, an index swap (re-anchoring) and a guide swap (a
move) are the same operation.

@racket[to-root] is the one lifecycle op deliberately @emph{outside} the lift: homing
folds the crumbs back into the head without navigating down again, and the installed
cursor survives for the next install.

@subsection{The navigate pipeline}

@racket[navigate] is four stages composed into one op:

@itemlist[
  @item{@bold{ascend} --- rise to a fixpoint of @racket[rise]: pop a crumb and rebuild
        the parent focus until the focus brackets the whole segment (its
        @racket[contains?] test: start not left of the focus's left edge, end not right
        of its right edge), or the stack empties;}
  @item{@bold{uncrossed} --- reject a crossed cursor (an end boundary left of start)
        before any descent;}
  @item{@bold{descend} --- strip whole sub-ropes to the minimal node, a fixpoint of
        @racket[toward]: halve the focus and route by the seam reads --- whole segment
        right of the seam descends right, whole segment left descends left, a straddle /
        gap / boundary-on-seam halts (carve places the exact cut); an atomic focus (an
        empty half) halts too;}
  @item{@bold{carve} --- cut the focus exactly at the two boundaries with
        @racket[multisect], the middle becoming the new focus.}
]

The refocusing throughout is @racket[lens]: given a splitter
@racket[(rope -> (values ls m rs))] it refocuses a head onto @racket[m], folding
@racket[ls] / @racket[rs] into the anchors, and returns the crumb that rebuilds the
parent.

@subsection{Printing reconstructs the cursor by re-cutting}

A zipper prints as its document with the cursor marked --- a caret at a gap, brackets
around a segment. The marked pieces are read by @racket[multisect]-ing the @emph{root}
document with the installed guides, not off the crumb stack. This leans on the
guide–focus alignment the lift maintains: because every write re-navigates, the cursor
stands exactly where its guides point, so the re-cut reproduces the focus.

@margin-note{A guide-free reconstruction off the crumbs was sketched and @bold{parked};
every zipper carries a cursor (@racket[start] requires one), so there is no guideless
case to fall back on.}

@subsection{zipper-focus, zipper-head, and the coercion boundary}

@racket[zipper-focus] is @racket[(compose zipper-head head-focus)]. @racket[zipper-head] is
the lens onto the whole machine head (@racket[before] · @racket[focus] · @racket[after]);
@racket[head-focus] the lens onto just the focus field. The split puts the @racket[make-rope]
coercion in @racket[zipper-head], where the zipper's summary is in scope: a bare head carries
no summary, so a raw-content focus (a string handed to a put) is coerced back into a rope at
head installation --- the boundary where every write lands and re-navigates. @racket[head-focus]
only swaps the field, leaving the flanking summaries. Both are private; only @racket[zipper-focus]
is exported.

@subsection{edge-sides: a cut read as two foci}

@racket[(edge-sides i)] is the lens onto edge @racket[i]'s summary cut, read straight off a
zipper. The focus folds into the side the edge doesn't face --- the start edge reads
(@racket[before] @litchar{|} @racket[focus]·@racket[after]), the end edge
(@racket[before]·@racket[focus] @litchar{|} @racket[after]) --- so the two foci @racket[L]
@racket[R] are the summaries flanking that one boundary. Its put writes a gap at the cut and
re-navigates (through @racket[zipper-head]). Consume the view with a continuation @racket[k]
(@racket[viewer]'s optional fold): @racket[k] receives @racket[L] @racket[R] --- @racket[cut-index]
reads a sexp index off them, @racket[list] collects the pair.

@subsection{Contracts: lens/c}

@racket[lens/c] is just @racket[procedure?]: a van Laarhoven lens is
@racket[(-> (-> any/c f) (-> zipper? f))], whose functor structure isn't a flat contract, and the
ops (@racket[viewer] / @racket[setter] / @racket[updater]) enforce the shape in use --- so the flat
predicate is all the contract can check.
