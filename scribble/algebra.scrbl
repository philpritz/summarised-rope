#lang scribble/manual

@(require scribble/example
          racket/sandbox
          racket/runtime-path
          (for-label racket
                     "../toolbox/algebra.rkt"))

@; A trusted sandbox so the example evaluator may require the local module
@; (the default sandbox blocks reading files outside collects).
@(define-runtime-path algebra-path "../toolbox/algebra.rkt")
@(define ev
   (call-with-trusted-sandbox-configuration
    (lambda ()
      (define e (make-base-eval))
      (e `(require racket/list
                   (file ,(path->string algebra-path))))
      e)))

@title{algebra}

The project's small algebraic helpers --- the canonical home other files point to.
Two families: the @bold{iso} (a reversible function, @racket[iso] / @racket[compose-iso]
/ @racket[expt-iso] / @racket[iso-law?] / @racket[check-iso-laws]) and the @bold{lens}
(a variadic van Laarhoven optic, @racket[make-lens] with ops @racket[viewer] /
@racket[setter] / @racket[updater], and the list optics @racket[list-of] / @racket[lref]
/ @racket[ldiag] / @racket[varg]); plus the combinators @racket[on], @racket[arg],
@racket[pass], @racket[spread], @racket[variadic], @racket[fixed], and
@racket[lexicographic].

@section{iso}

@defproc[(iso [to (-> any/c any/c)] [from (-> any/c any/c)]) iso?]{
A focused @tt{(to, from)} pair. Calling an iso runs its focused (forward) side ---
@racket[prop:procedure] makes it @emph{be} a function, so only its own combinators
see the @racket[from] half.

@examples[#:eval ev
  (define inc (iso add1 sub1))
  (code:line (inc 10)   (code:comment "calling runs `to`"))]
See also @racket[compose-iso], @racket[expt-iso].
}

@defproc[(compose-iso [i iso?] ...) iso?]{
Composes any number of isos, staying an iso. The composite's inverse runs the halves
in reverse order (@tt{(g.f)@superscript{-1} = f@superscript{-1}.g@superscript{-1}}).
@racket[(compose-iso)] with no arguments is the identity iso --- the group unit.

@examples[#:eval ev
  (define inc (iso add1 sub1))
  (code:line ((compose-iso inc inc inc) 10) (code:comment "three composed, forward"))
  (code:line ((compose-iso) 42)             (code:comment "no isos = identity"))]
}

@defproc[(expt-iso [i iso?] [n exact-integer?]) iso?]{
Raises @racket[i] to an integer power, @bold{staying an iso}. Negative @racket[n] is
the inverse's positive power, so @racket[(expt-iso i -1)] @emph{is} the inverse and
@racket[(expt-iso i 0)] is the identity. The result is itself an iso, so it can be
inverted or re-exponentiated in turn.

@examples[#:eval ev
  (define inc (iso add1 sub1))
  (code:line ((expt-iso inc 3) 10)  (code:comment "forward thrice"))
  (code:line ((expt-iso inc -3) 13) (code:comment "negative = inverse's power"))
  (code:line ((expt-iso inc 0) 99)  (code:comment "n = 0 is the identity"))]
See @secref["iso-closure"] for why integer powers come for free.
}

@defproc[(iso-law? [i iso?] [x any/c]) boolean?]{
Whether @racket[x] round-trips through @racket[i] unchanged: run it through
@racket[to] then back through @racket[from] and compare with @racket[equal?].

@examples[#:eval ev
  (define inc (iso add1 sub1))
  (iso-law? inc 10)
  (code:line (iso-law? (iso add1 add1) 10) (code:comment "from doesn't undo to"))]
}

@defproc[(check-iso-laws [i iso?] [xs list?]) list?]{
Sweeps a corpus through @racket[iso-law?], returning the inputs that @emph{don't}
round-trip. @racket['()] means @racket[i] is a genuine iso over every one of them.

@examples[#:eval ev
  (define inc (iso add1 sub1))
  (check-iso-laws inc '(0 5 -3 99))
  (code:line (check-iso-laws (iso add1 add1) '(1 2 3)) (code:comment "none round-trip"))]
}

@section{make-lens and the lens ops}

@defproc[(make-lens [peek (-> any/c any (code:comment "structvals ... -> (values put focus ...)"))]) (-> procedure? procedure?)]{
Builds a variadic van Laarhoven lens from a @deftech{store coalgebra}: a @racket[peek]
that, given the structure's values, returns the @bold{put-back first} and then the foci
as multiple values. The put rebuilds the structure from new foci. One lens body serves
view and set alike; the three ops below drive it. Composition is plain @racket[compose]
--- function composition @emph{is} lens composition.

@examples[#:eval ev
  (code:comment "a lens onto a list's head (one focus)")
  (define fst (make-lens (lambda (xs) (values (lambda (x) (cons x (cdr xs))) (first xs)))))
  ((viewer fst) '(1 2 3))
  ((setter fst 9) '(1 2 3))
  ((updater fst add1) '(1 2 3))
  (code:comment "composition threads the put-backs: onto first-of-first")
  ((viewer (compose fst fst)) '((1 2) 3))
  ((setter (compose fst fst) 9) '((1 2) 3))]
See @secref["lens-theory"] for the put-first convention and the Const/Identity split.
}

@defproc[(viewer [l procedure?]) procedure?]{
A getter: returns the foci as @bold{multiple values} (one focus → one value), running
no put.}

@defproc[(setter [l procedure?] [x any/c] ...) procedure?]{
A command taking one new value per focus; the put runs, rebuilding the structure.}

@defproc[(updater [l procedure?] [f procedure?] ...) procedure?]{
A command taking one function per focus, each applied to its focus before the put.}

@section{list optics: list-of, lref, ldiag, varg}

@defproc[(list-of [el procedure?]) procedure?]{
Lifts a single-focus element lens over a list. @bold{One} focus --- the list of element
views; the put rebuilds element-wise via @racket[el]'s setter. The chain stays
single-value (it is @racket[lref] that fans out).

@examples[#:eval ev
  (code:comment "a car-lens, mapped over a list of pairs")
  (define carl (make-lens (lambda (p) (values (lambda (x) (cons x (cdr p))) (car p)))))
  (define li (list-of carl))
  (define gl (list (cons 1 'g) (cons 2 'g) (cons 3 'g)))
  (code:line ((viewer li) gl)                 (code:comment "ONE focus: the list of views"))
  ((setter li (list 10 20 30)) gl)]
}

@defproc[(lref [i exact-nonnegative-integer?] ...) procedure?]{
Indexes a list at positions @racket[i ...], @bold{fanning the focus into N values} (the
picked elements); the put writes them back into a copy. Length-safe --- it overwrites
slots, never reshapes, so an under-supplied put leaves the rest in place.

@examples[#:eval ev
  (define carl (make-lens (lambda (p) (values (lambda (x) (cons x (cdr p))) (car p)))))
  (define gl (list (cons 1 'g) (cons 2 'g) (cons 3 'g)))
  (define L (compose (list-of carl) (lref 0 2)))
  ((setter L 'X 'Y) gl)
  (code:line ((setter L 'X) gl) (code:comment "under-supplied: length preserved"))]
See also @racket[varg].
}

@defproc[(ldiag [i exact-nonnegative-integer?]) procedure?]{
The diagonal of a list: view position @racket[i] (the bias); the put @bold{broadcasts}
one value to every slot. The gap-collapsing twin of @racket[(lref i)] --- lawful only
when the slots are already equal.

@examples[#:eval ev
  ((viewer (ldiag 1)) '(a b c))
  (code:line ((setter (ldiag 0) 'X) '(a b c)) (code:comment "broadcast to all"))]
}

@defproc[(varg [i exact-nonnegative-integer?] ...) procedure?]{
The lens twin of @racket[arg]: focus the values at positions @racket[i ...], in that
order; the put writes them back. So @racket[((viewer (varg . is)) ...)] equals
@racket[((arg . is) ...)]. Lawful for distinct positions (a selection / permutation); a
repeated position is a lossy diagonal whose put-get fails.

@examples[#:eval ev
  (define carl (make-lens (lambda (p) (values (lambda (x) (cons x (cdr p))) (car p)))))
  (define gl (list (cons 1 'g) (cons 2 'g) (cons 3 'g)))
  (code:line ((viewer (compose (list-of carl) (lref 0 1 2) (varg 2 0))) gl) (code:comment "reorders to (3 1)"))]
See also @racket[arg].
}

@section{on, arg, pass, spread}

@defproc[(on [op procedure?] [f procedure?]) procedure?]{
@racket[(on op f) a b ...] = @racket[(op (f a) (f b) ...)] --- @racket[op] applied to its
arguments, each projected through @racket[f]. The n-ary generalization of Haskell's
binary @tt{Data.Function.on}. With a summary as @racket[f] it reads each side through it
--- e.g. @racket[(on guide smr)] wraps a guide for a bundle.

@examples[#:eval ev
  (code:line ((on + abs) -1 2 -3)   (code:comment "abs each, then +"))
  ((on cons add1) 1 2)]
}

@defproc[((arg [i exact-nonnegative-integer?] ...) [x any/c] ...) any]{
Projects arguments by 0-based position: returns the @racket[i]-th, ... arguments as
multiple values. The generalized projection (the K combinator) --- @racket[(arg 0)]
selects the first argument.

@examples[#:eval ev
  (code:line ((arg 2 0) 'a 'b 'c) (code:comment "two values: c then a"))]
See also @racket[varg], its lens twin.
}

@defproc[((pass [arg any/c] ...) [f procedure?] ...) any]{
Holds a tuple of arguments, then applies each function to them, as multiple values:
@racket[((pass . args) f g ...)] = @racket[(values (apply f args) (apply g args) ...)].
The thrush @racket[((pass x) f)] = @racket[(f x)], forked over several functions (juxt,
as values not a list).

@examples[#:eval ev
  (code:line ((pass 5) add1) (code:comment "one function = the thrush"))
  (code:line ((pass 3 4) + * -) (code:comment "each f applied to (3 4)"))]
}

@defproc[((spread [h procedure?] [f procedure?] ...) [a any/c] ...) any/c]{
Spread-combine: each function to its @bold{corresponding} argument, results combined by
@racket[h] --- @racket[((spread h f g ...) a b ...)] = @racket[(h (f a) (g b) ...)]. The
transpose-dual of @racket[pass] (which forks functions over one fixed arg-tuple).

@examples[#:eval ev
  ((spread list add1 sub1) 10 20)
  (code:line ((spread + values string-length) 10 "abc") (code:comment "the make-summary shape"))]
See @secref["spread-fold"] for its use inside @racket[variadic], and @secref["inlining"]
for the arity dispatch.
}

@section{variadic, fixed, lexicographic}

@defproc[((variadic [op procedure?] [id any/c]) [a any/c] ...) any/c]{
Lifts a binary @racket[op] (called accumulator-first, @racket[(op acc x)]) and a seed
@racket[id] to a function of any arity that left-folds its arguments from @racket[id]:
@racket[(variadic op id) a b] = @racket[(op (op id a) b)], and @racket[((variadic op id))]
= @racket[id]. @racket[id] is folded in even in the base cases --- no identity-law
assumption, so the result matches a plain left fold for @emph{any} @racket[op] / @racket[id].

@examples[#:eval ev
  ((variadic + 0) 1 2 3 4)
  (code:line ((variadic - 0) 5 3) (code:comment "non-monoidal: seed and order matter"))]
}

@defproc[((fixed [improve procedure?] [same? procedure? equal?] [key procedure? list]) [a any/c] ...) any]{
Iterates @racket[improve] from a seed to a fixed point. The seed and @racket[improve] may
carry @bold{multiple} values; the halt is an equality over a projection ---
@racket[(key v ...)] applied to the value-tuple @emph{as arguments}, mirroring
@racket[remove-duplicates]'s @racket[#:key]. Default @racket[key] = @racket[list] gives
whole-tuple @racket[equal?] (a true fixed point); pick a selector (@racket[(arg 0)]) or a
derived quantity, with a fitting equality, to settle on that instead.

@examples[#:eval ev
  (code:line ((fixed (lambda (n) (quotient n 2))) 100) (code:comment "halve to the fixpoint 0"))
  (code:line ((fixed (lambda (a b) (values b (min a b)))) 5 3) (code:comment "multi-value"))
  (code:line ((fixed sub1 = (lambda (n) (quotient n 10))) 25) (code:comment "stop when tens digit settles"))]
See @secref["inlining"] for the small-arity dispatch.
}

@defproc[((lexicographic [cmp (-> any/c any/c (or/c -1 0 1))]) [xs list?] [ys list?]) (or/c -1 0 1)]{
Lifts an element comparison to a 3-way order on sequences. Walks two lists in parallel;
the first non-zero elementwise verdict decides. If they agree up to the shorter, the
shorter is the lesser --- a prefix precedes its extension.

@examples[#:eval ev
  (define lc (lexicographic (lambda (a b) (cond [(< a b) -1] [(> a b) 1] [else 0]))))
  (code:line (lc '(1 2 3) '(1 2 3)) (code:comment "equal"))
  (code:line (lc '(1 2) '(1 2 3))   (code:comment "prefix is lesser"))
  (code:line (lc '(1 5) '(1 2 9))   (code:comment "first difference decides"))]
}

@section[#:tag "design"]{Design and internals}

The narrative offloaded from the source comments: why the iso closes under integer
powers, the store-coalgebra basis of the lens, and the inlining rationale.

@subsection[#:tag "iso-closure"]{Why expt-iso gets all of Z}

Isos compose as a group: @racket[compose-iso] is the multiplication, the identity iso
(@racket[(compose-iso)]) is the unit, and toggling the focus is the inverse. So
@racket[expt-iso] gets the whole of @tt{Z} for free --- a negative power is the
@emph{positive} power of the inverse, and @racket[(expt-iso i -1)] = the inverse itself.

This is the @tt{expt -1 = inverse} of generic arithmetic (scmutils' @tt{(expt M -1)},
Lean's @tt{a ^ (-1 : Z) = a@superscript{-1}}), but landing @bold{inside the iso type}
rather than handing back a bare function: the result of any @racket[expt-iso] is still an
iso, so it can be inverted, re-exponentiated, or fed back into @racket[compose-iso]. The
law @racket[(expt-iso (expt-iso i m) n)] = @racket[(expt-iso i (* m n))] holds because
the group structure is closed.

@subsection[#:tag "lens-theory"]{The lens: store coalgebra, put-first, Const vs Identity}

A lens here is a @bold{variadic} van Laarhoven optic over the @bold{value stream}. It
focuses one or more foci inside a structure that is itself one or more values. The
@racket[peek] is a store coalgebra: @tt{structvals ... -> (values put focus ...)}.

@bold{Why put-first.} With variadic foci the focus count varies from lens to lens, so the
foci cannot occupy a fixed prefix of the result. The @racket[put] is the one value whose
position is always fixed, so it leads. And because composition is plain @racket[compose],
@racket[(compose f g)] already does the multi-value threading that @racket[call-with-values]
would otherwise spell out: an outer lens's foci flow straight in as the inner lens's
@tt{structvals}, with the arities lining up at each seam.

@bold{Why Const vs Identity.} One lens body serves both view and set; the foci handler
@racket[k] picks the functor. @bold{view} = the Const functor: it carries the foci out and
runs @emph{no} put. @bold{set / over} = the Identity functor: raw values, the put runs. The
default action is @racket[(apply put ...)]; only view departs from it, so it alone is
tagged --- the private @racket[const-box] holds the foci as a list, and a lone
@racket[const-box] coming back is the signal to skip the put. Identity stays bare values,
so the common path pays nothing.

The list optics build on this: @racket[list-of] keeps one focus (the chain stays
single-value), @racket[lref] is where it fans out to N, @racket[ldiag] is @racket[lref]'s
gap-collapsing twin, and @racket[varg] is @racket[arg] reified as a lens.

@subsection[#:tag "spread-fold"]{spread inside variadic}

@racket[spread] preprocesses a reducer's arguments. @racket[(variadic (spread combine
values coerce) id)] folds coerced arguments: @racket[values] passes the accumulator
through untouched, @racket[coerce] maps each incoming element, and @racket[combine] folds
the two. This is the @racket[make-summary] shape in @tt{rope-core.rkt} --- the accumulator
is the running summary, the element is coerced to a summary, and the monoid combine fuses
them.

@subsection[#:tag "inlining"]{The small-arity inlining}

@racket[spread], @racket[variadic], and @racket[fixed] each special-case their common
arities so the hot path skips the generic machinery; the inlined forms emit @emph{exactly}
the same values as the generic tail.

@itemlist[
  @item{@racket[spread] dispatches on the @bold{function count} via @racket[case-lambda]:
  1..4 functions get an inlined positional lambda (no rest-arg list, no @racket[map]), so
  the common arities run at hand-wrapper speed; 5+ falls to a @racket[map]/@racket[apply]
  tail.}
  @item{@racket[variadic] inlines the 0/1/2-ary cases (nearly every call), skipping the
  rest-arg list and the @racket[foldl] --- but folding @racket[id] in regardless, so the
  value is identical for any @racket[op] / @racket[id].}
  @item{@racket[fixed] inlines arities 1..4 (navigate's @tt{ascend}/@tt{descend} are
  2-value). The internal macro @tt{fixed-case} builds, per clause, a loop on named
  variables with no per-step @racket[list]/@racket[apply]/@racket[compose], carrying the
  running key forward (one @racket[key] call per step, not two). It expands at compile time
  and lives inside @racket[fixed] so its template captures @tt{improve}/@tt{same?}/@tt{key}
  from that scope. Any larger arity falls to @tt{rest-loop}, which reifies the tuple as a
  list and threads it back via @racket[(compose list improve)].}
]

@section{Row lifts}

@racket[lref], @racket[ldiag], and @racket[list-of] take a one-list world with the
put policy baked in. The row lifts factor the policy out into an opt:

@verbatim{
  P               : (v1 ... vk)       <-> view; news; put -> j values
  (opt-lref n P)  : (list1 ... listk) <-> same view/news;  put -> j lists, written at n
  (opt-ldiag n P) :                        same, broadcast along each written list
  (opt-list P)    :                    <-> view/news LISTIFIED; P at every index
}

The one law: the lifted opt has @emph{P's signature with every world component
wrapped in a list} --- arities inherit, so a policy/outer mismatch is an arity
error at the seam, never a silent drop.

@racket[focal] is the context-discipline policy --- whole row in view, first
value writable, one value back --- so @racket[(opt-lref n focal)] reads "slot n of
the focal list, its context in view". @racket[(varg 0)] as the policy gives
symmetric puts (every list returned rebuilt); @racket[(vdiag 0)] broadcasts one
value @emph{across} the lists at n, the orthogonal diagonal to @racket[ldiag]'s
along-the-list broadcast.
