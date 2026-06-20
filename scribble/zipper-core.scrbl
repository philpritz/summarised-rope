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
