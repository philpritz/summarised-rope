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
