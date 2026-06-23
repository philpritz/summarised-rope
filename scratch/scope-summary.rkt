#lang racket

;; SCRATCH: a SCOPE summary -- bound/free variable highlighting for the core binder forms
;; (lambda/let/let*/letrec), pure sexp layer, NO strings/comments.
;;
;; Stage 1 here: `analyze` -- a straight stack parser (ground truth, also drives the widget).
;; Classifies each atom occurrence: 'keyword | 'binding | 'bound | 'free | 'literal.
;; Stages 2+ (monoid leaf/combine, laws, timing) come after, validated against this.

(require racket/match
         "../rope-core.rkt"            ; make-summary make-rope multisect
         "../summaries/summaries.rkt"      ; bundle char-smr
         "../helper-algebras.rkt"     ; on
         "../bench/bench.rkt")        ; measure (struct-out stats)
(provide (all-defined-out))

(define binder-kw
  (hash "lambda" 'lam "λ" 'lam
        "let" 'let "let*" 'let* "letrec" 'letrec "letrec*" 'letrec
        "let-values" 'let-values "let*-values" 'let*-values
        "letrec-values" 'letrec-values "letrec*-values" 'letrec-values
        "define" 'define "define-values" 'define-values))
(define (for-kw? t) (regexp-match? #px"^for[*]?(/.+)?$" t))   ; for, for*, for/list, for*/fold, ...
(define openers (string->list "([{"))
(define closers (string->list ")]}"))

;; tokens with positions: (list type start end text), type in 'open|'close|'atom
(define (toks s)
  (for/list ([p (in-list (regexp-match-positions* #px"[][(){}]|[^][(){}\\s]+" s))])
    (define t (substring s (car p) (cdr p)))
    (list (cond [(and (= 1 (string-length t)) (memv (string-ref t 0) openers)) 'open]
                [(and (= 1 (string-length t)) (memv (string-ref t 0) closers)) 'close]
                [else 'atom])
          (car p) (cdr p) t)))

(define (number-like? t) (regexp-match? #px"^[-+]?[0-9]" t))
(define (kw-marker? t)   (regexp-match? #px"^#:" t))     ; a keyword token like #:key (not a binding)

;; ---- the AST: tokens -> a position-bearing tree ----
(struct an (a b t) #:transparent)         ; atom node: [a,b) source span + text
(struct fm (a kids) #:transparent)        ; form node: open-bracket pos + child nodes

;; parse robustly: a stray close ends the current group; EOF closes all open groups.
(define (parse s)
  (define (rd tks)                        ; -> (values nodes rest-at-close-or-eof)
    (let loop ([tks tks] [acc '()])
      (cond
        [(null? tks) (values (reverse acc) '())]
        [else
         (match-define (list ty a b t) (car tks))
         (case ty
           [(open)  (define-values (kids rest) (rd (cdr tks)))
                    (loop (if (pair? rest) (cdr rest) rest) (cons (fm a kids) acc))]
           [(close) (values (reverse acc) tks)]
           [(atom)  (loop (cdr tks) (cons (an a b t) acc))])])))
  (define-values (nodes _) (rd (toks s)))
  nodes)

;; ---- environment (the in-scope names) + span emitters ----
(define (env+ env names) (append names env))            ; names as strings; membership by `member`
(define (kw-span n)      (list (an-a n) (an-b n) 'keyword))
(define (binding-span n) (list (an-a n) (an-b n) 'binding))
(define (ref-span n env)                                ; an atom used as a value reference
  (list (an-a n) (an-b n)
        (cond [(number-like? (an-t n))   'literal]
              [(and (member (an-t n) env) #t) 'bound]
              [else                      'free])))

;; ---- name extractors ----
(define (all-atoms node)                                ; every atom under a node (skips '.')
  (cond [(an? node) (if (equal? (an-t node) ".") '() (list node))]
        [(fm? node) (append* (map all-atoms (fm-kids node)))]
        [else '()]))
;; lambda formals: atoms in the list (bare atom = single rest); skip '.', skip #:kw markers;
;; an optional/keyword clause [x default] contributes its head atom x.
(define (formal-atoms node)
  (cond
    [(an? node) (if (equal? (an-t node) ".") '() (list node))]
    [(fm? node)
     (append*
      (map (lambda (k)
             (cond [(and (an? k) (not (equal? (an-t k) ".")) (not (kw-marker? (an-t k)))) (list k)]
                   [(and (fm? k) (pair? (fm-kids k)) (an? (car (fm-kids k)))) (list (car (fm-kids k)))]
                   [else '()]))
           (fm-kids node)))]
    [else '()]))
;; a let-style clause [name rhs] -> head atom + rhs node
(define (clause-name+rhs c)
  (if (and (fm? c) (pair? (fm-kids c)))
      (values (and (an? (car (fm-kids c))) (car (fm-kids c)))
              (and (pair? (cdr (fm-kids c))) (cadr (fm-kids c))))
      (values #f #f)))
;; a values clause [(a b ...) rhs] -> name atoms of child-0 + rhs node
(define (vclause-names+rhs c)
  (if (and (fm? c) (pair? (fm-kids c)))
      (values (filter an? (let ([h (car (fm-kids c))]) (if (fm? h) (fm-kids h) (list h))))
              (and (pair? (cdr (fm-kids c))) (cadr (fm-kids c))))
      (values '() #f)))
;; the function name a define head introduces: (f ..)->f ; curried ((f a) b)->f
(define (head-name node)
  (cond [(an? node) (an-t node)]
        [(and (fm? node) (pair? (fm-kids node))) (head-name (car (fm-kids node)))]
        [else #f]))
;; names a node introduces into its ENCLOSING scope (define/define-values), for sibling threading
(define (defines-of n)
  (cond
    [(not (and (fm? n) (pair? (fm-kids n)) (an? (car (fm-kids n))))) '()]
    [else
     (define k  (hash-ref binder-kw (an-t (car (fm-kids n))) #f))
     (define c1 (and (pair? (cdr (fm-kids n))) (cadr (fm-kids n))))
     (case k
       [(define)        (cond [(an? c1) (list (an-t c1))]
                              [(fm? c1) (let ([nm (head-name c1)]) (if nm (list nm) '()))]
                              [else '()])]
       [(define-values) (if (fm? c1) (map an-t (filter an? (fm-kids c1))) '())]
       [else            '()])]))

;; ---- the walk ----
;; a body is a sequence of forms; define/define-values in it bind for the RIGHT-siblings.
(define (walk-body nodes env)
  (let loop ([ns nodes] [env env] [acc '()])
    (cond
      [(null? ns) acc]
      [else (loop (cdr ns) (env+ env (defines-of (car ns)))
                  (append acc (classify (car ns) env)))])))

(define (classify node env)
  (cond [(an? node) (list (ref-span node env))]
        [(fm? node) (classify-form node env)]
        [else '()]))

(define (classify-form f env)
  (define kids (fm-kids f))
  (cond
    [(null? kids) '()]
    [(not (an? (car kids))) (walk-body kids env)]        ; head is a form: ((..) ..)
    [else
     (define h (car kids))
     (define k (hash-ref binder-kw (an-t h) #f))
     (cond
       [(eq? k 'lam)                                  (do-lambda f env)]
       [(memq k '(let let* letrec))                   (do-let f env k)]
       [(memq k '(let-values let*-values letrec-values)) (do-let-values f env k)]
       [(eq? k 'define)                               (do-define f env)]
       [(eq? k 'define-values)                        (do-define-values f env)]
       [(for-kw? (an-t h))                            (do-for f env (an-t h))]
       [else                                          (do-app f env)])]))

(define (do-app f env)
  (define kids (fm-kids f))
  (append (classify (car kids) env) (walk-body (cdr kids) env)))   ; operands threaded (begin/when ...)

(define (do-lambda f env)
  (define kids (fm-kids f))
  (cond
    [(null? (cdr kids)) (list (kw-span (car kids)))]
    [else
     (define params (formal-atoms (cadr kids)))
     (append (list (kw-span (car kids))) (map binding-span params)
             (walk-body (cddr kids) (env+ env (map an-t params))))]))

;; let / let* / letrec, plus named let (let name (binds) body...)
(define (do-let f env k)
  (define kids (fm-kids f))
  (define h (car kids))
  (cond
    [(and (eq? k 'let) (pair? (cdr kids)) (an? (cadr kids)))     ; named let
     (define nm    (cadr kids))
     (define binds (and (pair? (cddr kids)) (caddr kids)))
     (define body  (if (pair? (cddr kids)) (cdddr kids) '()))
     (define-values (bspans names) (let-clauses binds env 'parallel))
     (append (list (kw-span h) (binding-span nm)) bspans
             (walk-body body (env+ env (cons (an-t nm) names))))]
    [else
     (define binds (and (pair? (cdr kids)) (cadr kids)))
     (define body  (if (pair? (cdr kids)) (cddr kids) '()))
     (define mode  (case k [(let) 'parallel] [(let*) 'seq] [(letrec) 'rec]))
     (define-values (bspans names) (let-clauses binds env mode))
     (append (list (kw-span h)) bspans (walk-body body (env+ env names)))]))

;; process a binder list ([x r]...); rhs env depends on mode; returns (values spans names)
(define (let-clauses binds env mode)
  (cond
    [(not (fm? binds)) (values '() '())]
    [else
     (define all (filter values (map (lambda (c) (let-values ([(n _) (clause-name+rhs c)]) (and n (an-t n))))
                                      (fm-kids binds))))
     (let loop ([cs (fm-kids binds)] [seen '()] [spans '()] [names '()])
       (cond
         [(null? cs) (values spans (reverse names))]
         [else
          (define-values (nm rhs) (clause-name+rhs (car cs)))
          (define renv (case mode [(parallel) env] [(seq) (env+ env seen)] [(rec) (env+ env all)]))
          (loop (cdr cs) (if nm (cons (an-t nm) seen) seen)
                (append spans (if nm (list (binding-span nm)) '()) (if rhs (classify rhs renv) '()))
                (if nm (cons (an-t nm) names) names))]))]))

;; let-values / let*-values / letrec-values: clause [(a b) rhs]
(define (do-let-values f env k)
  (define kids (fm-kids f))
  (define h (car kids))
  (define binds (and (pair? (cdr kids)) (cadr kids)))
  (define body  (if (pair? (cdr kids)) (cddr kids) '()))
  (define mode  (case k [(let-values) 'parallel] [(let*-values) 'seq] [(letrec-values) 'rec]))
  (cond
    [(not (fm? binds)) (append (list (kw-span h)) (walk-body body env))]
    [else
     (define all (append* (map (lambda (c) (let-values ([(ns _) (vclause-names+rhs c)]) (map an-t ns)))
                               (fm-kids binds))))
     (let loop ([cs (fm-kids binds)] [seen '()] [spans '()] [names '()])
       (cond
         [(null? cs) (append (list (kw-span h)) spans (walk-body body (env+ env names)))]
         [else
          (define-values (ns rhs) (vclause-names+rhs (car cs)))
          (define nss (map an-t ns))
          (define renv (case mode [(parallel) env] [(seq) (env+ env seen)] [(rec) (env+ env all)]))
          (loop (cdr cs) (append nss seen)
                (append spans (map binding-span ns) (if rhs (classify rhs renv) '()))
                (append nss names))]))]))

;; define: (define x rhs) | (define (f a..) body..) | curried ((f a) b)
(define (do-define f env)
  (define kids (fm-kids f))
  (define h  (car kids))
  (define c1 (and (pair? (cdr kids)) (cadr kids)))
  (define body (if (pair? (cdr kids)) (cddr kids) '()))
  (cond
    [(an? c1) (append (list (kw-span h) (binding-span c1)) (walk-body body env))]   ; x not in own rhs
    [(fm? c1) (define hatoms (all-atoms c1))                                        ; name + params bind in body
              (append (list (kw-span h)) (map binding-span hatoms)
                      (walk-body body (env+ env (map an-t hatoms))))]
    [else (list (kw-span h))]))

(define (do-define-values f env)
  (define kids (fm-kids f))
  (define h  (car kids))
  (define c1 (and (pair? (cdr kids)) (cadr kids)))
  (define rhs (and (pair? (cddr kids)) (caddr kids)))
  (define names (if (fm? c1) (filter an? (fm-kids c1)) '()))
  (append (list (kw-span h)) (map binding-span names) (if rhs (classify rhs env) '())))

;; for / for* / for/X: clause [id seq]; for* threads ids sequentially. (for/fold's accumulator
;; list is approximated -- its second clause-list is treated as body; noted as a known gap.)
(define (do-for f env kw)
  (define seq? (regexp-match? #px"^for[*]" kw))
  (define kids (fm-kids f))
  (define h (car kids))
  (define clauses (and (pair? (cdr kids)) (cadr kids)))
  (define body (if (pair? (cdr kids)) (cddr kids) '()))
  (cond
    [(not (fm? clauses)) (append (list (kw-span h)) (walk-body body env))]
    [else
     (let loop ([cs (fm-kids clauses)] [seen '()] [spans '()] [names '()])
       (cond
         [(null? cs) (append (list (kw-span h)) spans (walk-body body (env+ env names)))]
         [else
          (define-values (nm rhs) (clause-name+rhs (car cs)))
          (define renv (if seq? (env+ env seen) env))
          (loop (cdr cs) (if nm (cons (an-t nm) seen) seen)
                (append spans (if nm (list (binding-span nm)) '()) (if rhs (classify rhs renv) '()))
                (if nm (cons (an-t nm) names) names))]))]))

;; analyze a whole string -> (listof (list start end class)), sorted by position.
(define (analyze s) (sort (walk-body (parse s) '()) < #:key car))

;; ============================================================================
;; The MONOID: a reduced value + associative combine, so in-scope at a cut is a cached
;; read (O(depth)) rather than a re-parse.  Immutable frames; harvest to the nearest
;; binder frame.  (Tested token-aligned -- mid-atom splits would need the sexp head/tail
;; atom-merge, omitted here.)

;; a frame.  chs = its completed child FORMS (reversed).  A FORM = (list atom? head kids),
;; kids = (list (list atom? head)) of ITS children (one level -- enough to harvest names).
;; own  = this form's OWN binder names (params / let|for vars / named-let loop), live per its
;;        binder rule;  defs = names harvested from define-children in this form's body, live
;;        whenever present (a define is visible to its right-siblings).
(struct F (bkt kind idx head chs own defs) #:transparent)
;; sv: closes = (listof (cons bracket (listof form))) -- each dangling closer with the forms
;; that preceded it; lvl = bottom-level forms; opens = surviving frames innermost-first;
;; base = top-level (define ...) names (always in scope).
(struct sv (closes lvl opens base) #:transparent)

(define (kidinfo chs) (reverse (map (lambda (f) (list (first f) (second f))) chs)))   ; drop kids
(define (frame->form top) (list #f (or (F-head top) '?) (kidinfo (F-chs top))))

;; canonicalization: the read uses own/defs/base as SETS (membership), so their list order and
;; duplicates are meaning-irrelevant slack that breaks equal?.  Normalize to sorted, de-duped
;; sets; apply at every leaf/combine output so equal-meaning values are structurally equal?.
(define (sset xs) (sort (remove-duplicates (filter string? xs)) string<?))  ; real names only ('? is not one)
(define (canon-F f) (struct-copy F f [own (sset (F-own f))] [defs (sset (F-defs f))]))
(define (canon v)
  (sv (sv-closes v) (sv-lvl v) (map canon-F (sv-opens v)) (sset (sv-base v))))

;; the head atom's binder kind: the binder-kw table, or a for-comprehension keyword.
(define (atom-kind t)
  (or (hash-ref binder-kw t #f)
      (and (for-kw? t) (if (regexp-match? #px"^for[*]" t) 'for* 'for))))

(define (atom-kids form)     (for/list ([c (in-list (third form))] #:when (first c)) (second c)))       ; atom children
(define (bracket-heads form) (for/list ([c (in-list (third form))] #:when (not (first c))) (second c))) ; pair/clause heads
;; lambda formals: atom children (skip "." and #:kw markers) + bracket children's heads ([x def]).
(define (formal-names form)
  (append (for/list ([c (in-list (third form))]
                     #:when (and (first c) (not (equal? (second c) ".")) (not (kw-marker? (second c)))))
            (second c))
          (bracket-heads form)))

;; names a completed child FORM contributes to the frame's OWN binds, by (kind, child idx).
(define (own-harvest kind idx form)
  (cond
    [(and (eq? kind 'lam) (= idx 1))                          (formal-names form)]
    [(and (memq kind '(let let* letrec for for*)) (= idx 1))  (bracket-heads form)]
    [(and (eq? kind 'named-let) (= idx 2))                    (bracket-heads form)]
    [(and (eq? kind 'define) (= idx 1) (not (first form)))    (atom-kids form)]   ; (define (f a b) ..) -> a b
    [else '()]))
;; a completed child FORM that is a (define ...) adds its name to the ENCLOSING scope.
;; kids[0] is the head atom "define" (every head is nested as child-0), so the name is kids[1]'s
;; head: (define x ..)->x ; (define (f ..) ..)->f.
(define (define-name form)
  (if (and (not (first form)) (equal? (second form) "define") (>= (length (third form)) 2))
      (list (second (cadr (third form))))
      '()))

;; nest a completed child FORM into the innermost frame: append, bump idx, harvest own + defs,
;; and flip a 'let to 'named-let when its child-1 is an atom (the loop name).
(define (nest-head opens form)
  (define top0 (car opens))
  ;; head promotion: a headless frame (its `(` came from an earlier leaf) takes its head from
  ;; its first child atom -- mirrors scope-leaf setting the head when it reads it directly.
  (define top (if (and (not (F-head top0)) (= (F-idx top0) 0) (first form))
                  (struct-copy F top0 [head (second form)] [kind (atom-kind (second form))])
                  top0))
  (define i (F-idx top))
  (define-values (kind loop)
    (if (and (eq? (F-kind top) 'let) (= i 1) (first form))
        (values 'named-let (list (second form)))     ; (let loop ...) -- loop name is own
        (values (F-kind top) '())))
  (cons (struct-copy F top [kind kind] [chs (cons form (F-chs top))] [idx (add1 i)]
                     [own  (append loop (own-harvest kind i form) (F-own top))]
                     [defs (append (define-name form) (F-defs top))])
        (cdr opens)))

(define (scope-leaf s)
  (let loop ([tks (toks s)] [opens '()] [blvl '()] [closes '()] [base '()])
    (cond
      [(null? tks) (canon (sv (reverse closes) (reverse blvl) opens base))]
      [else
       (match-define (list ty _ _ t) (car tks))
       (case ty
         [(open) (loop (cdr tks) (cons (F (string-ref t 0) #f 0 #f '() '() '()) opens) blvl closes base)]
         [(close)
          (cond
            [(null? opens) (loop (cdr tks) '() '() (cons (cons (string-ref t 0) (reverse blvl)) closes) base)]
            [(pair? (cdr opens)) (loop (cdr tks) (nest-head (cdr opens) (frame->form (car opens))) blvl closes base)]
            [else (define form (frame->form (car opens)))
                  (loop (cdr tks) '() (cons form blvl) closes (append (define-name form) base))])]
         [(atom)
          (cond
            [(null? opens) (loop (cdr tks) '() (cons (list #t t '()) blvl) closes base)]
            [else
             (define top (car opens))
             (define top1 (if (F-head top) top (struct-copy F top [head t] [kind (atom-kind t)])))
             (loop (cdr tks) (nest-head (cons top1 (cdr opens)) (list #t t '())) blvl closes base)])])])))

;; combine: apply R's closers to L's stack (nesting each closer's preceding forms into the
;; frame it closes, then closing it), then nest R's bottom forms, then push R's opens.  Harvest
;; is local (nest-head); R's base names survive only if nothing of L's was left open to absorb them.
(define (scope+ L R)
  (match-define (sv Lc Ll Lo Lb) L)
  (match-define (sv Rc Rl Ro Rb) R)
  (let loop ([opens Lo] [blvl Ll] [base Lb] [rc Rc] [esc '()])
    (cond
      [(pair? rc)
       (match-define (cons _ heads) (car rc))
       (cond
         ;; closer with nothing left to close -> it stays dangling, but the bottom-forms that
         ;; precede it (blvl) must travel WITH it (they nest into whatever a left context closes).
         [(null? opens) (loop opens '() base (cdr rc)
                              (cons (cons (car (car rc)) (append blvl (cdr (car rc)))) esc))]
         [else
          (define os1 (for/fold ([o opens]) ([f (in-list heads)]) (nest-head o f)))     ; forms before the closer
          (cond
            [(pair? (cdr os1)) (loop (nest-head (cdr os1) (frame->form (car os1))) blvl base (cdr rc) esc)]
            [else (define form (frame->form (car os1)))
                  (loop '() (append blvl (list form)) (append (define-name form) base) (cdr rc) esc)])])]
      [else
       (define os1 (for/fold ([o opens]) ([f (in-list Rl)] #:when (pair? o)) (nest-head o f)))
       (canon
        (sv (append Lc (reverse esc))
            (if (null? os1) (append blvl Rl) blvl)
            (append Ro os1)
            (if (null? os1) (append Rb base) base)))])))

;; a frame's OWN binds go live once it is past its head into its body.
(define (body-start kind)
  (case kind
    [(named-let)                           3]    ; head, loop name, binder list, then body
    [(lam let let* letrec for for* define)  2]   ; define: head, (f a b), then body (params live there)
    [else                                  #f]))
(define (own-active? f) (let ([bs (body-start (F-kind f))]) (and bs (>= (F-idx f) bs))))
;; names declared so far by an OPEN binder-list frame (its closed bracket children's heads).
(define (open-bl-names f)
  (for/list ([c (in-list (F-chs f))] #:when (not (first c))) (second c)))
;; in-scope = base defines + each open frame's defs (always) + its own (when active) + the names
;; declared so far in an open let*/letrec binder list (visible in subsequent RHSs).
(define (sv-in-scope v)
  (let loop ([os (sv-opens v)] [acc (sv-base v)])
    (cond
      [(null? os) acc]
      [else
       (define f (car os))
       (define p (and (pair? (cdr os)) (cadr os)))
       (define a0 (append (F-defs f) acc))                            ; defines: always live
       (define a1 (if (own-active? f) (append (F-own f) a0) a0))      ; own: per binder rule
       (define a2 (if (and p (memq (F-kind p) '(let* letrec)) (= (F-idx p) 1))
                      (append (open-bl-names f) a1) a1))              ; sequential RHS: prior names
       (loop (cdr os) a2)])))

;; ============================================================================
(module+ test
  (require rackunit)
  (define (ws-chunks s) (regexp-match* #px"\\S+\\s*|\\s+" s))            ; token-aligned pieces
  (define (fold-leaf pieces) (foldl (lambda (c a) (scope+ a (scope-leaf c))) (scope-leaf "") pieces))
  (define corpus
    (list "(let ([x 1] [y 2]) (+ x y z))"
          "(lambda (a b) (cons a (f b q)))"
          "(let ([x 1]) (lambda (y) (+ x y w)))"
          "(let* ([x 1] [y x]) (+ x y))"
          "(letrec ([f 1] [g 2]) (f g h))"
          "(let ([a 1]) (let ([b 2]) (+ a b c)))"
          "(lambda (p) (let ([q 1]) (g p q r)))"
          ;; --- the added binders ---
          "(define x 1) (define y x) (+ x y z)"          ; top-level sequential defines
          "(define (f a b) (+ a b x))"                   ; function define -> a b in body, f enclosing
          "(lambda () (define h 1) (+ h k))"             ; internal define, visible to siblings
          "(let ([a 1]) (define b a) (+ a b c))"         ; define in a let body
          "(let loop ([i 0] [acc 1]) (+ i acc loop j))"  ; named let -- loop + vars in body
          "(for ([x xs] [y ys]) (+ x y z))"              ; for clauses bind in body, seqs are free
          "(letrec* ([p 1] [q p]) (+ p q r))"            ; letrec* (backward ref)
          "(lambda (a b . rest) (g a b rest c))"))       ; rest arg
  (define leaf-fails 0) (define fold-fails 0) (define refs 0)
  (for ([s (in-list corpus)])
    (for ([sp (in-list (analyze s))] #:when (memq (third sp) '(bound free)))
      (match-define (list a _ cls) sp)
      (define name (substring s a (cadr sp)))
      (set! refs (add1 refs))
      (define want (eq? cls 'bound))
      (define pre (substring s 0 a))
      (unless (equal? (and (member name (sv-in-scope (scope-leaf pre))) #t) want) (set! leaf-fails (add1 leaf-fails)))
      (unless (equal? (and (member name (sv-in-scope (fold-leaf (ws-chunks pre)))) #t) want)
        (set! fold-fails (add1 fold-fails))
        (printf "  FOLD FAIL: ~s | name=~s want=~a | prefix=~s | leaf-scope=~s fold-scope=~s\n"
                s name want pre (sv-in-scope (scope-leaf pre)) (sv-in-scope (fold-leaf (ws-chunks pre)))))))
  (printf "refs checked: ~a   leaf-fails: ~a   fold-fails (assoc): ~a\n" refs leaf-fails fold-fails)
  (check-equal? leaf-fails 0)
  (check-equal? fold-fails 0))

(module+ main
  (define (show label s)
    (printf "~a  ~s\n" label s)
    (for ([sp (in-list (analyze s))])
      (match-define (list a b cls) sp)
      (printf "   ~a [~a,~a) ~a\n" (~a (substring s a b) #:min-width 10) a b cls)))
  (show "let       " "(let ([x 1] [y 2]) (+ x y z))")
  (show "lambda    " "(lambda (a b) (cons a (f b q)))")
  (show "nested    " "(let ([x 1]) (lambda (y) (+ x y w)))")
  (show "let*      " "(let* ([x 1] [y x]) (+ x y))")
  (show "shadow    " "(let ([x 1]) (let ([x 2]) x))")

  (define wsample
    (string-append
     "(lambda (n step)\n"
     "  (let ([sum (+ n step)])\n"
     "    (let* ([a sum]\n"
     "           [b (* a n)])\n"
     "      (lambda (k)\n"
     "        (cons a (cons b (cons k missing)))))))"))
  (printf "\nWIDGET-DATA ~s\n" (cons wsample (analyze wsample)))

  ;; ---- timing: the in-scope READ at a deep cut, vs document size (expect flat = O(depth)) ----
  ;; Build the cut's summary by a token-aligned fold of the prefix (the monoid combine), then
  ;; time only sv-in-scope -- the per-cut read a cached rope would do.  (A real rope also needs
  ;; the sexp head/tail atom-merge for mid-atom leaf splits; omitted, so we fold token-aligned.)
  (define (ws-chunks s) (regexp-match* #px"\\S+\\s*|\\s+" s))
  (define (fold-leaf pieces) (foldl (lambda (c a) (scope+ a (scope-leaf c))) (scope-leaf "") pieces))
  (define depth 20)
  (define open (apply string-append (for/list ([i (in-range depth)]) (format "(let ([x~a ~a]) " i i))))
  (printf "\nin-scope READ at a depth-~a cut, by document size (us):\n" depth)
  (printf "~a  ~a  ~a  ~a\n" (~a "pad forms" #:min-width 10) (~a "chars" #:min-width 9)
          (~a "names" #:min-width 6) (~a "read ns/op" #:min-width 10))
  (for ([pad (in-list '(0 200 2000 20000))])
    (define prefix (string-append (apply string-append (make-list pad "(g a b) ")) open))
    (define summary (fold-leaf (ws-chunks prefix)))     ; the cut's cached summary (built untimed)
    (define n (length (sv-in-scope summary)))
    (sv-in-scope summary) (collect-garbage)
    (define-values (_r _c real _g)
      (time-apply (lambda () (for ([_ (in-range 300000)]) (sv-in-scope summary))) '()))
    (printf "~a  ~a  ~a  ~a\n" (~a pad #:min-width 10) (~a (string-length prefix) #:min-width 9)
            (~a n #:min-width 6) (~a (~r (/ (* real 1e6) 300000) #:precision 1) #:min-width 10))))
