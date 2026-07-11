#lang racket

;; Stage: the staged / church-store optic -- a leaner successor to algebra.rkt's
;; store-shaped `opt`. A STAGE is a bare function, no struct:
;;
;;     ((f . idxs) . ws)  ->  (values g* put)
;;
;;   g*  : the CHURCH STORE -- (g* k) = ((k . rnds) . foci): it feeds its consumer
;;         the RENDERS first (a stage's configuration bus), then the FOCI (its
;;         worlds). Renders and foci are the two channels; the put path never sees
;;         the renders.
;;   put : news -> ws'   -- the write-back.
;;
;; Two curried application stages -- idxs (the configuration, from the previous
;; stage's renders) then ws (the world) -- so a plain stage is (pure world-fn) and
;; a configured stage binds its config in a parenthesised head via the experimental
;; curried `lambda`: (lambda ((cfg ...) w ...) ...).
;;
;; The whole calculus is application + composition:
;;   FORWARD  the bus is application -- (g1 f2): the next stage IS the consumer of
;;            the previous store, so composition is `(g1 f2)` plus threading puts.
;;   BACKWARD the puts thread by `compose` (values flow through it untouched).
;; The projections are consumers fed to g*, each ONE combinator:
;;   (g* (pure values)) = get      (g* pure) = view      (g* (pure put)) = recompose
;; -- `pure` on both sides: `pure` itself holds the renders (view); `(pure X)` skips
;; them and does X to the foci (get, recompose, update). LAW: (g* (pure put)) is the
;; recompose -- the identity for a lawful stage, the normalization e for a lossy one.
;;
;; The surface:
;;   compose-stage      stages, outer to inner; () = identity-stage
;;   identity-stage     the unit -- passes its indexes and worlds through
;;   enter              a world church-encoded: ((enter z) f) = (values g* put),
;;                      the pipeline entry ("no indexes first")
;;   recompose          (recompose f w ...) = the bare run -- the stage's own e
;;   stage-get stage-view stage-get*        the reads (foci / renders / both)
;;   stage-set stage-update                 the writes
;; Narrative and the derivation (why the peek is half an iso, why the renders lead,
;; the walk-count that motivated the single read): the design note.

(require "algebra.rkt")   ; pure, pass  (compose is racket's)

(provide compose-stage compose-stage2 identity-stage
         enter recompose
         stage-get stage-view stage-get* stage-set stage-update)

;; helper: run a function and reify its multiple values as a list (algebra keeps a
;; private copy; stage.rkt is self-contained above the toolbox aggregator).
(define (value-list f . args) (call-with-values (lambda () (apply f args)) list))

;; ---------- composition: the bus is application, the puts compose ----------
;; compose-stage2 b1 b2: run b1 at OUR indexes and the world, hand its store g1 the
;; next stage b2 (whose indexes are b1's renders, whose worlds are b1's foci), and
;; thread the two puts. The composite is itself a stage -- `lambda idxs` receives
;; ITS configuration from further out (empty at the entry).
(define ((compose-stage2 b1 b2) . idxs)
  (lambda ws
    (define-values (g1 put1) (apply (apply b1 idxs) ws))
    (define-values (g2 put2) (g1 b2))           ; the one load-bearing line
    (values g2 (compose put1 put2))))           ; compose threads the values

;; identity-stage: the unit -- foci = its worlds, renders = its indexes, put = values.
(define (identity-stage . idxs)
  (lambda ws (values (lambda (k) (apply (apply k idxs) ws)) values)))

(define (compose-stage . bs)
  (foldl (lambda (b acc) (compose-stage2 acc b)) identity-stage bs))

;; ---------- entry: a world, church-encoded ----------
;; (enter z) is a degenerate store -- no renders, the world focal: ((enter z) k) =
;; ((k) z). Applying it to a stage IS the pipeline entry, so "store applies consumer"
;; is the single interaction at every level (entry, between stages, projection).
(define (enter . ws) (compose (apply pass ws) (pass)))

;; ---------- projections: consumers fed to the store ----------
;; each opens the stage at no indexes (one read) and feeds g* a terminal consumer.
(define ((stage-get f) . ws)                    ; foci as values
  (define-values (g _put) ((apply enter ws) f))
  (g (pure values)))
(define ((stage-view f) . ws)                   ; renders as values
  (define-values (g _put) ((apply enter ws) f))
  (g pure))
(define ((stage-get* f) . ws)                   ; foci then renders
  (define-values (g _put) ((apply enter ws) f))
  (g (lambda rnds (lambda foci (apply values (append foci rnds))))))
(define ((stage-set f) . news)                  ; ignore g, feed the put
  (lambda ws
    (define-values (_g put) ((apply enter ws) f))
    (apply put news)))
(define ((stage-update f h) . ws)               ; h : (foci ... renders ...) -> news
  (define-values (g put) ((apply enter ws) f))
  (g (lambda rnds (lambda foci
       (apply (compose put h) (append foci rnds))))))
(define (recompose f . ws)                      ; the bare run = the stage's own e
  (define-values (g put) ((apply enter ws) f))
  (g (pure put)))

;; ============================================================================
(module+ test
  (require rackunit
           (submod "algebra.rkt" experimental))    ; the curried `lambda` for configured stages

  ;; a toy tower over integers, exercising the bus end to end:
  ;;   split10 (plain): world n -> focus (quotient n 10); RENDER (remainder n 10),
  ;;           which configures the next stage; put q -> q*10 (drops the remainder,
  ;;           so the stage is LOSSY -- recompose floors n to a multiple of 10).
  (define split10
    (pure (lambda (n)
            (values (lambda (c) ((c (remainder n 10)) (quotient n 10)))
                    (lambda (q) (* q 10))))))
  ;;   tag (configured by r): the focus q passes through; render = (list r sign q).
  (define tag
    (lambda ((r) q)
      (values (lambda (c) ((c (list r (if (negative? q) '- '+))) q))
              (lambda (q*) q*))))
  (define tower (compose-stage split10 tag))

  ;; --- reads: the data path is the foci; the bus rides the view (curried, like
  ;;     algebra's opt-get: (stage-get f) is a reusable reader over worlds) ---
  (check-equal? ((stage-get  tower) 47) 4)               ; quotient
  (check-equal? ((stage-view tower) 47) (list 7 '+))     ; r = remainder configured tag; sign of 4
  (check-equal? (call-with-values (lambda () ((stage-get* tower) 47)) list)
                (list 4 (list 7 '+)))                     ; foci then renders (tag's render is one list)

  ;; --- the config genuinely FLOWED: tag saw the remainder as its index ---
  (check-equal? ((stage-view tower) -53) (list -3 '-))   ; (remainder -53 10) = -3; q = -5 -> sign -

  ;; --- writes: the put path, renders never in it ---
  (check-equal? (((stage-set tower) 9) 47) 90)           ; put 9 -> 90, original ignored
  ;; update's h sees foci THEN renders (like opt-update); it returns the put's news
  (check-equal? ((stage-update tower (lambda (q _r) (add1 q))) 47) 50)  ; 4 -> 5 -> *10

  ;; --- recompose = e: floor to a multiple of 10, idempotent ---
  (check-equal? (recompose tower 47) 40)
  (check-equal? (recompose tower (recompose tower 47)) 40)

  ;; --- enter is a store: ((enter z) f) = (values g* put); one read serves all ---
  (define-values (g put) ((enter 47) tower))
  (check-equal? (g (pure values)) 4)                     ; get
  (check-equal? (g pure) (list 7 '+))                    ; view
  (check-equal? (g (pure put)) 40)                       ; recompose, by hand
  (check-equal? (put 9) 90)                              ; set, by hand

  ;; --- identity-stage is the unit of compose-stage ---
  (check-equal? ((stage-get (compose-stage)) 99) 99)
  (check-equal? ((stage-get (compose-stage split10)) 47) 4)   ; a singleton tower
  (check-equal? (recompose identity-stage 42) 42))
