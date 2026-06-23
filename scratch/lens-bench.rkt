#lang racket/base
;; Micro-bench: three lens encodings on a synthetic nested-vector structure, so
;; the LENS MACHINERY is what's timed (no zipper navigation to swamp it).
;;   store  -- coalgebra s -> (values focus put); composition = hand-threaded compose-lens (CURRENT)
;;   tagged -- van Laarhoven, Const a struct + inline if; composition = plain compose
;;   yoneda -- van Laarhoven, Const/Identity as lambdas; composition = plain compose
;; raw = hand-written get/set, the floor.
(require racket/vector
         racket/list)

;; ---------- (1) store coalgebra (current encoding) ----------
(define ((s:viewer l)   s) (let-values ([(a _)   (l s)]) a))
(define ((s:setter l x) s) (let-values ([(_ put) (l s)]) (put x)))
(define ((s:updater l f) s)(let-values ([(a put) (l s)]) (put (f a))))
(define (s:compose . ls)
  (foldr (lambda (outer rest)
           (lambda (s)
             (let*-values ([(a put-a) (outer s)]
                           [(x put-x) (rest a)])
               (values x (lambda (x*) (put-a (put-x x*)))))))
         (lambda (s) (values s (lambda (x) x)))
         ls))
(define (s:vref i)
  (lambda (v) (values (vector-ref v i)
                      (lambda (x) (let ([w (vector-copy v)]) (vector-set! w i x) w)))))

;; ---------- (2) van Laarhoven, tagged Const ----------
(struct const-box (v))
(define ((t:vl peek) k)
  (lambda (s) (let-values ([(a put) (peek s)])
                (let ([fa (k a)]) (if (const-box? fa) fa (put fa))))))
(define ((t:viewer l)   s) (const-box-v ((l const-box) s)))
(define ((t:setter l x) s) ((l (lambda (_) x)) s))
(define ((t:updater l f) s)((l f) s))
(define (t:vref i)
  (t:vl (lambda (v) (values (vector-ref v i)
                            (lambda (x) (let ([w (vector-copy v)]) (vector-set! w i x) w))))))

;; ---------- (3) van Laarhoven, Yoneda lambdas ----------
(define ((y:vl peek) k)
  (lambda (s) (let-values ([(a put) (peek s)])
                (lambda (h) ((k a) (compose h put))))))
(define (y:run fa) (fa (lambda (x) x)))
(define ((y:viewer l)   s) (y:run ((l (lambda (a) (lambda (h) a))) s)))
(define ((y:setter l x) s) (y:run ((l (lambda (_) (lambda (h) (h x)))) s)))
(define ((y:updater l f) s)(y:run ((l (lambda (a) (lambda (h) (h (f a))))) s)))
(define (y:vref i)
  (y:vl (lambda (v) (values (vector-ref v i)
                            (lambda (x) (let ([w (vector-copy v)]) (vector-set! w i x) w))))))

;; ---------- nested 2-vectors of given depth; the all-0 path bottoms at 41 ----------
(define (make-nest d) (if (= d 0) 41 (vector (make-nest (sub1 d)) (make-nest (sub1 d)))))
(define (raw-view s d) (let loop ([s s] [d d]) (if (= d 0) s (loop (vector-ref s 0) (sub1 d)))))
(define (raw-set s d x)
  (if (= d 0) x (let ([w (vector-copy s)]) (vector-set! w 0 (raw-set (vector-ref s 0) (sub1 d) x)) w)))

;; ---------- timing ----------
(define N 5000000)
(define (bench label f)
  (collect-garbage) (collect-garbage) (collect-garbage)
  (printf "~a " label)
  (define acc (time (for/fold ([a 0]) ([i (in-range N)]) (+ a (f i)))))
  (void acc))

(define (run-depth d)
  (printf "\n=== depth ~a  (compose of ~a vref lenses, N=~a) ===\n" d d N)
  (define dat (make-nest d))
  (define sL (apply s:compose (make-list d (s:vref 0))))
  (define tL (apply compose   (make-list d (t:vref 0))))
  (define yL (apply compose   (make-list d (y:vref 0))))
  (define sv (s:viewer sL)) (define ss (s:setter sL 99)) (define su (s:updater sL add1))
  (define tv (t:viewer tL)) (define ts (t:setter tL 99)) (define tu (t:updater tL add1))
  (define yv (y:viewer yL)) (define ys (y:setter yL 99)) (define yu (y:updater yL add1))
  ;; correctness gate
  (unless (and (= (sv dat) 41) (= (tv dat) 41) (= (yv dat) 41)) (error "view mismatch"))
  (unless (and (= (raw-view (ss dat) d) 99) (= (raw-view (ts dat) d) 99) (= (raw-view (ys dat) d) 99))
    (error "set mismatch"))
  (unless (and (= (raw-view (su dat) d) 42) (= (raw-view (tu dat) d) 42) (= (raw-view (yu dat) d) 42))
    (error "update mismatch"))
  (bench "store  view" (lambda (i) (sv dat)))
  (bench "tagged view" (lambda (i) (tv dat)))
  (bench "yoneda view" (lambda (i) (yv dat)))
  (bench "raw    view" (lambda (i) (raw-view dat d)))
  (bench "store  set " (lambda (i) (raw-view (ss dat) d)))
  (bench "tagged set " (lambda (i) (raw-view (ts dat) d)))
  (bench "yoneda set " (lambda (i) (raw-view (ys dat) d)))
  (bench "raw    set " (lambda (i) (raw-view (raw-set dat d 99) d)))
  (bench "store  upd " (lambda (i) (raw-view (su dat) d)))
  (bench "tagged upd " (lambda (i) (raw-view (tu dat) d)))
  (bench "yoneda upd " (lambda (i) (raw-view (yu dat) d))))

(printf "correctness ok; timing...\n")
(run-depth 1)
(run-depth 3)
(run-depth 6)
