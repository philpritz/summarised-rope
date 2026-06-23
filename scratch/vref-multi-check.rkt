#lang racket
(require "../helper-algebras.rkt")

;; variadic vref: 1 index -> bare focus (current behaviour); 2+ -> list focus (product lens)
(define (vrefN . is)
  (make-lens
   (lambda (v)
     (define (rd i) (vector-ref v i))
     (define (wr xs) (let ([w (vector-copy v)])
                       (for ([i (in-list is)] [x (in-list xs)]) (vector-set! w i x))
                       w))
     (if (null? (cdr is))
         (values (rd (car is)) (lambda (x) (wr (list x))))   ; bare
         (values (map rd is)   wr)))))                        ; list

(printf "single view ~s  (expect b)\n"        ((viewer (vrefN 1)) (vector 'a 'b 'c)))
(printf "single set  ~s  (expect #(a x c))\n" ((setter (vrefN 1) 'x) (vector 'a 'b 'c)))
(printf "multi  view ~s  (expect (a c))\n"     ((viewer (vrefN 0 2)) (vector 'a 'b 'c)))
(printf "multi  set  ~s  (expect #(x b y))\n" ((setter (vrefN 0 2) (list 'x 'y)) (vector 'a 'b 'c)))
(printf "multi  over ~s  (expect #(A b C))\n" ((updater (vrefN 0 2) (lambda (xs) (map (lambda (s) (string->symbol (string-upcase (symbol->string s)))) xs))) (vector 'a 'b 'c)))

;; multiple-VALUES / multiple-ARGS ergonomics, as thin spread/gather over the list focus:
(define (view* l s)    (apply values ((viewer l) s)))
(define ((set*  l . xs) s) ((setter l xs) s))
(printf "view* ~s  (expect two values a c)\n" (call-with-values (lambda () (view* (vrefN 0 2) (vector 'a 'b 'c))) list))
(printf "set*  ~s  (expect #(x b y))\n"        ((set* (vrefN 0 2) 'x 'y) (vector 'a 'b 'c)))
