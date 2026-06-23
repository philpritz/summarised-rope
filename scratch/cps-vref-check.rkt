#lang racket
;; CPS / Church-encoded store cell: raw takes a continuation `recv`, calls it
;; with the focus value(s) AND the put -- foci ride as multiple ARGS, not a slot.
(define ((vref* . is) v recv)
  (define vals (map (lambda (i) (vector-ref v i)) is))
  (define (put . xs) (let ([w (vector-copy v)])
                       (for ([i (in-list is)] [x (in-list xs)]) (vector-set! w i x))
                       w))
  (apply recv (append vals (list put))))           ; (recv a b ... put)

;; view: recv ignores put, returns the foci as MULTIPLE VALUES
(printf "view  ~s  (expect (a c))\n"
        (call-with-values (lambda () ((vref* 0 2) (vector 'a 'b 'c) (lambda (x z put) (values x z)))) list))
;; set: recv feeds MULTIPLE ARGS to put
(printf "set   ~s  (expect #(X b Z))\n"
        ((vref* 0 2) (vector 'a 'b 'c) (lambda (x z put) (put 'X 'Z))))
;; over: transform the foci, hand results to put
(printf "over  ~s  (expect #(A b C))\n"
        ((vref* 0 2) (vector 'a 'b 'c)
         (lambda (x z put) (put (string->symbol (string-upcase (symbol->string x)))
                                (string->symbol (string-upcase (symbol->string z)))))))
;; navigator: recv keeps foci AND put
(printf "keep  ~s  (expect (a c #t))\n"
        (call-with-values (lambda () ((vref* 0 2) (vector 'a 'b 'c) (lambda (x z put) (values x z (procedure? put))))) list))
