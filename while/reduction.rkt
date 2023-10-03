#lang racket

(require redex)

(require "syntax.rkt")

(define-extended-language s-While While
  [s ::= (((x natural) ...) S) ((x natural) ...)]
  [v ::= natural])


(define r (reduction-relation s-While
  #:domain s
  (--> (any skip) any)
  (--> (((x_1 natural_1) ... (x natural) (x_2 natural_2) ...) (:= x natural_3)) ((x_1 natural_1) ... (x natural_3) (x_2 natural_2) ...))
  (--> ((((name xs x_!_) natural) ...) (:= (name x1 x_!_) natural_1)) ((x1 natural_1) (xs natural) ...))
  (--> (any (if true S_1 S_2)) (any S_1))
  (--> (any (if false S_1 S_2)) (any S_2))
  (--> (any (while B S)) (any (if B (S (while B S)) skip)))
  ; regra básica da sequência
  (--> (any (skip S)) (any S))
  (--> (((x_1 natural_1) ... (x natural) (x_2 natural_2) ...) ((:= x natural_3) S)) (((x_1 natural_1) ... (x natural_3) (x_2 natural_2) ...) S))
  (--> ((((name xs x_!_) natural) ...) ((:= (name x1 x_!_) natural_1) S)) (((x1 natural_1) (xs natural) ...) S))
  ))

;(traces r (term (() skip)))
;(traces r (term (() (:= x 5))))
;(traces r (term (((y 2) (x 3) (z 4)) (:= x 5))))
;(traces r (term (() (if true skip (:= x 7)))))
;(traces r (term (() (if false skip (:= x 7)))))
;(traces r (term (() (while true skip))))
;(traces r (term (() (while false skip))))
;(traces r (term (((x 0)) ((:= x 3) skip))))
;(traces r (term (() ((:= x 3) skip))))

(define ->r (compatible-closure r s-While S))
#;(traces ->r (term (((x 5) (y 7))
                   (((:= z x) (:= x y)) (:= y z)))))
