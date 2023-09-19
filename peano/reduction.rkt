#lang racket

(require redex)

(require "syntax.rkt")

(define ->
  (reduction-relation Peano
     #:domain P
     #:codomain P
     (--> (+ P 0)
          P
          "↦0")
     (--> (+ P_1 (s P_2))
          (s (+ P_1 P_2))
          "↦s")))

#;(apply-reduction-relation ->
    (term (+ (+ (s 0) 0) 0)))

#;(apply-reduction-relation* ->
     (term (+ (+ (s 0) 0) 0)))

#;(traces ->
     (term (+ (+ (s 0) 0) 0)))

#;(stepper ->
     (term (+ (+ (s 0) 0) 0)))

#;(stepper ->
     (term (+ (+ (s (s 0)) (s 0)) 0)))

(redex-match? Peano (+ P 0)
              (term (s (+ (s (s 0)) 0))))
                             
(redex-match? Peano (+ P_1 (s P_2))
              (term (s (+ (s (s 0)) 0))))              

(define ->r
  (compatible-closure -> Peano P))

#;(stepper ->r
     (term (+ (+ (s (s 0)) (s 0)) 0)))
