#lang racket

(require redex)

(require "syntax.rkt")

(define r
  (reduction-relation Pierce
     #:domain T
     #:codomain T
     (--> (if true T_1 T_2)
          T_1
          "if-true")
     (--> (if false T_1 T_2)
          T_2
          "if-false")
     (--> (iszero 0)
          true
          "=0")
     (--> (iszero (succ T))
          false
          "/=0")
     (--> (pred 0)
          0
          "pred0")
     (--> (pred (succ T))
          T
          "pred-succ")))

#;(stepper r (term
    (iszero (pred (succ 0)))))

#;(stepper r (term
    (if (iszero (succ 0)) (succ (pred 0)) 0)))

#;(define ->r (compatible-closure r Pierce T))

#;(stepper ->r (term
    (iszero (pred (succ 0)))))

(stepper ->r (term
    (if (iszero (succ 0)) (succ (pred 0)) 0)))
