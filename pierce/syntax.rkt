#lang racket

(require redex)

(provide (all-defined-out))

(define-language Pierce
  [t ::= true false 0
     (if t t t)
     (succ t)
     (pred t)
     (iszero t)])

(redex-match? Pierce t (term (if false 0 1)))
(redex-match? Pierce t (term (iszero (pred (succ 0)))))

(redex-match Pierce t (term (if false 0 (succ 0))))
(redex-match Pierce (if t_1 t_2 t_3) (term (if false 0 (succ 0))))

(redex-match Pierce (natural_1 ... natural_2 natural_3 ...) (term (1 2 3)))

(redex-let Pierce ([(if (iszero t_1) t_2 _) (term (if (iszero (succ 0)) (succ (pred 0)) 0))])
           (display "t1: ") (displayln (term t_1))
           (display "t2: ") (displayln (term t_2)))
