#lang racket

(require redex)

(define-language Pierce
  [T ::= true false 0
     (if T T T)
     (succ T)
     (pred T)
     (iszero T)])

(redex-match? Pierce T (term (if false 0 1)))
(redex-match? Pierce T (term (iszero (pred (succ 0)))))

(redex-match Pierce T (term (if false 0 (succ 0))))
(redex-match Pierce (if T_1 T_2 T_3) (term (if false 0 (succ 0))))

(redex-match Pierce (natural_1 ... natural_2 natural_3 ...) (term (1 2 3)))

(redex-let Pierce ([(if (iszero T_1) T_2 _) (term (if (iszero (succ 0)) (succ (pred 0)) 0))])
           (display "T1: ") (displayln (term T_1))
           (display "T2: ") (displayln (term T_2)))
