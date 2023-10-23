#lang racket

(require redex)

(require "syntax.rkt")

; redução β-reduction
(define β-reduction (reduction-relation Lambda
  #:domain e
  (--> ((λ x e) e_1) (subs x e_1 e))))

(define-extended-language LambdaCtx Lambda
  [v ::= w (λ x v)]
  [w ::= x (w v)]
  [C1 ::= (λ x C) C]
  [C ::= hole (C e) (w C1)])

(define normal-reduction (context-closure β-reduction LambdaCtx C1))

;(traces normal-reduction (term ((λ x x) ((λ x x) (λ z ((λ x x) z))))))
;(traces normal-reduction (term ((λ z ((λ x x) z)) x)))
;(traces normal-reduction (term ((x ((λ x x) y)) z)))
#;(traces normal-reduction (term
      ((λ x ((λ y y) (x ((λ z z) (λ w w))))) x)))
#;(traces normal-reduction (term
      (λ x ((λ y y) (x ((λ z z) (λ w w)))))))
#;(traces normal-reduction (term
 (((x ((λ x x) y) ) z) ((λ x x) z))))
#;(traces normal-reduction (term
 (y (λ x ((λ x x) z)))))
#;(traces normal-reduction (term
  (((λ x (λ y x)) (λ x x)) ((λ x (x x)) (λ x (x x))))))

  