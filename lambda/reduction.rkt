#lang racket

(require redex)

(require "syntax.rkt")

; redução β-reduction
(define β-reduction (reduction-relation Lambda
  #:domain e
  (--> ((λ x e) e_1) (subs x e_1 e))))                  

             ;(λ z .λ x .x z ) y
;(traces β-reduction (term ((λ z (λ x (x z))) y)))
             ;(λ y .(λ x .y x )) ((λ y .x ) y )
;(traces β-reduction (term ((λ y (λ x (y x))) ((λ y x) y))))
             ;(λ x .λ y .(x y ) y ) z
;(traces β-reduction (term ((λ x (λ y ((x y) y))) z)))
             ;((λ f .(f λ f .f )) λ s .(s s ))
;(traces β-reduction (term ((λ f (f (λ f f))) (λ s (s s)))))


; fecho compatível da β-reduction é a β-redução total
(define total-β-reduction (compatible-closure β-reduction Lambda e))

              ;(λ x .x ) ((λ x .x ) (λ z .((λ x .x ) z )))

;(traces β-reduction (term ((λ x x) ((λ x x) (λ z ((λ x x) z))))))
(traces total-β-reduction (term ((λ x x) ((λ x x) (λ z ((λ x x) z))))))
