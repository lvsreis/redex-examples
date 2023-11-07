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
;(traces total-β-reduction (term ((λ x x) ((λ x x) (λ z ((λ x x) z))))))

#| Estratégias de Redução |#

(define-extended-language LambdaCtx Lambda
  [v ::= (λ x v) w]
  [w ::=  x (w v)]
  [C1 ::= (λ x C) C]
  [C ::= hole (C e) (w C1)])
     
(define normal-reduction (context-closure β-reduction LambdaCtx C1))
;(traces normal-reduction (term ((λ x x) ((λ x x) (λ z ((λ x x) z))))))
;(traces normal-reduction (term ((λ z ((λ x x) z)) x)))
;(traces normal-reduction (term ((x ((λ x x) y)) z)))
#;(traces normal-reduction (term
      ((λ x ((λ y y) (x ((λ z z) (λ w w))))) x)))
(traces normal-reduction (term
      (λ x ((λ y y) (x ((λ z z) (λ w w)))))))
#;(traces normal-reduction (term
 (((x ((λ x x) y) ) z) ((λ x x) z))))
#;(traces normal-reduction (term
 (y (λ x ((λ x x) z)))))
#;(traces normal-reduction (term
  (((λ x (λ y x)) (λ x x)) ((λ x (x x)) (λ x (x x))))))

(define-extended-language Call-by-name Lambda
  [v ::= (λ x e) w]
  [w ::=  x (w v)]
  [C ::= hole (C e) (w C)])
     
(define call-by-name-reduction (context-closure β-reduction Call-by-name C))
;(traces call-by-name-reduction (term ((λ x x) ((λ x x) (λ z ((λ x x) z))))))
;(traces call-by-name-reduction (term ((λ z ((λ x x) z)) x)))
;(traces call-by-name-reduction (term ((x ((λ x x) y)) z)))
#;(traces call-by-name-reduction (term
      ((λ x ((λ y y) (x ((λ z z) (λ w w))))) x)))
#;(traces call-by-name-reduction (term
      (λ x ((λ y y) (x ((λ z z) (λ w w)))))))
#;(traces call-by-name-reduction (term
 (((x ((λ x x) y) ) z) ((λ x x) z))))
#;(traces call-by-name-reduction (term
 (y (λ x ((λ x x) z)))))
#;(traces call-by-name-reduction (term
  (((λ x (λ y x)) (λ x x)) ((λ x (x x)) (λ x (x x))))))

(define-extended-language Call-by-value Lambda
  [v ::= (λ x e) w]
  [w ::=  x (w v)]
  [C ::= hole (C e) (v C)])


(define call-by-value-reduction (reduction-relation Call-by-value
  #:domain e
  (--> (in-hole C ((λ x e) v)) (in-hole C (subs x v e)))))

;(traces call-by-value-reduction (term ((λ x x) ((λ x x) (λ z ((λ x x) z))))))
;(traces call-by-value-reduction (term ((λ z ((λ x x) z)) x)))
;(traces call-by-value-reduction (term ((x ((λ x x) y)) z)))
#;(traces call-by-value-reduction (term
      ((λ x ((λ y y) (x ((λ z z) (λ w w))))) x)))
#;(traces call-by-value-reduction (term
      (λ x ((λ y y) (x ((λ z z) (λ w w)))))))
#;(traces call-by-value-reduction (term
 (((x ((λ x x) y) ) z) ((λ x x) z))))
#;(traces call-by-value-reduction (term
 (y (λ x ((λ x x) z)))))
#;(traces call-by-value-reduction (term
  (((λ x (λ y x)) (λ x x)) ((λ x (x x)) (λ x (x x))))))
