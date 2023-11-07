#lang racket

(require redex)

(define-language TLambda
  [e ::= x v (if e e e)
     (- e e) (iszero e)
     (e e) (λ x t e)]
  [t ::= Nat Bool (-> t t)]
  [v ::= true false natural]
  [x ::= variable-not-otherwise-mentioned]

  ; Contexto (call-by-name)
  [vw ::= (λ x t e) w]
  [w ::= x v (w vw)]
  [C ::= hole (- C e) (- v C) (iszero C)
     (if C e e) (C e) (w C)]

  ; Contexto de tipos
  [Γ ::= • ((x T) Γ)]
  
  #:binding-forms
  (λ x t e #:refers-to x))
     
(define r (reduction-relation TLambda
  #:domain e
  (--> ((λ x t e) e_1) (substitute e x e_1))
  (--> (if true e_1 e_2) e_1)
  (--> (if false e_1 e_2) e_2)
  (--> (iszero natural) ,(if (zero? (term natural)) (term true) (term false)))
  (--> (- natural_1 natural_2) ,(- (term natural_1) (term natural_2)))))

(define rw (context-closure r TLambda C))

;(traces rw (term ((λ x Nat x) ((λ x Nat x) (λ z Nat ((λ x Nat x) z))))))
;(traces rw (term ((λ z Nat ((λ x Nat x) z)) 1)))
;(traces rw (term ((x ((λ x Nat x) y)) z)))
#;(traces rw (term
      ((λ x Nat ((λ y Nat y) (x ((λ z Nat z) (λ w Nat w))))) x)))
#;(traces rw (term
      (λ x Nat ((λ y Nat y) (x ((λ z Nat z) (λ w Nat w)))))))
#;(traces rw (term
 (((x ((λ x Nat x) y) ) z) ((λ x Nat x) z))))
#;(traces rw (term
 (y (λ x Nat ((λ x Nat x) z)))))
#;(traces rw (term
  (((λ x (-> Nat Nat) (λ y Nat x)) (λ x Nat x)) ((λ x Nat (x x)) (λ x Nat (x x))))))

#;(traces rw (term
   (if ((λ x Bool (iszero x)) 0) (- 3 2) 2)))

;(define an-e1 (generate-term TLambda e 10))

#;(traces rw (term
    (λ x Nat ((λ x Bool x) x))))

;(traces rw an-e1)
  
  
