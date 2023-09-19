#lang racket

(require redex)

(require "syntax.rkt")

(define r
  (reduction-relation Logic
     #:domain L
     #:codomain L
     (--> (¬ ⊤)
          ⊥
          "neg1")
     (--> (¬ ⊥)
          ⊤
          "neg2")
     (--> (∧ ⊤ L)
          L
          "conj1")
     #;(--> (∧ L ⊤)
          L
          "conj2")
     (--> (∧ ⊥ L)
          ⊥
          "conj3")
     #;(--> (∧ L ⊥)
          ⊥
          "conj4")
     (--> (∨ ⊤ L)
          ⊤
          "disj1")
     #;(--> (∨ L ⊤)
          ⊤
          "disj2")
     (--> (∨ ⊥ L)
          L
          "disj3")
     #;(--> (∨ L ⊥)
          L
          "disj4")
     (--> (→ ⊤ L)
          L
          "cond1")
     #;(--> (→ L ⊤)
          ⊤
          "cond2")
     (--> (→ ⊥ L)
          ⊤
          "cond3")
     #;(--> (→ L ⊥)
          (¬ L)
          "cond4")
     (--> (↔ ⊤ L)
          L
          "bic1")
     #;(--> (↔ L T)
          L
          "bic2")
     (--> (↔ ⊥ L)
          (¬ L)
          "bic3")
     #;(--> (↔ L ⊥)
          (¬ L)
          "bic4")))

#;(stepper r (term
    (∨ (→ ⊤ ⊥) (¬ (∧ ⊥ ⊤)))))

#;(stepper r (term
    (∨ (→ ⊤ ⊥) (∧ (→ (¬ ⊤) ⊥) (¬ ⊤))))) 

(define ->r
  (compatible-closure r Logic L))

#;(stepper ->r (term
    (∨ (→ ⊤ ⊥) (¬ (∧ ⊥ ⊤)))))

#;(stepper ->r (term
    (∨ (→ ⊤ ⊥) (∧ (→ (¬ ⊤) ⊥) (¬ ⊤))))) 
