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

#;(traces ->r (term
    (∨ (→ ⊤ ⊥) (∧ (→ (¬ ⊤) ⊥) (¬ ⊤)))))

#| Especificando Redução com Contexto |#

(define-extended-language LogicCtx Logic
  [C ::= hole
     (¬ C)
     (∧ C L)
     (∨ C L)
     (→ C L)
     (↔ C L)])

; relação de redução usando o context-closure
(define r_ctx (context-closure r LogicCtx C))

#;(traces r_ctx (term
    (∨ (→ ⊤ ⊥) (¬ (∧ ⊥ ⊤)))))

#;(traces r_ctx (term
    (∨ (→ ⊤ ⊥) (∧ (→ (¬ ⊤) ⊥) (¬ ⊤)))))

(define rw_ctx
  (reduction-relation LogicCtx
     #:domain L
     #:codomain L
     (==> (¬ ⊤) ⊥ "neg⊤")
     (==> (¬ ⊥) ⊤ "neg⊥")
     (==> (∧ ⊤ L) L "conj⊤")
     (==> (∧ ⊥ L) ⊥ "conj⊥")
     (==> (∨ ⊤ L) ⊤ "disj⊤")
     (==> (∨ ⊥ L) L  "disj⊥")
     (==> (→ ⊤ L)  L "cond⊤")
     (==> (→ ⊥ L) ⊤ "cond⊥")
     (==> (↔ ⊤ L) L "bic⊤")
     (==> (↔ ⊥ L) (¬ L) "bic⊥")
  with [(--> (in-hole C t1) (in-hole C t2))
        (==> t1 t2)]))

#;(traces rw_ctx (term
    (∨ (→ ⊤ ⊥) (¬ (∧ ⊥ ⊤)))))

#;(traces rw_ctx (term
    (∨ (→ ⊤ ⊥) (∧ (→ (¬ ⊤) ⊥) (¬ ⊤)))))
