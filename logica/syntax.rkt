#lang racket

(require redex)

(define-language Logic
  [L ::= ⊤ ⊥ p
     (¬ L)
     (∧ L L)
     (∨ L L)
     (→ L L)
     (↔ L L)]
  [p q r ::= variable-not-otherwise-mentioned])
