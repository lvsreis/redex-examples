#lang racket

(require redex)

(define-language WExpr
  [A ::= natural x
     (+ A A)
     (* A A)
     (- A A)]
  [B ::= true false
     (= A A)
     (<= A A)
     (& B B)
     (not B)]
  [x y z ::= variable-not-otherwise-mentioned])

(define-extended-language While WExpr
  [S ::= skip
     (:= x A)
     (S S)
     (if B S S)
     (while B S)])