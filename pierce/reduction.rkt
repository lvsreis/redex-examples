#lang racket

(require redex)

(require "syntax.rkt")

(define r
  (reduction-relation Pierce
     #:domain T
     #:codomain T
     (--> (if true T_1 T_2)
          T_1
          "if-true")
     (--> (if false T_1 T_2)
          T_2
          "if-false")
     (--> (iszero 0)
          true
          "=0")
     (--> (iszero (succ T))
          false
          "/=0")
     (--> (pred 0)
          0
          "pred0")
     (--> (pred (succ T))
          T
          "pred-succ")))

#;(stepper r (term
    (iszero (pred (succ 0)))))

#;(stepper r (term
    (if (iszero (succ 0)) (succ (pred 0)) 0)))

(define ->r (compatible-closure r Pierce T))

#;(stepper ->r (term
    (iszero (pred (succ 0)))))

#;(stepper ->r (term
    (if (iszero (succ 0)) (succ (pred 0)) 0)))

(define-extended-language PierceCtx Pierce
  [C ::= hole
     (if C T T)
     (succ C)
     (pred C)
     (iszero C)])

; definição da relação de redução usando contexto
(define r_ctx (context-closure r PierceCtx C))

(traces r_ctx (term
    (if (iszero (pred (succ 0))) (succ (pred 0)) 0)))

(define rw_ctx (reduction-relation PierceCtx
     #:domain T #:codomain T
     (==> (if true T_1 T_2) T_1
          "if-true")
     (==> (if false T_1 T_2) T_2
          "if-false")
     (==> (iszero 0) true "=0")
     (==> (iszero (succ T)) false "/=0")
     (==> (pred 0) 0 "pred0")
     (==> (pred (succ T)) T "pred-succ")
     with [(--> (in-hole C t1) (in-hole C t2))
           (==> t1 t2)]))

#;(traces rw_ctx (term
    (if (iszero (pred (succ 0))) (succ (pred 0)) 0)))
