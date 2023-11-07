#lang racket

(require redex)

(require "syntax.rkt")

(provide (all-defined-out))

(define r
  (reduction-relation Pierce
     #:domain t
     #:codomain t
     (--> (if true t_1 t_2)
          t_1
          "if-true")
     (--> (if false t_1 t_2)
          t_2
          "if-false")
     (--> (iszero 0)
          true
          "=0")
     (--> (iszero (succ t))
          false
          "/=0")
     (--> (pred 0)
          0
          "pred0")
     (--> (pred (succ t))
          t
          "pred-succ")))

#;(stepper r (term
    (iszero (pred (succ 0)))))

#;(stepper r (term
    (if (iszero (succ 0)) (succ (pred 0)) 0)))

(define ->r (compatible-closure r Pierce t))

#;(stepper ->r (term
    (iszero (pred (succ 0)))))

#;(stepper ->r (term
    (if (iszero (succ 0)) (succ (pred 0)) 0)))

(define-extended-language PierceCtx Pierce
  [C ::= hole
     (if C t t)
     (succ C)
     (pred C)
     (iszero C)])

; definição da relação de redução usando contexto
(define r_ctx (context-closure r PierceCtx C))

#;(traces r_ctx (term
    (if (iszero (pred (succ 0))) (succ (pred 0)) 0)))

(define rw_ctx (reduction-relation PierceCtx
     #:domain t #:codomain t
     (==> (if true t_1 t_2) t_1
          "if-true")
     (==> (if false t_1 t_2) t_2
          "if-false")
     (==> (iszero 0) true "=0")
     (==> (iszero (succ t)) false "/=0")
     (==> (pred 0) 0 "pred0")
     (==> (pred (succ t)) t "pred-succ")
     with [(--> (in-hole C t1) (in-hole C t2))
           (==> t1 t2)]))

#;(traces rw_ctx (term
    (if (iszero (pred (succ 0))) (succ (pred 0)) 0)))
