#lang racket

(require redex)

(require "syntax.rkt")
(require "reduction.rkt")

; Os seguintes programas são termos da linguagem
(redex-match? Pierce t
  (term
    (pred false)
  ))

(redex-match? Pierce t
  (term
    (succ (iszero 0))
  ))

(redex-match? Pierce t
  (term
    (iszero true)
  ))

; Mas todos avaliam para termos stuck! Ou seja, são programas errôneos
(traces r_ctx (term
    (pred false)))

(traces r_ctx (term
    (succ (iszero 0))))

(traces r_ctx (term
    (iszero true)))

; Aos termos serão associados com os tipos dos possíveis resultados da avaliação dos programa
  ; Os tipos de resultados são: numéricos ou booleanos
  ; Alguns termos não tem tipos bem definidos, apesar de não levarem a erros (stuck) durante execução avaliação do programa
(traces r_ctx (term
  (if (iszero 0) 0 false)))

(define-extended-language TypedPierce Pierce
  [T ::= Nat Bool]
  [v ::= true false nv]
  [nv ::= 0 (succ nv)])

(define-judgment-form TypedPierce
  #:mode (: I O)
  #:contract (: t T)
  [----------------------- "true"
   (: true Bool)]

  [----------------------- "false"
   (: false Bool)]

  [(: t_1 Bool)
   (: t_2 T)
   (: t_3 T)
   ------------------------ "if"
   (: (if t_1 t_2 t_3) T)]

  [------------------------ "0"
   (: 0 Nat)]

  [(: t Nat)
   ----------------------- "succ"
   (: (succ t) Nat)]

  [(: t Nat)
   ----------------------- "pred"
   (: (pred t) Nat)]

  [(: t Nat)
   ----------------------- "iszero"
   (: (iszero t) Bool)])
  
(judgment-holds (: (iszero 0) Bool))
(judgment-holds (: (iszero 0) T) T)

(judgment-holds (: (pred false) T))
(judgment-holds (: (succ (iszero 0)) T))
(judgment-holds (: (iszero true) T))
(judgment-holds (:  (if (iszero 0) 0 false) T))

(show-derivations
   (build-derivations
    (: (if (iszero 0) 0 (pred 0)) T)))
