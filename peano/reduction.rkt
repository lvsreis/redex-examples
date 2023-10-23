#lang racket

(require redex)

(require "syntax.rkt")

(define r
  (reduction-relation Peano
     #:domain P
     #:codomain P
     (--> (+ P 0)
          P
          "↦0")
     (--> (+ P_1 (s P_2))
          (s (+ P_1 P_2))
          "↦s")))

#;(apply-reduction-relation r
    (term (+ (+ (s 0) 0) 0)))

#;(apply-reduction-relation* r
     (term (+ (+ (s 0) 0) 0)))

#;(traces r
     (term (+ (+ (s 0) 0) 0)))

#;(stepper r
     (term (+ (+ (s 0) 0) 0)))

#;(stepper r
     (term (+ (+ (s (s 0)) (s 0)) 0)))

(redex-match? Peano (+ P 0)
              (term (s (+ (s (s 0)) 0))))
                             
(redex-match? Peano (+ P_1 (s P_2))
              (term (s (+ (s (s 0)) 0))))              

(define ->r
  (compatible-closure r Peano P))

#;(stepper ->r
     (term (+ (+ (s (s 0)) (s 0)) 0)))


#| Definição de Contexto |#
(define-extended-language PeanoCtx Peano
  (C ::= hole
     (s C)
     (+ P C)
     #;(+ C P)))

(define s1
  (term (s hole)))

(redex-match PeanoCtx C s1)

(define s2
  (term (+ (s (s 0)) 0)))

(redex-match? PeanoCtx P
              (term (in-hole ,s1 ,s2)))

(redex-match PeanoCtx (in-hole C P)
   (term (s (+ (s (s 0)) 0))))

(redex-match PeanoCtx (in-hole C (+ P 0))
   (term (s (+ (s (s 0)) 0))))

#| criando um fecho compatível usando contexto  |#
(define ->>r (context-closure r PeanoCtx C))

(traces ->>r (term (+ (+ (s (s 0)) (s 0)) 0)))


#| Definindo uma relação de redução usando contexto e o in-hole |#
(define r_ctx (reduction-relation PeanoCtx
   #:domain P
   #:codomain P
   (--> (in-hole C (+ P 0))
        (in-hole C P)
        "↦0")
   (--> (in-hole C (+ P_1 (s P_2)))
        (in-hole C (s (+ P_1 P_2)))
        "↦s")))

(traces r_ctx (term (+ (+ (s (s 0)) (s 0)) 0)))

#| Definindo uma relação de redução usando contexto usando shortcuts with |#
(define rw_ctx (reduction-relation PeanoCtx
   #:domain P
   #:codomain P
   (==> (+ P 0) P
        "↦0")
   (==> (+ P_1 (s P_2)) (s (+ P_1 P_2))
        "↦s")
  with
  [(--> (in-hole C a) (in-hole C b))
   (==> a b)]))

;(traces rw_ctx (term (+ (+ (s (s 0)) (s 0)) 0)))
