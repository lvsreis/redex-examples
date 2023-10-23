#lang racket

(require redex)

(require "syntax.rkt")

#|
values = natural

00) ⟨Δ, n⟩ ⟶ n
01) ⟨Δ, (+ n₁ n₂)⟩ ⟶ ⟨Δ, n₁ + n₂⟩
02) ⟨Δ, (* n₁ n₂)⟩ ⟶ ⟨Δ, n₁ * n₂⟩
03) ⟨Δ, (- n₁ n₂)⟩ ⟶ ⟨Δ, n₁ - n₂⟩
04) ⟨Δ, x⟩ ⟶ ⟨Δ, Δ[[x]]⟩

05) ⟨Δ, true⟩ ⟶ true
06) ⟨Δ, false⟩ ⟶ false
07) ⟨Δ, (= n₁ n₁)⟩ ⟶ ⟨Δ, true⟩
08) ⟨Δ, (= n₁ n₂)⟩ ⟶ ⟨Δ, false⟩, se n₁ ≠ n₂
09) ⟨Δ, (<= n₁ n₂)⟩ ⟶ ⟨Δ, n₁ ≤ n₂⟩
10) ⟨Δ, (& false B)⟩ ⟶ ⟨Δ, false⟩
11) ⟨Δ, (& true B)⟩ ⟶ ⟨Δ, B⟩
12) ⟨Δ, (not true)⟩ ⟶ ⟨Δ, false⟩
13) ⟨Δ, (not false)⟩ ⟶ ⟨Δ, true⟩

14) ⟨Δ, (+ A₁ A₂)⟩ ⟶ ⟨Δ, n A₂⟩, se ⟨Δ, A₁⟩ ⟶ n
15) ⟨Δ, (+ A₁ A₂)⟩ ⟶ ⟨Δ, A₃ A₂⟩, se ⟨Δ, A₁⟩ ⟶ ⟨Δ, A₃⟩
16) ⟨Δ, (+ n₁ A₂)⟩ ⟶ ⟨Δ, n₁ n₂⟩, se ⟨Δ, A₂⟩ ⟶ n₂
17) ⟨Δ, (+ n₁ A₂)⟩ ⟶ ⟨Δ, n₁ A₃⟩, se ⟨Δ, A₂⟩ ⟶ ⟨Δ, A₃⟩
... igual para *, -, = e <=

|#


(define-extended-language s-While While
  [s ::= (((x natural) ...) S) ((x natural) ...)]
  [v ::= natural])


(define r (reduction-relation s-While
  #:domain s
  (--> (any skip) any)
  (--> (((x_1 natural_1) ... (x natural) (x_2 natural_2) ...) (:= x natural_3)) ((x_1 natural_1) ... (x natural_3) (x_2 natural_2) ...))
  (--> ((((name xs x_!_) natural) ...) (:= (name x1 x_!_) natural_1)) ((x1 natural_1) (xs natural) ...))
  (--> (any (if true S_1 S_2)) (any S_1))
  (--> (any (if false S_1 S_2)) (any S_2))
  (--> (any (while B S)) (any (if B (S (while B S)) skip)))
  ; regra básica da sequência
  (--> (any (skip S)) (any S))
  (--> (((x_1 natural_1) ... (x natural) (x_2 natural_2) ...) ((:= x natural_3) S)) (((x_1 natural_1) ... (x natural_3) (x_2 natural_2) ...) S))
  (--> ((((name xs x_!_) natural) ...) ((:= (name x1 x_!_) natural_1) S)) (((x1 natural_1) (xs natural) ...) S))
  ))

;(traces r (term (() skip)))
;(traces r (term (() (:= x 5))))
;(traces r (term (((y 2) (x 3) (z 4)) (:= x 5))))
;(traces r (term (() (if true skip (:= x 7)))))
;(traces r (term (() (if false skip (:= x 7)))))
;(traces r (term (() (while true skip))))
;(traces r (term (() (while false skip))))
;(traces r (term (((x 0)) ((:= x 3) skip))))
;(traces r (term (() ((:= x 3) skip))))

(define ->r (compatible-closure r s-While S))
#;(traces ->r (term (((x 5) (y 7))
                   (((:= z x) (:= x y)) (:= y z)))))
