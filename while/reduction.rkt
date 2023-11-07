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

(define-extended-language WhileCtx While
  [env ::= ((x natural) ...)] ; Definição do Ambiente (Environment)
  [v ::= natural true false]  ; valores
  [state ::= (env S)]         ; estado da máquina
  [s ::= state env]           ; resultado da computação small-step
  [E ::= hole                 ; contexto de redução
     (+ E A) (* E A) (- E A) (+ v E) (* v E) (- v E)
     (= E A) (<= E A) (& E B) (not E) (= v E) (<= v E) (& v E)
     (:= x E) (E S) (if E S S) #;(while E S)])

(define-metafunction WhileCtx
  update-env : env x v -> env
  [(update-env ((x_1 v_1) ... (x v) (x_2 v_2) ...) x v_new) ((x_1 v_1) ... (x v_new) (x_2 v_2) ...)]
  [(update-env ((x_1 v_1) ...) x v) ((x v) (x_1 v_1) ...)])

(define r (reduction-relation WhileCtx
   #:domain state
   #:codomain s
   ; Semântica dos Comandos
   (--> [env skip] env)                                 ; ⟨Δ, skip⟩ ⟶ Δ
   (--> [env (in-hole E (:= x v))]                      ; ⟨Δ, := x v⟩ ⟶ ⟨Δ[[x / x]], skip⟩
        [(update-env env x v) (in-hole E skip)])
   (--> [env (in-hole E (skip S))]                      ; ⟨Δ, skip ; S⟩ ⟶ ⟨Δ, S⟩
        [env (in-hole E S)])
   (--> [env (in-hole E (if true S_1 S_2))]             ; ⟨Δ, if true S₁ S₂⟩ ⟶ ⟨Δ, S₁⟩
        [env (in-hole E S_1)])
   (--> [env (in-hole E (if false S_1 S_2))]            ; ⟨Δ, if false S₁ S₂⟩ ⟶ ⟨Δ, S₂⟩
        [env (in-hole E S_2)])
   (--> [env (in-hole E (while B S))]                   ; ⟨Δ, while B S⟩ ⟶ ⟨Δ, if B then (S; while B S) else skip⟩
        [env (in-hole E (if B (S (while B S)) skip))])
   ; Semântica das expressões
   (--> [((x_1 v_1) ... (x v) (x_2 v_2) ...) (in-hole E x)]         ; ⟨Δ, x⟩ ⟶ ⟨Δ, Δ[[x]]⟩
        [((x_1 v_1) ... (x v) (x_2 v_2) ...) (in-hole E v)])
   (--> [env (in-hole E (+ natural_1 natural_2))]                   ; ⟨Δ, (+ n₁ n₂)⟩ ⟶ ⟨Δ, n₁ + n₂⟩
        [env (in-hole E ,(+ (term natural_1) (term natural_2)))])
   (--> [env (in-hole E (* natural_1 natural_2))]                   ; ⟨Δ, (* n₁ n₂)⟩ ⟶ ⟨Δ, n₁ * n₂⟩
        [env (in-hole E ,(* (term natural_1) (term natural_2)))])
   (--> [env (in-hole E (- natural_1 natural_2))]                   ; ⟨Δ, (- n₁ n₂)⟩ ⟶ ⟨Δ, n₁ - n₂⟩
        [env (in-hole E ,(- (term natural_1) (term natural_2)))])
   (--> [env (in-hole E (= natural natural))]                       ; ⟨Δ, (= n n)⟩ ⟶ ⟨Δ, true⟩
        [env (in-hole E true)])
   (--> [env (in-hole E (= natural_!_ natural_!_))]                 ; ⟨Δ, (= n₁ n₂)⟩ ⟶ ⟨Δ, false⟩
        [env (in-hole E false)])
   (--> [env (in-hole E (<= natural_1 natural_2))]                  ; ⟨Δ, (<= n₁ n₂)⟩ ⟶ ⟨Δ, n₁ ≤ n₂⟩
        [env (in-hole E ,(if (<= (term natural_1) (term natural_2)) (term true) (term false)))])
   (--> [env (in-hole E (& true B))]                                ; ⟨Δ, (& true b)⟩ ⟶ ⟨Δ, b⟩
        [env (in-hole E B)])
   (--> [env (in-hole E (& false B))]                               ; ⟨Δ, (& false b)⟩ ⟶ ⟨Δ, false⟩
        [env (in-hole E false)])
   (--> [env (in-hole E (not true))]                                ; ⟨Δ, (not true)⟩ ⟶ ⟨Δ, false⟩
        [env (in-hole E false)])
   (--> [env (in-hole E (not false))]                               ; ⟨Δ, (not false)⟩ ⟶ ⟨Δ, true⟩
        [env (in-hole E true)])))

;(traces r (term (() skip)))
;(traces r (term (() (:= x (- 3 2)))))
;(traces r (term (((y 2) (x 3) (z 4)) (:= x (+ y z)))))
;(traces r (term (() ((:= x 5) (:= y 3)))))
;(traces r (term (() (if (= 1 1) skip (:= x 7)))))
;(traces r (term (() (if (<= 3 2) skip (:= x 7)))))
;(traces r (term (() (while true skip))))
;(traces r (term (() (while false skip))))
;(traces r (term (((x 0)) ((:= x 3) skip))))
;(traces r (term (() ((:= x 3) skip))))

#;(traces r (term (((x 5) (y 7))
                   (((:= z x) (:= x y)) (:= y z)))))

#;(traces r (term (((x 3))
                 ((:= y 1) (while (not (= x 1))
                             ((:= y (* y x)) (:= x (- x 1))))))))

#;(traces r (term (((x 17) (y 5))
                 ((:= z 0)
                  (while (<= y x)
                    ((:= z (+ z 1))
                     (:= x (- x y))))))))
