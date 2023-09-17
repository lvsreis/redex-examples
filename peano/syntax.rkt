#lang racket

(require redex)

(define-language Peano
  (P ::= 0
     (s P)
     (+ P P)))

(define ex1 (term 0))                          ; 0
(define ex2 (term (+ 0 (s 0))))                ; 0 + s(0)
(define ex3 (term (+ (s 0) (s (s 0)))))        ; s(0) + s(s(0))
(define ex4 (term (+ (s (s 0)) (+ (s 0) 0))))  ; s(s(0)) + s(0) + 0

(test-equal (redex-match? Peano P (term (s 0))) #t)
(test-match Peano P (term (s 0)))
(test-no-match Peano P (term (+ 0 s(0))))

(test-results)
