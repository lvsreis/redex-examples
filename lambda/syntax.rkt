#lang racket

(require redex)

(provide (all-defined-out))

(define-language Lambda
  [e ::= x
     (e e)
     (λ x e)]
  [x y := variable-not-otherwise-mentioned])

(redex-match? Lambda e (term (x (λ x (λ y x)))))

(define-metafunction Lambda
  unique-vars : x ... -> boolean
  [(unique-vars) #t]
  [(unique-vars x x_1 ... x x_2 ...) #f]
  [(unique-vars x x_1 ...) (unique-vars x_1 ...)])

(test-equal (term (unique-vars x y z)) #t)
(test-equal (term (unique-vars x y x)) #f)

(define-metafunction Lambda
  unique-vars2 : x ... -> boolean
  [(unique-vars2 x_!_1 ...) #t]
  [(unique-vars2 x ...) #f])

(test-equal (term (unique-vars2)) #t)
(test-equal (term (unique-vars2 x y z)) #t)
(test-equal (term (unique-vars2 x y x)) #f)

(define-metafunction Lambda
  union : (x_!_1 ...) (x_!_2 ...) -> (x ...)
  [(union () (x ...)) (x ...)]  
  [(union (x x_1 ...) (x_2 ...)) (x x_1 ... x_2 ...)]) ; the result can have repeted names

(define-metafunction Lambda
  remove : x (x ...) -> (x ...)
  [(remove x ()) ()]
  [(remove x (x_1 ... x x_2 ...)) (remove x (x_1 ... x_2 ...))]
  [(remove x (x_1 ...)) (x_1 ...)])
  
(define-metafunction Lambda
  FV : e -> (x ...)
  [(FV x) (x)]
  [(FV (e_1 e_2)) (x_1 ... x_2 ...)
         (where (x_1 ...) (FV e_1))
         (where (x_2 ...) (FV e_2))]
  [(FV (λ x e)) (remove x (FV e))])

(test-equal (term (FV (λ x x))) (term ()))
(test-equal (term (FV (x y))) '(x y))
(test-equal (term (FV (λ y (x y)))) '(x))
(test-equal (term (FV (λ z (λ x (λ y (x (y z))))))) '())
(test-equal (term (FV ((λ x x) x))) '(x))

(define-metafunction Lambda
  subs-naive : x e e -> e
  [(subs-naive x e x) e]
  [(subs-naive x e x_1) x_1]
  [(subs-naive x e (λ x_1 e_1)) (λ x_1 (subs-naive x e e_1))]
  [(subs-naive x e (e_1 e_2)) ((subs-naive x e e_1) (subs-naive x e e_2))])

(test-equal (term (subs-naive x (λ z (z w)) (λ y x)))
            (term (λ y (λ z (z w)))))

; A wrong result due naive substitution
(test-equal (term (subs-naive x y (λ x x)))
            (term (λ x y)))

(define-metafunction Lambda
  subs+ : x e e -> e
  [(subs+ x e x) e]
  [(subs+ x e x_1) x_1]
  [(subs+ x e (λ x e_1)) (λ x e_1)]
  [(subs+ x e (λ x_1 e_1)) (λ x_1 (subs+ x e e_1))]
  [(subs+ x e (e_1 e_2)) ((subs+ x e e_1) (subs+ x e e_2))])

; It resolve the last problem of naive substitution
(test-equal (term (subs+ x y (λ x x)))
            (term (λ x x)))

; But it can capture free variables
(test-equal (term (subs+ x z (λ z x)))
            (term (λ z z)))

(define-metafunction Lambda
  in : x (x ...) -> boolean
  [(in x (x_1 ... x x_2 ...)) #t]
  [(in x (x_1 ...)) #f])

(define-metafunction Lambda
  subs : x e e -> e
  [(subs x e x) e]
  [(subs x e x_1) x_1]
  [(subs x e (λ x e_1)) (λ x e_1)]
  [(subs x e (λ x_1 e_1)) (λ x_1 (subs x e e_1))
        (where #f (in x_1 (FV e)))]
  [(subs x e (λ x_1 e_1)) (λ x_new (subs x e e_new))
     (where x_new ,(variable-not-in (term (x_1 e_1)) (term x_1)))
     (where e_new (subs x_1 x_new e_1))]
  [(subs x e (e_1 e_2)) ((subs x e e_1) (subs x e e_2))])

; Now the substitution is correct!
(test-equal (term (subs x z (λ z x)))
            (term (λ z1 z)))

(test-results)



; The redex substitutions functions

(default-language Lambda)
; Observe the result of these functions when there is no binding definition
(term
 (substitute (x (λ x (λ y x)))
             x
             (y y)))
(alpha-equivalent? Lambda
       (term (x (λ x (λ y x))))
       (term (x (λ z (λ y z)))))

(define-language Lambda-with-binding
  [e ::= x
     (e e)
     (λ x e)]
  [x y := variable-not-otherwise-mentioned]

  #:binding-forms
   (λ x e #:refers-to x))

(default-language Lambda-with-binding)
(term
 (substitute (x (λ x (λ y x)))
             x
             (y y)))

(alpha-equivalent? Lambda-with-binding
       (term (x (λ x (λ y x))))
       (term (x (λ z (λ y z)))))
