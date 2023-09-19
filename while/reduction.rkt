#lang racket

(require redex)

(require "syntax.rkt")

(define r (reduction-relation While
  #:domain (S ((x natural) ...))
  #:codomain (S ((x natural) ...))
  (--> 
