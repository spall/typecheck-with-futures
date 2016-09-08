#lang racket

(require "s-exp-stlc.rkt")

#| http://www.cs.cornell.edu/courses/cs6110/2013sp/lectures/lec25-sp13.pdf

   Simply-typed lambda calculus

   prim values b ::= n | true | false | null
   terms       e ::= b | x | e1 e2 | lambda x:t . e
   prim types  B ::= int | bool | 1
   types       t ::= t1 -> t2 | B

|#

(define (gen-type)
  (let ([rand (random 3)])
    (cond
      [(= rand 0)
       'int]
      [(= rand 1)
       'bool]
      [else
       `(,(gen-type) ,(gen-type))])))

(define (gen-prim-value)
  (let ([rand (random 3)])
    (cond
      [(= rand 0) ;; return number
       (random 100)]
      [(= rand 1) ;; return true
       'true]
      [else       ;; return false
       'false])))

(define (gen-term size ls)
  (cond
    [(<= size 1)
     (let ([rand (random 2)])
       (if (= rand 0)
           (list-ref ls (random (length ls)))
           (gen-prim-value)))]
    [else
     (let ([rand (random 2)])
       (cond
         [(= rand 0) ;; app
          `(,(gen-lambda (ceiling (/ size 2)) ls) ,(gen-term (ceiling (/ size 2)) ls))]
         [else ;; lambda
          (gen-lambda (- size 1) ls)]))]))

;; could generate expressions that return lambdas.  meaning application
(define (gen-lambda size ls)
  (let ([arg (gensym)])
    `(lambda (,arg : ,(gen-type))
       ,(gen-term (- size 1) (cons arg ls)))))


(define (gen-sexp size)
  (gen-term size '()))

  