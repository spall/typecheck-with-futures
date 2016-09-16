#lang racket

(require "s-exp-stlc.rkt"
         racket/random)
(provide gen-well-formed-sexp)

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
       `(,(gen-type) -> ,(gen-type))])))

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
          (gen-application (- size 1) ls)]
         [else ;; lambda
          (gen-lambda (- size 1) ls)]))]))


;; generates nested lambdas.
(define (gen-lambda size ls)
  (if (= 1 size)
      (let ([arg (gensym)])
        `(lambda (,arg : ,(gen-type))
           ,(gen-term 0 (cons arg ls))))
        
      (let ([arg (gensym)])
        `(lambda (,arg : ,(gen-type))
           ,(gen-lambda (sub1 size) (cons arg ls))))))

(define (gen-lambda-w-body body)
  (let ([arg (gensym)])
    `(lambda (,arg : ,(gen-type))
       ,body)))

;; takes a lambda, generates necessary parameters
(define (gen-parameters lam)
  (match lam
    [`(lambda (,(? symbol? arg) : ,t1) ,body)
     (if (is-lambda? body)
         (cons (gen-term-w-type t1)
               (gen-parameters body))
         (cons (gen-term-w-type t1)
               '()))]
    [else
     (error 'gen-application "not a lambda ~v~n" lam)]))

;; constructs an application from lambda and params
(define (gen-application expr params)
  (cond
    [(empty? params)
     expr]
    [else
     (gen-application `(,expr ,(car params))
                      (cdr params))]))
  
;; expr has type lambda?
(define (is-lambda? expr)
  (match (typecheck-expr expr)
    [`(,t1 -> ,t2)
     #t]
    [_
     #f]))
    
;; generates term with type t
(define (gen-term-w-type t)
  (match t
    ['int
     (random 100)]
    ['bool
     (random-ref (list 'true 'false))]
    ['ntype
     'null]
    [`(,(? type? t1) -> ,(? type? t2))
     `(lambda (,(gensym) : ,t1) ,(gen-term-w-type t2))]
    [else
     (error 'gen-term-w-type "not a valid type ~v" t)]))


(define (gen-sexp size)
  (gen-term size '()))

(random-seed 1)

(define (gen-well-formed-sexp size)
  (let ([lam (gen-lambda size '())])
    (gen-application lam (gen-parameters lam))))



  