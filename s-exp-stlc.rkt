#lang racket

(provide typecheck-expr
         type?)
#| http://www.cs.cornell.edu/courses/cs6110/2013sp/lectures/lec25-sp13.pdf

   Simply-typed lambda calculus

   prim values b ::= n | true | false | null
   terms       e ::= b | x | e1 e2 | lambda x:t . e
   prim types  B ::= int | bool | 1
   types       t ::= t1 -> t2 | B

|#

(define empty-env (lambda (x) 'error))

(define (type? t)
  (match t
    [(or 'int 'bool 'ntype)
     #t]
    [`(,t1 -> ,t2)
     (and (type? t1) (type? t2))]
    [_
     #f]))

(define (type-equal? t1 t2)
  (match* (t1 t2)
    [(`(,t1_ -> ,t2_) `(,t1__ -> ,t2__))
     (and (type-equal? t1_ t1__) (type-equal? t2_ t2__))]
     [('int 'int)
      #t]
     [('bool 'bool)
      #t]
     [('ntype 'ntype)
      #t]
     [(_ _)
      #f]))

(define (typecheck expr tenv)
  (match expr
    [(? exact-integer? n)
     'int]
    [(or 'true 'false)
     'bool]
    ['null
     'ntype]
    [(? symbol? x)
     (tenv x)]
    [`(lambda (,(? symbol? x) : ,(? type? t)) ,body)
     `(,t -> ,(typecheck body
                         (lambda (arg)
                           (if (eq? x arg) ;; eq? is same. on symbols
                               t
                               (tenv arg)))))]
    [`(,e1 ,e2)
     (match (typecheck e1 tenv)
       [`(,t1 -> ,t2)
        (if (type-equal? t1 (typecheck e2 tenv))
            t2
            (error 'typecheck "no type"))]
       [else
        (error 'typecheck "no type")])]
    [else
     (error 'typecheck "bad form: ~a" expr)]))

(define (typecheck-expr expr)
  (typecheck expr empty-env))



;; deal with stack overflow, make tail recursive?

  

#|
 generate some really huge terms so they take a non-trivial amount of time to typecheck.
 write a naive typecheck in parallel function that takes a list of terms and typechecks them
 all in parallel.

 look at comparisons of how big the terms are and how many and performance.

 do this with s-exp
 what is the speedup? 1x 2x?

 how big before parallel is useful?
 
 don't start doing recursive parallel typecheck calls
 top level probably best place

 next week global data strucutre type environment.
|#
