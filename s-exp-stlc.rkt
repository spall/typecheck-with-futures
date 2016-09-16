#lang racket

(provide typecheck-expr
         parallel-typecheck-expr
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
    [else
     #f]))

(define (typecheck expr tenv)
  (match expr
    [(? integer? n)
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
                           (if (equal? x arg) ;; eq? is same. on symbols
                               t
                               (tenv arg)))))]
    [`(,e1 ,e2)
     (match (typecheck e1 tenv)
       [`(,t1 -> ,t2)
        (if (equal? t1 (typecheck e2 tenv))
            t2
            (error 'typecheck "no type"))]
       [else
        (error 'typecheck "no type")])]
    [else
     (error 'typecheck "bad form: ~a" expr)]))

(define (typecheck-expr expr)
  (typecheck expr empty-env))

(define (parallel-typecheck-expr exprs)
  (for-each
   touch
   (map (lambda (e) (future (lambda () (typecheck-expr e))))
        exprs)))
  

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
