#lang racket

(provide typecheck-expr
         type?)
(require (rename-in racket/unsafe/ops
                    [unsafe-car ucar]
                    [unsafe-cdr ucdr]))
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

(define (andmap2 proc l1 l2)
  (cond
    [(and (empty? l1) (empty? l2))
     #t]
    [else
     (and (proc (ucar l1) (ucar l2))
          (andmap2 proc (ucdr l1) (ucdr l2)))]))

(define (foldl1 proc init l1)
  (cond
    [(empty? l1)
     init]
    [else
     (foldl1 proc (proc (ucar l1) init) (ucdr l1))]))

(define (foldl2 proc init l1 l2)
  (cond
    [(and (empty? l1) (empty? l2))
     init]
    [else
     (foldl2 proc (proc (ucar l1) (ucar l2) init) (ucdr l1) (ucdr l2))]))

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
    [`(begin ,expr ..1) ;; 
     (foldl1 (λ (e init)
               (typecheck e tenv))
             #f expr)]
    [`(lambda ((,(? symbol? x) : ,(? type? t)) ..1) ,body)
     ;;`(lambda (,@(list `(,(? symbol? x) : ,(? type? t)) ..1)) ,body)
     ;;`(lambda ((,(? symbol? x) : ,(? type? t)) ..1) ,body)
     (let ([new-tenv (foldl2 (lambda (arg type tenv_)
                               (lambda (z)
                                 (if (eq? z arg)
                                     type
                                     (tenv_ z))))
                             tenv x t)])
       `(,@t  -> ,(typecheck body new-tenv)))]
    [`(,e1 ,e2 ..1)
     (match (typecheck e1 tenv)
       [`(,t1 ..1 -> ,t2)
        (if (andmap2 (lambda (t_ e_) 
                      (type-equal? t_ (typecheck e_ tenv)))
                    t1 e2)
            t2
            (error 'typecheck "no type: ~a~n" expr))]
       [else
        (error 'typecheck "no type ~a~n" expr)])]
    [else
     (error 'typecheck "bad form: ~a" expr)]))

(define (typecheck-expr expr)
  (typecheck expr empty-env))

;; (list-rest a b c) == (list a b c ...)
;; 

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
