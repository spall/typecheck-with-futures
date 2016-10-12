#lang racket

(provide typecheck-expr
         typecheck-sequential
         naive-typecheck-parallel
         better-typecheck-parallel
         places-typecheck-parallel
         type?)
(require (rename-in racket/unsafe/ops
                    [unsafe-car ucar]
                    [unsafe-cdr ucdr])
         "performance.rkt")
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
    [ls  ;; `(,t1 -> ,t2)   turn to `(result params)
     (if (and (not (list? ls)) (empty? (cdr ls)))
         #f
         (and (andmap1 type? (ucdr ls)) (type? (ucar ls))))]
    [_
     #f]))

(define (type-equal? t1 t2)
  (match* (t1 t2)
    [('int 'int)
     #t]
    [('bool 'bool)
     #t]
    [('ntype 'ntype)
     #t]
    [(ls1 ls2)    ;;(`(,t1_ -> ,t2_) `(,t1__ -> ,t2__))
     (if (and (not (and (list? ls1) (list? ls2)))
              (empty? (cdr ls1))
              (empty? (cdr ls2)))
         #f
         (and (andmap2 type-equal? (ucdr ls1) (ucdr ls2))
              (type-equal? (ucar ls1) (ucar ls2))))]
    [(_ _)
     #f]))

(define (andmap1 proc l1)
  (cond
    [(empty? l1)
     #t]
    [(proc (ucar l1))
     (andmap1 proc (ucdr l1))]
    [else
     #f]))

(define (andmap2 proc l1 l2)
  (cond
    [(and (empty? l1) (empty? l2))
     #t]
    [(or (empty? l1) (empty? l2))
     (error 'andmap2 "lists not same length")]
    [(proc (ucar l1) (ucar l2))
     (andmap2 proc (ucdr l1) (ucdr l2))]
    [else
     #f]))

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
    [(or (empty? l1) (empty? l2))
     (error 'foldl2 "lists not same length")]
    [else
     (foldl2 proc (proc (ucar l1) (ucar l2) init) (ucdr l1) (ucdr l2))]))

;; from  2,764,194,400 bytes allocated in the heap
;; to  1,680,458,704 bytes allocated in the heap
;; to 910,630,272 bytes allocated in the heap

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
    [`(begin . ,expr) ;; `(begin ,expr ..1) to `(begin . ,expr) from 2,209,902,840 bytes allocated in the heap to 2,064,695,672 bytes allocated in the heap 
     (foldl1 (λ (e init)
               (typecheck e tenv))
             #f expr)]
    [`(lambda ,args ,body)
     (let ([new-tenv (foldl1 (λ (arg tenv_)
                               (λ (z) ;; destruct arg.
                                 (if (eq? z (ucar arg))
                                     (caddr arg)
                                     (tenv_ z))))
                             tenv args)])
       (cons (typecheck body new-tenv) (map caddr args)))]
    [`(,e1 . ,e2) ;; `(,e1 ,e2 ..1) => `(,e1 . ,e2) went from 2,064,695,672 bytes allocated in the heap to 1,683,298,584 bytes allocated in the heap 
     (match (typecheck e1 tenv)
       [ls;;`(,t1 -> ,t2) ;; represent types differently.
        (if (and (not (list? ls)) (empty? (cdr ls)))
            (error 'typecheck "no type ~a~n" expr)
            (if (andmap2 (lambda (t_ e_)
                           (type-equal? t_ (typecheck e_ tenv)))
                         (ucdr ls) e2)
                (ucar ls)
                (error 'typecheck "no type: ~a~n" expr)))]
       [else
        (error 'typecheck "no type ~a~n" expr)])]
    [else  ;; i think the previous expression will capture some of the "else" as well
     (error 'typecheck "bad form: ~a" expr)]))

(define (typecheck-expr expr)
 ;; (typecheck expr empty-env))
  (dump-memory-stats)
  (typecheck expr empty-env)
  (dump-memory-stats))
;;  (dump-memory-stats (typecheck expr empty-env)))

(define (typecheck-sequential exprs)
  (void
   (for ([e (in-vector exprs)])
     (typecheck-expr e))))

(define (naive-typecheck-parallel exprs)
  (for-each
   touch
   (for/list ([e (in-vector exprs)])
     (future (λ () (typecheck-expr e))))))

(define (better-typecheck-parallel exprs)
  (define pcount (processor-count))
  (define len (vector-length exprs))
  (define seq-size (ceiling (/ len pcount)))
  
  (for-each
   touch
   (for/list ([pc (in-range (min pcount len))])
     (future (λ ()
               (for ([e (in-vector exprs (* pc seq-size)
                                   (min len (* (+ 1 pc) seq-size)))])
                 (typecheck-expr e)))))))

;; places

(define (places-typecheck-parallel exprs)
  (define pcount (processor-count))
  (define len (vector-length exprs))
  (define seq-size (ceiling (/ len pcount)))

  (for-each
   place-channel-get
   (for/list ([pc (in-range (min pcount len))])
     (define p (place ch
                      (define results (time (for/list ([e (place-channel-get ch)])
                                        (typecheck-expr e))))
                      (place-channel-put ch results)))
     (place-channel-put p
                        (vector-copy exprs
                                     (* pc seq-size)
                                     (min len (* (+ 1 pc) seq-size))))
     p)))

                        


  

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

