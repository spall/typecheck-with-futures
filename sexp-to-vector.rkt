#lang racket

(require "stlc-sexp-gen.rkt")

#|
 prim values  b ::= n | true | false | null
 terms        e ::= b | x | (e1 e2) | (lambda ((x : t) ..1) e) | (begin e ..1)

 prim types   B ::= int | bool | 1
 types        t ::= t1 -> t2 | B
 
|#

#| vector representation
 1. Can just store b in vector as is
 2. same with x
 3. begin:       'begin n expr_1 expr_2 ... expr_n
 4. lambda:      'lambda n a+t_1 a+t_2 ... a+t_n body
 5. application: 'app proc n arg_1 arg_2 ... arg_n

 types:
 1. prim types can just be stored regularly
 2. 'lamt n pt_1 pt_2 ... pt_n body_type

|#
(define (sexp-t->vector-t type)
  (match type
    ['int
     (vector 'int)]
    ['bool
     (vector 'bool)]
    ['ntype
     (vector 'ntype)]
    [ls ;; (car ls) goes on end. is body type
     (let* ([vparams (map sexp-t->vector-t (cdr ls))]
            [len (length vparams)])
       (vector-append (apply vector-append (cons (vector 'lamt len) vparams))
                      (sexp-t->vector-t (car ls))))]
    [else
     (error 'sexp-t->vector-t "cannot conver" type)]))

(define (sexp->vector expr)
  (match expr
    [(? exact-integer? n)
     (vector n)]
    [(or 'true 'false)
     (vector expr)]
    ['null
     (vector 'null)]
    [(? symbol? x)
     (vector x)]
    [`(begin ,expr)
     (let* ([vexprs (map sexp->vector expr)]
            [len (length vexprs)])
       (apply vector-append (cons (vector 'begin len) vexprs)))]
    [`(lambda ,args ,body) ;; 'lambda n a+t_1 a+t_2 ... a+t_n body
     (let* ([vargs (map (λ (a)
                          (vector-append (vector (car a))
                                         (sexp-t->vector-t (car (cdr (cdr a))))))
                        args)]
            [len (length vargs)])
       (vector-append (apply vector-append (cons (vector 'lambda len)
                                                 vargs))
                      (sexp->vector body)))]
    [`(,e1 . ,e2) ;;'app proc n arg_1 arg_2 ... arg_n
     (let* ([ve2 (map sexp->vector e2)]
            [len (length ve2)])
       (apply vector-append (cons (vector-append (vector 'app) (sexp->vector e1))
                                  (cons (vector len)
                                        ve2))))]
    [else
     (error 'sexp->vector "cannot convert" expr)]))

(define (gen-vector-stlc-exprs depth arg-num)
  (define sexp (gen-well-formed-sexp depth arg-num))
  (sexp->vector sexp))
  ;; convert