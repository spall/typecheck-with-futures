#lang racket

(require "../../sexp/stlc-sexp-gen.rkt"
         "vector-stlc.rkt")

(provide gen-vector-stlc-exprs
         gen-well-formed-sexp
         sexp->vector)

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
(define (sexp->vector expr)
  (match expr
    [(? exact-integer? n)
     (values (vector n) 1)]
    [(or 'true 'false)
     (values (vector expr) 1)]
    ['null
     (values (vector 'null) 1)]
    [(? symbol? x)
     (values (vector x) 1)]
    [`(begin ,expr)
     (let*-values ([(vexprs lens) (for/fold ([vexprs_ '()] [lens_ 0])
                                            ([v (in-vector expr)])
                                    (let-values ([(vec len) (sexp->vector v)])
                                      (values (cons vec vexprs_) (+ len lens_))))]
                   [(len) (length vexprs)])
       (values (apply vector-append (cons (vector 'begin (+ 3 lens) len) vexprs))
               (+ 3 lens)))]
    [`(lambda ,args ,body) ;; 'lambda n a+t_1 a+t_2 ... a+t_n body
     (let*-values ([(vargs) (for/list ([a (in-vector args)])
                              (vector (car a) (car (cdr (cdr a)))))]
                   [(len) (length vargs)]
                   [(bvec blen) (sexp->vector body)])
       (values (vector-append (apply vector-append (cons (vector 'lambda (+ (* 2 len) blen 3) len)
                                                         vargs))
                              bvec)
               (+ (* 2 len) blen 3)))]
    [`(,e1 . ,e2) ;;'app proc n arg_1 arg_2 ... arg_n
     (let*-values ([(ve2 lens) (for/fold ([ve2_ '()] [lens 0])
                                         ([e (in-list e2)])
                                 (let-values ([(vec l) (sexp->vector e)])
                                   (values (cons vec ve2_) (+ l lens))))]
                   [(len) (length ve2)]
                   [(ve1 lene1) (sexp->vector e1)])
       (values (apply vector-append (cons (vector-append (vector 'app (+ 3 lens lene1))
                                                         ve1)
                                          (cons (vector len)
                                                (reverse ve2))))
               (+ 3 lens lene1)))]
    [else
     (error 'sexp->vector "cannot convert" expr)]))

(define (gen-vector-stlc-exprs depth arg-num)
  (define sexp (gen-well-formed-sexp depth arg-num))
  (let-values ([(vec len) (sexp->vector sexp)])
    vec))
  ;; convert
