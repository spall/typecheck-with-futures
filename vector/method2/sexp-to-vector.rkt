#lang racket

(require "stlc-sexp-gen.rkt"
         "vector-stlc.rkt"
	 racket/fixnum)

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
(define (sexp->vector expr type-vec pos)
  (match expr
    [(? exact-integer? n)
     (values (fxvector NUM n) 2 type-vec pos)]
    ['true
    (values (fxvector TRUE) 1 type-vec pos)]
    ['false
     (values (fxvector FALSE) 1 type-vec pos)]
    ['null
     (values (fxvector NULL) 1 type-vec pos)]
    [(? string? x)
     (values (fxvector SYM (string->number x)) 2 type-vec pos)]
    [`(begin ,expr)
     (let*-values ([(vexprs lens tv npos) (for/fold ([vexprs_ '()] [lens_ 0] [tv type-vec] [pos pos])
                                            ([v (in-fxvector expr)])
                                    (let-values ([(vec len tv pos) (sexp->vector v)])
                                      (values (cons vec vexprs_) (+ len lens_)
				      	      tv pos)))]
                   [(len) (length vexprs)])
       (values (apply vector-append (cons (fxvector BEGIN (+ 3 lens) len) vexprs))
               (+ 3 lens) tv npos))]
    [`(lambda ,args ,body) ;; 'lambda n a+t_1 a+t_2 ... a+t_n body
     (let*-values ([(vargs) (for/list ([a (in-fxvector args)]
     		  	    	       [pos (in-range pos (+ (fxvector-length args) 1))])
			      (begin (helper a type-vec pos)
                              (vector-append (sexp->vector (car a)) (fxvector pos))))] ;; CHANGE ME
                   [(len) (length vargs)]
                   [(bvec blen) (sexp->vector body)])
       (values (vector-append (apply vector-append (cons (fxvector LAMBDA (+ (* 3 len) blen 3) len)
                                                         vargs))
                              bvec)
               (+ (* 3 len) blen 3) type-vec (+ pos len)))]
    [`(,e1 . ,e2) ;;'app proc n arg_1 arg_2 ... arg_n
     (let*-values ([(ve2 lens) (for/fold ([ve2_ '()] [lens 0])
                                         ([e (in-list e2)])
                                 (let-values ([(vec l tv pos) (sexp->vector e)])
                                   (values (cons vec ve2_) (+ l lens))))]
                   [(len) (length ve2)]
                   [(ve1 lene1) (sexp->vector e1)])
       (values (apply vector-append (cons (vector-append (fxvector APP (+ 3 lens lene1))
                                                         ve1)
                                          (cons (fxvector len)
                                                (reverse ve2))))
               (+ 3 lens lene1)))]
    [else
     (error 'sexp->vector "cannot convert" expr)]))

(define (helper expr types-vec pos)
  (define sym (car expr))
  (define type (caddr a))
  (set! types-vec pos type))

(define (gen-vector-stlc-exprs depth arg-num)
  (define sexp (gen-well-formed-sexp depth arg-num))
  (let-values ([(vec len) (sexp->vector sexp (vector) 0)])
    vec))
