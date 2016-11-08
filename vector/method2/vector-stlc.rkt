#lang racket

(provide ;;type?
         type-equal?
         typecheck-expr)

(require profile
         (rename-in racket/unsafe/ops
                    [unsafe-car ucar]
                    [unsafe-cdr ucdr]
                    [unsafe-vector-length uvec-len]
                    [unsafe-vector-ref uref]
                    [unsafe-vector-set! uset!]
		    [unsafe-fxvector-length ufxlen]
		    [unsafe-fxvector-ref ufxref]
		    [unsafe-fxvector-set! ufxset!]
                    [unsafe-fx< i<]
                    [unsafe-fx+ i+]
                    [unsafe-fx- i-]
		    [unsafe-fx= i=]))
;; language

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



(define (safe-vref v pos)
  (define size (uvec-len v))
  (if (i< pos size)
      (uref v pos)
      #f))

(define empty-env '()) ;; use a different representation

(define (extend-env env sym expr)
  (cons (list sym expr) env))

(define (apply-env env sym)
  (cond
    [(empty? env)
     (error 'apply-env "Could not find variable ~v~n" sym)]
    [(i= (ucar (ucar env)) sym)
     (ucar (ucdr (ucar env)))] ;; return (list start end expr)
    [else
     (apply-env (ucdr env) sym)]))

(define (type? t)
  (match t
    [(or 'int 'bool 'ntype)
     #t]
    [ls  ;; `(,t1 -> ,t2)   turn to `(result params)
     (if (or (not (list? ls)) (empty? (cdr ls)))
         #f
         (and (andmap type? (ucdr ls)) (type? (ucar ls))))]
    [_
     #f]))

(define (lambda-type? t)
  (and (list? t) (not (empty? (ucdr t)))))


;; return #f or how much we've advanced in 2 vectors
(define (type-equal? t1 t2)
  (match* (t1 t2)
    [('int 'int)
     #t]
    [('bool 'bool)
     #t]
    [('ntype 'ntype)
     #t]
    [(ls1 ls2)    ;;(`(,t1_ -> ,t2_) `(,t1__ -> ,t2__))
     (if (or (not (and (list? ls1) (list? ls2)))
              (empty? (ucdr ls1))
              (empty? (ucdr ls2)))
         #f
         (and (andmap type-equal? (ucdr ls1) (ucdr ls2))
              (type-equal? (ucar ls1) (ucar ls2))))]
    [(_ _)
     #f]))

(define (get-len expr pos)
  (define tag (ufxref expr pos))
  (cond
    [(or (i= TRUE tag) (i= FALSE tag) (i= NULL tag))
     1]
    [(or (i= NUM tag) (i= SYM tag))
     2]
    [(or (i= BEGIN tag) (i= LAMBDA tag) (i= APP tag))
     (uref expr (i+ 1 pos))]
    [else
     (error 'get-len "not an expr")]))

(define NULL 1)
(define TRUE 2)
(define FALSE 3)
(define BEGIN 4)
(define LAMBDA 5)
(define APP 6)
(define SYM 7)
(define NUM 8)

;; store size of, for instance, lambda, so typecheck doesn't have to return multiple values.
;; can just add that value to pos when it returns.
(define (typecheck expr types pos tenv)
  
  (define type-store types)
  (define (typecheck-driver expr pos tenv)
    (define tag (ufxref expr pos))
    (cond
      [(i= NUM tag)
       'int]
      [(or (i= TRUE tag) (i= FALSE tag))
       'bool]
      [(i= NULL tag)
       'ntype]
      [(i= SYM tag)
       (apply-env tenv (ufxref expr (i+ 1 pos)))]

      [(i= BEGIN tag)
       (let ([count (ufxref expr (i+ 2 pos))])
         (let loop1 ([p (i+ 3 pos)]
                     [c count])
           (if (i< c 2)
               (typecheck-driver expr p tenv)
               (let ([_ (typecheck-driver expr p tenv)]
               	     [p1 (get-len expr p)])
                 (loop1 p1 (i- c 1))))))]

      [(i= LAMBDA tag)
       (let ([count (ufxref expr (i+ 2 pos))])
         (let loop ([tenv_ tenv]
                    [p (i+ 3 pos)]
           	    [c count]
                    [type '()])
           (if (< c 1) ;; typecheck body
               (let ([btype (typecheck-driver expr p tenv_)])
                 (cons btype (reverse type))) ;; hmmmmmm
               (let* ([sym (ufxref expr p)]
                      [atype_index (ufxref expr (i+ 1 p))] ;; atype is an index into types now.
		      [atype (uref type-store atype_index)])
                 (loop (extend-env tenv_ sym atype)
                       (i+ 2 p)
                       (i- c 1)
                       (cons atype type))))))]
      [(i= APP tag)
       (let* ([lamt (typecheck-driver expr (i+ 2 pos) tenv)]
              [npos (i+ (get-len expr (i+ 2 pos)) pos 2)]
              [arg_num (ufxref expr npos)])
         (cond
           [(not (lambda-type? lamt))
            (error 'typecheck "no type ~a~n" expr)]
           [else
            (let loop5 ([npos (i+ 1 npos)] ;; position in expr
                        [lamt^ (ucdr lamt)] ;; position in lamt
                      	[count arg_num]) ;; how many times we loop
              (cond
                [(i< count 1)
               	 (ucar lamt)] ;; return body type
              	[else ;; here
                 (let ([atype (typecheck-driver expr npos tenv)]
                       [np (i+ (get-len expr npos) npos)])
                   (if (type-equal? atype (ucar lamt^))
                       (loop5 np (ucdr lamt^) (i- count 1))
                       (error 'typecheck  "no type: ~a~n" expr)))]))]))]
      [else
       (error 'typecheck "bad form: ~a" expr)]))
  (typecheck-driver expr pos tenv)) 

;; typechecker that gathers constraints as it goes and then checks them all at the end.
;; optimization
;; In original typechecker, type lambda, which requires walking over all params.
;; then walk over that produced type to compare with args.
;; optimize by creating a specialized typechecker function that does this at same time
;;(define (typecheck-app lam args count arg_pos)
  

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

(define (typecheck-driver expr pos tenv)
  (typecheck expr pos tenv))

#|
  (profile-thunk (thunk (let-values ([(_ __)
                                      (typecheck expr pos tenv out 0)])
                          out))
                 #:repeat 20)) |#

(define (typecheck-expr expr types)
  (typecheck-driver expr types 0 empty-env))