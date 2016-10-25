#lang racket

(provide ;;type?
         type-equal?
         typecheck-expr
         typecheck-sequential
         naive-typecheck-parallel
         better-typecheck-parallel)

(require profile
         (rename-in racket/unsafe/ops
                    [unsafe-car ucar]
                    [unsafe-cdr ucdr]
                    [unsafe-vector-length uvec-len]
                    [unsafe-vector-ref uref]
                    [unsafe-vector-set! uset!])
         (rename-in racket/fixnum
                    [fx< i<]
                    [fx+ i+]
                    [fx- i-]))
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
    [(eq? (ucar (ucar env)) sym)
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
  (and (list? t) (not (empty? (cdr t)))))


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
              (empty? (cdr ls1))
              (empty? (cdr ls2)))
         #f
         (and (andmap type-equal? (ucdr ls1) (ucdr ls2))
              (type-equal? (ucar ls1) (ucar ls2))))]
    [(_ _)
     #f]))

;; typechecker that gathers constraints as it goes and then checks them all at the end.


(define (typecheck expr pos tenv)
  (define tag (uref expr pos))
  (cond
    [(exact-integer? tag)
     (values (i+ 1 pos) 'int)]
    [(or (eq? 'true tag) (eq? 'false tag))
     (values (i+ 1 pos) 'bool)]
    [(eq? 'null tag)
     (values (i+ 1 pos) 'ntype)]
    [(eq? 'begin tag)
     (let ([count (uref expr (i+ 1 pos))])
       (let loop ([p (i+ 2 pos)]
                  [c count])
         (if (i< c 2)
             (typecheck expr p tenv)
             (let-values ([(p1 _) (typecheck expr p tenv)])
               (loop p1 (i- c 1))))))]
    [(eq? 'lambda tag)
     (let ([count (uref expr (i+ 1 pos))])
       ;; extend env
       (let loop ([tenv_ tenv]
                  [p (i+ 2 pos)]
                  [c count]
                  [type '()])
         (if (< c 1) ;; typecheck body
             (let-values ([(tmp1 btype) (typecheck expr p tenv_)])
               (values tmp1 (cons btype (reverse type)))) ;; hmmmmm
             (let* ([sym (uref expr p)]
                    [atype (uref expr (i+ 1 p))])
               (loop (extend-env tenv_ sym atype)
                     (i+ 2 p)
                     (i- c 1)
                     (cons atype type))))))]
    [(eq? 'app tag)
     (let-values ([(npos lamt) (typecheck expr (i+ 1 pos) tenv)])
         (let ([arg_num (uref expr npos)])
           (cond
             [(not (lambda-type? lamt))
              (error 'typecheck "no type ~a~n" expr)]
             [else
              (let loop5 ([npos (i+ 1 npos)] ;; position in expr
                          [lamt^ (cdr lamt)] ;; position in lamt
                          [count arg_num]) ;; how many times we loop
                (cond
                  [(i< count 1)
                   (values npos (car lamt))] ;; return body type
                  [else ;; here
                   (let-values ([(np atype) (typecheck expr npos tenv)])
                     (if (type-equal? atype (car lamt^))
                         (loop5 np (cdr lamt^) (sub1 count))
                         (error 'typecheck "no type: ~a~n" expr)))]))])))]
    [(symbol? tag)
     (values (i+ 1 pos) (apply-env tenv tag))]
    [else
     (error 'typecheck "bad form: ~a" expr)]))


;; 


;; try representing types using own vectors like
;; #(lam n a #(type) b #(type))
;; this is like an indirection



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
  (let-values ([(_ type)
                (typecheck expr pos tenv)])
    type))
#|
  (profile-thunk (thunk (let-values ([(_ __)
                                      (typecheck expr pos tenv out 0)])
                          out))
                 #:repeat 20)) |#

(define (typecheck-expr expr)
  (typecheck-driver expr 0 empty-env))

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
  (define len (uvec-len exprs))
  (define seq-size (ceiling (/ len pcount)))
  
  (for-each
   touch
   (for/list ([pc (in-range (min pcount len))])
     (future (λ ()
               (for ([e (in-vector exprs (* pc seq-size)
                                   (min len (* (i+ 1 pc) seq-size)))])
                 (typecheck-expr e)))))))