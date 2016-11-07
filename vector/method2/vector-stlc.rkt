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
                    [unsafe-vector-set! uset!]
                    [unsafe-fx< i<]
                    [unsafe-fx+ i+]
                    [unsafe-fx- i-]))
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
  (define tag (uref expr pos))
  (cond
    [(or (exact-integer? tag)
         (eq? 'true tag) (eq? 'false tag)
         (eq? 'null tag))
     1]
    [(or (eq? 'begin tag) (eq? 'lambda tag) (eq? 'app tag))
     (uref expr (i+ 1 pos))]
    [(symbol? tag)
     1]
    [else
     (error 'get-len "not an expr")]))

;; store size of, for instance, lambda, so typecheck doesn't have to return multiple values.
;; can just add that value to pos when it returns.
(define (typecheck expr pos tenv)
  (define tag (uref expr pos))
  (cond
    [(exact-integer? tag)
     'int]
    [(or (eq? 'true tag) (eq? 'false tag))
     'bool]
    [(eq? 'null tag)
     'ntype]
    [(eq? 'begin tag)
     (let ([count (uref expr (i+ 2 pos))])
       (let loop1 ([p (i+ 3 pos)]
                   [c count])
         (if (i< c 2)
             (typecheck expr p tenv)
             (let ([_ (typecheck expr p tenv)]
                   [p1 (get-len expr p)])
               (loop1 p1 (i- c 1))))))]
    [(eq? 'lambda tag)
     (let ([count (uref expr (i+ 2 pos))])
       (let loop ([tenv_ tenv]
                  [p (i+ 3 pos)]
                  [c count]
                  [type '()])
         (if (< c 1) ;; typecheck body
             (let ([btype (typecheck expr p tenv_)])
               (cons btype (reverse type))) ;; hmmmmmm
             (let* ([sym (uref expr p)]
                    [atype (uref expr (i+ 1 p))])
               (loop (extend-env tenv_ sym atype)
                     (i+ 2 p)
                     (i- c 1)
                     (cons atype type))))))]
    [(eq? 'app tag)
     (let* ([lamt (typecheck expr (i+ 2 pos) tenv)]
            [npos (+ (get-len expr (i+ 2 pos)) pos 2)]
            [arg_num (uref expr npos)])
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
               (let ([atype (typecheck expr npos tenv)]
                     [np (+ (get-len expr npos) npos)])
                 (if (type-equal? atype (ucar lamt^))
                     (loop5 np (ucdr lamt^) (i- count 1))
                     (error 'typecheck  "no type: ~a~n" expr)))]))]))]
    [(symbol? tag)
     (apply-env tenv tag)]
    [else
     (error 'typecheck "bad form: ~a" expr)]))

;; typechecker that gathers constraints as it goes and then checks them all at the end.
;; optimization
;; In original typechecker, type lambda, which requires walking over all params.
;; then walk over that produced type to compare with args.
;; optimize by creating a specialized typechecker function that does this at same time
;;(define (typecheck-app lam args count arg_pos)
  
  
  #|
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
|#
  
  


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
  (typecheck expr pos tenv))

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