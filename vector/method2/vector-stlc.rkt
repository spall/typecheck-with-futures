#lang racket

(provide ;;type?
         type-equal?
         typecheck-expr
         typecheck-sequential
         naive-typecheck-parallel
         better-typecheck-parallel
	 NULL
	 TRUE
	 FALSE
	 BEGIN
	 LAMBDA
	 APP
	 SYM
	 NUM)

;; dump memory stats
;; parallel speedup?
;; plai/typed
;; write plai/typed program that takes long time to typecheck.
;; build matthew's branch of racket "linklet" macro-expander is written in racket.


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
		    [unsafe-fx= i=])
         future-visualizer)
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
      [(i= NUM tag)          'int]
      [(i= NULL tag)         'ntype]
      [(or (i= TRUE tag)
           (i= FALSE tag))   'bool]
      [(i= SYM tag)
       
       (apply-env tenv (ufxref expr (i+ 1 pos)))]
      [(i= BEGIN tag) ;; pos + 1 is length. pos + 2 is count
       
       (let ([count (ufxref expr (i+ 2 pos))])
         (let loop ([pos (i+ 3 pos)]
                    [count count])
           (if (i< count 2)
               (typecheck-driver expr pos tenv)
               (begin
                 (typecheck-driver expr pos tenv) ;; don't care
                 (loop (i+ pos (get-len expr pos))
                       (i- count 1))))))]
      [(i= LAMBDA tag) ;; 1 + pos is length. pos + 2 is param count
       
       (let ([count (ufxref expr (i+ 2 pos))])
         (let loop1 ([pos (i+ 3 pos)]
                    [count count]
                    [tenv tenv]
                    [lam-type '()])
           (if (i< count 1) ;; typecheck body
               (cons (typecheck-driver expr pos tenv)
                    lam-type);; (reverse lam-type)) ;; not ideal
               (let ([sym (ufxref expr (i+ 1 pos))] ;; SYM is at pos, actually "sym" is at pos + 1
                     [ptype (uref type-store (ufxref expr (i+ pos 2)))])  ;;(typecheck-driver expr (i+ pos 2) tenv)])
                 (loop1 (i+ (i+ pos 2) 1)
                       (i- count 1)
                       (extend-env tenv sym ptype) ;; extend type env
                       (cons ptype lam-type))))))]
      [(i= APP tag)   ;; 1 + pos is length. pos + 2 is e1 ... after e1. number of args. then args.

       (let ([e1-type (typecheck-driver expr (i+ 2 pos) tenv)]
             [after-e1 (i+ (i+ 2 pos)
                           (get-len expr (i+ 2 pos)))])
         (cond
           [(lambda-type? e1-type)
            (let ([btype (car e1-type)]
                  [ptypes (cdr e1-type)]
                  [arg-count (ufxref expr after-e1)])
              (let loop2 ([ptypes ptypes]
                         [ac arg-count]
                         [pos (i+ after-e1 1)])
                (cond
                  [(and (i= ac 0) (empty? ptypes))
                   btype]
                  [(or (i= ac 0) (empty? ptypes))
                   (error "Args and params not same length~n")]
                  [(type-equal? (car ptypes) (typecheck-driver expr pos tenv))
                   (loop2 (cdr ptypes) (i- ac 1) (i+ pos (get-len expr pos)))]
                  [else
                   (error "Arg and param type did not match: ~a ~a~n" (typecheck-driver expr pos tenv) (car ptypes))])))]
           [else (error "Not a lambda type: " e1-type)]))]
      [else (error "Not a valid expression: " tag)]))

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
LAMBDA
 types:
 1. prim types can just be stored regularly
 2. 'lamt n pt_1 pt_2 ... pt_n body_type

|#

(define (typecheck-driver expr types pos tenv)
  (typecheck expr types pos tenv))

(define (typecheck-sequential exprs types)
  (void
   (for ([e (in-vector exprs)]
         [t (in-vector types)])
     (typecheck-expr e t))))

(define (naive-typecheck-parallel exprs types)
  (for-each touch
            (for/list ([e (in-vector exprs)]
                       [t (in-vector types)])
              (future (λ ()
                        (typecheck-expr e t))))))

(define (better-typecheck-parallel exprs types)
  (define pcount (processor-count))
  (define len (vector-length exprs))
  (define seq-size (ceiling (/ len pcount)))

  (for-each touch
            (for/list ([pc (in-range (min pcount len))])
              (future (λ ()
                        (for ([e (in-vector exprs (* pc seq-size)
                                            (min len (* (+ 1 pc) seq-size)))]
                              [t (in-vector types (* pc seq-size)
                                            (min len (* (+ 1 pc) seq-size)))])
                          (typecheck-expr e t)))))))
  
#|
  (profile-thunk (thunk (let-values ([(_ __)
                                      (typecheck expr pos tenv out 0)])
                          out))
                 #:repeat 20)) |#

(define (typecheck-expr expr types)
  (typecheck-driver expr types 0 empty-env))