#lang racket

(provide type?
         type-equal?
         typecheck-expr
         typecheck-sequential
         naive-typecheck-parallel
         better-typecheck-parallel)

(require (rename-in racket/unsafe/ops
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

(define (extend-env env sym val)
  (cons (cons sym val) env))

(define (apply-env env sym)
  (cond
    [(empty? env)
     (error 'apply-env "Could not find variable" sym)]
    [(eq? (ucar (ucar env)) sym)
     (ucdr (ucar env))]
    [else
     (apply-env (ucdr env) sym)]))

                 
(define (type? v pos)
  (define tag (uref v pos))
  
  (cond
    [(eq? 'int tag)
     (values #t (i+ 1 pos))]
    [(eq? 'bool tag)
     (values #t (i+ 1 pos))]
    [(eq? 'ntype tag)
     (values #t (i+ 1 pos))]
    [(eq? 'lamt tag)
     (let ([count (uref v (i+ 1 pos))])
       (if (or (not count)
               (i< (i+ 1 count) 2))
           (values #f (i+ 1 pos))
           (let loop ([count (i+ 1 count)]
                      [pos (i+ 2 pos)]) 
             (cond
               [(i< count 1)
                (values #t pos)]
               [else
                (let-values ([(t? npos) (type? v pos)])
                  (if t?
                      (loop (sub1 count)
                            npos)
                      (values #f (i+ 1 pos))))]))))]
    [else
     (values #f (i+ 1 pos))]))

(define (type-equal? v1 pos1 v2 pos2)
  (define tag1 (uref v1 pos1))
  (define tag2 (uref v2 pos2))
  
  (cond
    [(and (eq? 'int tag1)
          (eq? 'int tag2))
     (values #t (i+ 1 pos1) (i+ 1 pos2))]
    [(and (eq? 'bool tag1)
          (eq? 'bool tag2))
     (values #t (i+ 1 pos1) (i+ 1 pos2))]
    [(and (eq? 'ntype tag1)
          (eq? 'ntype tag2))
     (values #t (i+ 1 pos1) (i+ 1 pos2))]
    [(and (eq? 'lamt tag1)
          (eq? 'lamt tag2)) ;; are counts the same?
     (let ([count1 (uref v1 (i+ 1 pos1))]
           [count2 (uref v2 (i+ 1 pos2))])
       (if (or (not count1)
               (not count2)
               (not (eqv? (i+ 1 count1) (i+ 1 count2)))
               (i< (i+ 1 count1) 2))
           (values #f (i+ 1 pos1) (i+ 1 pos2))
           (let loop ([count (i+ 1 count1)]
                      [pos1 (i+ 2 pos1)]
                      [pos2 (i+ 2 pos2)])
             (cond
               [(i< count 1)
                (values #t pos1 pos2)]
               [else
                (let-values ([(t? npos1 npos2) (type-equal? v1 pos1 v2 pos2)])
                  (if t?
                      (loop (sub1 count) npos1 npos2)
                      (values #f (i+ 1 pos1) (i+ 1 pos2))))]))))]
    [else
     (values #f (i+ 1 pos1) (i+ 1 pos2))]))

(define (helper v start)
  (define tag (uref v start))
  (cond
    [(eq? 'lamt tag)
     (let ([count (uref v (i+ 1 start))])
       (if (or (not count)
               (i< (i+ 1 count) 2))
           (error 'helper "not a type")
           (let loop ([count (i+ 1 count)]
                      [pos (i+ 2 start)])
             (cond
               [(i< count 1)
                pos]
               [else
                (let-values ([(t? npos) (type? v pos)])
                  (if t?
                      (loop (sub1 count)
                            npos)
                      (error 'helper "not a type")))]))))]
           
    [else
     (error 'helper "not a lambda type")]))

;; returns a new vector with just the type in it
(define (get-type v pos)
  (define tag (uref v pos))
  (cond
    [(or (eq? 'int tag) (eq? 'bool tag) (eq? 'ntype tag))
     (values (vector tag) (i+ 1 pos))]
    [(eq? 'lamt tag)
     (let ([count (uref v (i+ 1 pos))])
       (if (or (not count)
               (i< (i+ 1 count) 2))
           (error 'get-type "not a type")
           (let ([ntype (make-vector (i- (helper v pos) pos))])
             (uset! ntype 0 'lamt)
             (uset! ntype 1 count)
             (let loop ([count (i+ 1 count)]
                        [pos (i+ 2 pos)]
                        [ntpos 2]) 
               (cond
                 [(i< count 1)
                  (values ntype pos)]
                 [else
                  (let-values ([(t? npos) (type? v pos)])
                    (if t?
                        (begin
                          (vector-copy! ntype ntpos v pos npos)
                               (loop (sub1 count)
                                     npos
                                     (i+ ntpos (i- npos pos))))
                        (error 'get-type "not a type")))]))
             )))]
    [else
     (error 'get-type "not a type")]))

(define (typecheck expr pos tenv)
  (define tag (uref expr pos))

  (cond
    [(exact-integer? tag)
     (values (vector 'int) (i+ 1 pos))]
    [(or (eq? 'true tag) (eq? 'false tag))
     (values (vector 'bool) (i+ 1 pos))]
    [(eq? 'null tag)
     (values (vector 'ntype) (i+ 1 pos))]
    [(eq? 'begin tag)
     (let ([count (uref expr (i+ 1 pos))])
       (if (not count)
           (error 'typecheck "bad form: ~a" expr)
           (for/fold ([res #f]
                      [p (i+ 2 pos)])
                     ([_ (in-range count)])
             (typecheck expr p tenv))))]
    [(eq? 'lambda tag) ;; 'lamt n pt_1 pt_2 ... pt_n body_type
     (let ([count (uref expr (i+ 1 pos))])
       (if (not count)
           (error 'typecheck "bad form: ~a" expr)
           (let*-values ([(new-tenv npos atypes)
                          (for/fold ([tenv_ tenv]
                                     [p (i+ 2 pos)]
                                     [tv (vector)])
                                    ([_ (in-range count)])        ;; extend env
                            (let-values ([(sym) (uref expr p)]
                                         [(type npos) (get-type expr (i+ 1 p))])
                              (values (extend-env tenv_ sym type)
                                      npos
                                      (vector-append tv type))))]
                         [(tbody npos2) (typecheck expr npos new-tenv)]) ;; tyepcheck body
             (values (vector-append (vector 'lamt count) atypes
                                    tbody)
                     npos2))))]
    [(eq? 'app tag)
     (let*-values ([(lamt npos) (typecheck expr (i+ 1 pos) tenv)]
                   [(arg_num) (uref expr npos)])
       (cond
         [(not (eq? (uref lamt 0) 'lamt))
          (error 'typecheck "no type ~a~n" expr)]
           ;; check params of lamt against args types. for loop here or something
         [else (let-values ([(b lampos_ npos) (let loop ([npos (i+ 1 npos)]
                                                         [lamtpos 2]
                                                         [count arg_num])
                                                (cond
                                                  [(i< count 1) (values #t lamtpos npos)]
                                                  [else
                                                   (let*-values ([(t_ np) (typecheck expr npos tenv)]
                                                                [(pt_ lnp) (get-type lamt lamtpos)]
                                                                [(bool a b) (type-equal? pt_ 0
                                                                              t_ 0)])
                                                     (if bool
                                                         (loop np lnp (sub1 count))
                                                         (values #f lamtpos npos)))]))])
                 (if b
                     (let-values ([(tmp1 tmp2) (get-type lamt lampos_)])
                       (values tmp1 npos))
                     (error 'typecheck "no type: ~a~n" expr)))]))]
    [(symbol? tag)
     (values (apply-env tenv tag) (i+ 1 pos))]
    [else
     (error 'typecheck "bad form: ~a" expr)]))

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
  (let-values ([(type a) (typecheck expr pos tenv)])
    type))

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