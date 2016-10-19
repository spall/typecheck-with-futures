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

(define (extend-env env sym start end expr)
  (cons (list sym start end expr) env))

(define (apply-env env sym)
  (cond
    [(empty? env)
     (error 'apply-env "Could not find variable ~v~n" sym)]
    [(eq? (ucar (ucar env)) sym)
     (ucdr (ucar env))] ;; return (list start end expr)
    [else
     (apply-env (ucdr env) sym)]))

;; try to get rid of (values ...) with single return value
;; pass around an output vector and position to typecheck ;; just allocate a very large vector that is too big....


;; return #f or a pos
(define (type? v pos)
   (define tag (uref v pos))
   
   (cond
     [(eq? 'int tag)
      (i+ 1 pos)]
     [(eq? 'bool tag)
      (i+ 1 pos)]
     [(eq? 'ntype tag)
      (i+ 1 pos)]
     [(eq? 'lamt tag)
      (let ([count (uref v (i+ 1 pos))])
        (if (or (not count)
                (i< (i+ 1 count) 2))
            #f
            (let loop1 ([count (i+ 1 count)]
                        [pos (i+ 2 pos)]) 
              (cond
                [(i< count 1)
                 pos]
                [else
                 (let ([npos (type? v pos)])
                   (if (not npos)
                       #f
                       (loop1 (sub1 count)
                              npos)))]))))]
     [else
      #f]))


;; return #f or how much we've advanced in 2 vectors
(define (type-equal? v1 pos1 v2 pos2)
  (define tag1 (uref v1 pos1))
  (define tag2 (uref v2 pos2))
  
  (cond
    [(and (eq? 'int tag1)
          (eq? 'int tag2))
     1]
    [(and (eq? 'bool tag1)
          (eq? 'bool tag2))
     1]
    [(and (eq? 'ntype tag1)
          (eq? 'ntype tag2))
     1]
    [(and (eq? 'lamt tag1)
          (eq? 'lamt tag2)) ;; are counts the same?
     (let ([count1 (uref v1 (i+ 1 pos1))]
           [count2 (uref v2 (i+ 1 pos2))])
       (if (or (not count1) (not count2)
               (not (eqv? (i+ 1 count1) (i+ 1 count2)))
               (i< (i+ 1 count1) 2))
           #f
           (let loop2 ([count (i+ 1 count1)]
                       [adv 2])
             (cond
               [(i< count 1)
                adv]
               [else
                (let ([adv_ (type-equal? v1 (i+ adv pos1) v2 (i+ adv pos2))])
                  (if (not adv_)
                      #f
                      (loop2 (sub1 count) (i+ adv adv_))))]))))] ;; right?
    [else
     #f]))

;; used by get-type
(define (helper v start)
  (define count (uref v (i+ 1 start)))
  (let loop3 ([count (i+ 1 count)]
              [pos (i+ 2 start)])
    (cond
      [(i< count 1)
       pos]
      [else
       (let ([npos (type? v pos)])
         (if (not npos)
             (error 'helper "not a type")
             (loop3 (sub1 count)
                    npos)))])))

;; returns pos after end of this type.
(define (get-type v pos)
  (define tag (uref v pos))
  (cond
    [(or (eq? 'int tag) (eq? 'bool tag) (eq? 'ntype tag))
     (i+ 1 pos)]
    [(eq? 'lamt tag)
     (let ([count (uref v (i+ 1 pos))])
       (if (i< (i+ 1 count) 2)
           (error 'get-type "not a type")
           (helper v pos)))]
    [else
     (error 'get-type "not a type")]))

(define (typecheck expr pos tenv out out-pos)
  (define tag (uref expr pos))
  (cond
    [(exact-integer? tag)
     (begin (uset! out out-pos 'int)
            (values (i+ 1 pos) (i+ 1 out-pos)))]
    [(or (eq? 'true tag) (eq? 'false tag))
     (begin (uset! out out-pos 'bool)
            (values (i+ 1 pos) (i+ 1 out-pos)))]
    [(eq? 'null tag)
     (begin (uset! out out-pos 'ntype)
            (values (i+ 1 pos) (i+ 1 out-pos)))]
    [(eq? 'begin tag)
     (let ([count (uref expr (i+ 1 pos))]) 
       (for/fold ([p (i+ 2 pos)]
                  [_ out-pos])
                 ([_ (in-range count)])
         (typecheck expr p tenv out out-pos)))]
    [(eq? 'lambda tag) ;; 'lamt n pt_1 pt_2 ... pt_n body_type
     (let ([count (uref expr (i+ 1 pos))])
       (uset! out out-pos 'lamt)
       (uset! out (i+ 1 out-pos) count)
       ;; extend env
       (let*-values ([(new-tenv npos nout-pos)
                      (for/fold ([tenv_ tenv]
                                 [p (i+ 2 pos)]
                                 [tv-pos (i+ 2 out-pos)])
                                ([_ (in-range count)])
                        (let* ([sym (uref expr p)]
                               [npos (get-type expr (i+ 1 p))])
                          (vector-copy! out tv-pos expr (i+ 1 p) npos)
                          (values (extend-env tenv_ sym (i+ 1 p) npos expr)
                                  npos
                                  (i+ tv-pos (i- npos (i+ 1 p))))))])
         (typecheck expr npos new-tenv out nout-pos)))]
    [(eq? 'app tag)
     (let*-values ([(lamt) (make-vector (i- (vector-length expr) pos))]
                   [(npos _) (typecheck expr (i+ 1 pos) tenv lamt 0)]
                   [(arg_num) (uref expr npos)])
       (cond
         [(not (eq? (uref lamt 0) 'lamt))
          (error 'typecheck "no type ~a~n" expr)]
         [else (let-values ([(b lampos_ npos)
                             (let loop5 ([npos (i+ 1 npos)] ;; position in expr
                                         [lamtpos 2] ;; position in lamt
                                         [count arg_num]) ;; how many times we loop
                               (cond
                                 [(i< count 1) (values #t lamtpos npos)]
                                 [else
                                  (let-values ([(np nop) (typecheck expr npos tenv out out-pos)])
                                    (let ([lnp (get-type lamt lamtpos)]
                                          [adv (type-equal? lamt lamtpos
                                                            out out-pos)])
                                      (if (not adv)
                                          (values #f lamtpos npos)
                                          (loop5 np lnp (sub1 count)))))]))])
                 (if b
                     (let ([end-body (get-type lamt lampos_)])
                       (vector-copy! out out-pos lamt lampos_ end-body)
                       (values npos (i+ out-pos (i- end-body lampos_))))
                     (error 'typecheck "no type: ~a~n" expr)))]))]
    [(symbol? tag)
     (let* ([ls (apply-env tenv tag)]
            [start (ucar ls)]
            [end (ucar (ucdr ls))]
            [e (ucar (ucdr (ucdr ls)))])
       (vector-copy! out out-pos e start end)
       (values (i+ 1 pos) (i+ out-pos (i- end start))))]
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
  (define out (make-vector (vector-length expr)))
  (let-values ([(_ __)
                                      (typecheck expr pos tenv out 0)])
                          out))
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