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
                (let ([adv_ (type-equal? (uref v1 (i+ adv pos1)) 0
                                         (uref v2 (i+ adv pos2)) 0)]);;([adv_ (type-equal? v1 (i+ adv pos1) v2 (i+ adv pos2))])
                  (if (not adv_)
                      #f
                      (loop2 (sub1 count) (i+ adv 1))))]))))] ;; right?
    [else
     #f]))

;; typechecker that gathers constraints as it goes and then checks them all at the end.


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
       (let loop ([p (i+ 2 pos)]
                  [c count])
         (if (i< c 2)
             (typecheck expr p tenv out out-pos)
             (let-values ([(p1 _) (typecheck expr p tenv out out-pos)])
               (loop p1 (i- c 1))))))]
    [(eq? 'lambda tag) ;; 'lamt n pt_1 pt_2 ... pt_n body_type
     (let ([count (uref expr (i+ 1 pos))])
       (uset! out out-pos 'lamt)
       (uset! out (i+ 1 out-pos) count)
       ;; extend env
       (let loop ([tenv_ tenv]
                  [p (i+ 2 pos)]
                  [tv-pos (i+ 2 out-pos)]
                  [c count])
         (if (< c 1)
             (let ([bodyv (make-vector (i- (uvec-len out) tv-pos))]) ;; need to typecheck body and put in a new vector.
               (let-values ([(tmp1 _) (typecheck expr p tenv_ bodyv 0)])
                 (uset! out tv-pos bodyv)
                 (values tmp1 (i+ 1 tv-pos))))
            ;; (typecheck expr p tenv_ out tv-pos) ;; typecheck body
             (let* ([sym (uref expr p)]
                    [atype (uref expr (i+ 1 p))])  ;;   [npos (get-type expr (i+ 1 p))]) ;; replace this
               (uset! out tv-pos atype)
             ;;  (vector-copy! out tv-pos expr (i+ 1 p) npos)
               (loop (extend-env tenv_ sym atype)
                     (i+ 2 p)
                     (i+ 1 tv-pos)
                     (i- c 1))))))]
    [(eq? 'app tag)
     (let ([lamt (make-vector (i- (uvec-len expr) pos))])
       (let-values ([(npos _) (typecheck expr (i+ 1 pos) tenv lamt 0)])
         (let ([arg_num (uref expr npos)])
           (cond
             [(not (eq? (uref lamt 0) 'lamt))
              (error 'typecheck "no type ~a~n" expr)]
             [else
              (let loop5 ([npos (i+ 1 npos)] ;; position in expr
                          [lamtpos 2] ;; position in lamt
                          [count arg_num]) ;; how many times we loop
                (cond
                  [(i< count 1)
                   (let ([btype (uref lamt lamtpos)])
                     (vector-copy! out out-pos btype)
                     (values npos (i+ out-pos (uvec-len btype))))]
                  [else ;; here
                   (let-values ([(np nop) (typecheck expr npos tenv out out-pos)])
                     (let* ([ptype (uref lamt lamtpos)]
                            [adv (type-equal? ptype 0
                                              out out-pos)])
                       (if (not adv)
                           (error 'typecheck "no type: ~a~n" expr)
                           (loop5 np (i+ 1 lamtpos) (sub1 count)))))]))]))))]
    [(symbol? tag)
     (let ([type (apply-env tenv tag)])
      ;; (vector-copy! out out-pos type)
       (vector-copy! out out-pos type)
       (values (i+ 1 pos) (i+ out-pos 1)))]
    [else
     (error 'typecheck "bad form: ~a" expr)]))


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
  (define out (make-vector (uvec-len expr)))
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