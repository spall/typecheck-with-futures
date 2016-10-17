#lang racket

(provide type?
         type-equal?)

(require (rename-in racket/unsafe/ops
                    [unsafe-car ucar]
                    [unsafe-cdr ucdr]))
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
  (define size (vector-length v))
  (if (< pos size)
      (vector-ref v pos)
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
  (define tag (safe-vref v pos))
  
  (cond
    [(eq? 'int tag)
     (values #t (add1 pos))]
    [(eq? 'bool tag)
     (values #t (add1 pos))]
    [(eq? 'ntype tag)
     (values #t (add1 pos))]
    [(eq? 'lamt tag)
     (let ([count (safe-vref v (add1 pos))])
       (if (or (not count)
               (< (+ 1 count) 2))
           (values #f (add1 pos))
           (let loop ([count (+ 1 count)]
                      [pos (+ 2 pos)]) 
             (cond
               [(< count 1)
                (values #t pos)]
               [else
                (let-values ([(t? npos) (type? v pos)])
                  (if t?
                      (loop (sub1 count)
                            npos)
                      (values #f (add1 pos))))]))))]
    [else
     (values #f (add1 pos))]))

(define (type-equal? v1 pos1 v2 pos2)
  (define tag1 (safe-vref v1 pos1))
  (define tag2 (safe-vref v2 pos2))
  
  (cond
    [(and (eq? 'int tag1)
          (eq? 'int tag2))
     (values #t (add1 pos1) (add1 pos2))]
    [(and (eq? 'bool tag1)
          (eq? 'bool tag2))
     (values #t (add1 pos1) (add1 pos2))]
    [(and (eq? 'ntype tag1)
          (eq? 'ntype tag2))
     (values #t (add1 pos1) (add1 pos2))]
    [(and (eq? 'lamt tag1)
          (eq? 'lamt tag2)) ;; are counts the same?
     (let ([count1 (safe-vref v1 (add1 pos1))]
           [count2 (safe-vref v2 (add1 pos2))])
       (if (or (not count1)
               (not count2)
               (not (eqv? (add1 count1) (add1 count2)))
               (< (add1 count1) 2))
           (values #f (add1 pos1) (add1 pos2))
           (let loop ([count (add1 count1)]
                      [pos1 (+ 2 pos1)]
                      [pos2 (+ 2 pos2)])
             (cond
               [(< count 1)
                (values #t pos1 pos2)]
               [else
                (let-values ([(t? npos1 npos2) (type-equal? v1 pos1 v2 pos2)])
                  (if t?
                      (loop (sub1 count) npos1 npos2)
                      (values #f (add1 pos1) (add1 pos2))))]))))]
    [else
     (values #f (add1 pos1) (add1 pos2))]))

(define (helper v start)
  (define tag (vector-ref v start))
  (cond
    [(eq? 'lamt tag)
     (let ([count (safe-vref v (add1 start))])
       (if (or (not count)
               (< (+ 1 count) 2))
           (error 'helper "not a type")
           (let loop ([count (+ 1 count)]
                      [pos (+ 2 start)])
             (cond
               [(< count 1)
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
  (define tag (vector-ref v pos))
  (cond
    [(or (eq? 'int tag) (eq? 'bool tag) (eq? 'ntype tag))
     (values (vector tag) (add1 pos))]
    [(eq? 'lamt tag)
     (let ([count (safe-vref v (add1 pos))])
       (if (or (not count)
               (< (+ 1 count) 2))
           (error 'get-type "not a type")
           (let ([ntype (make-vector (- (helper v pos) pos))])
             (vector-set! ntype 0 'lamt)
             (vector-set! ntype 1 count)
             (let loop ([count (+ 1 count)]
                        [pos (+ 2 pos)]
                        [ntpos 2]) 
               (cond
                 [(< count 1)
                  (values ntype pos)]
                 [else
                  (let-values ([(t? npos) (type? v pos)])
                    (if t?
                        (begin (displayln ntype)
                          (vector-copy! ntype ntpos v pos npos)
                               (loop (sub1 count)
                                     npos
                                     (+ ntpos (- npos pos))))
                        (error 'get-type "not a type")))]))
             )))]
    [else
     (error 'get-type "not a type")]))

(define (typecheck expr pos tenv)
  (define tag (safe-vref expr pos))

  (cond
    [(exact-integer? tag)
     (values (vector 'int) (add1 pos))]
    [(or (eq? 'true tag) (eq? 'false tag))
     (values (vector 'bool) (add1 pos))]
    [(eq? 'null tag)
     (values (vector 'ntype) (add1 pos))]
    [(eq? 'begin tag)
     (let ([count (safe-vref expr (add1 pos))])
       (if (not count)
           (error 'typecheck "bad form: ~a" expr)
           (for/fold ([res #f]
                      [p (+ 2 pos)])
                     ([_ (in-range count)])
             (typecheck expr p tenv))))]
    [(eq? 'lambda tag) ;; 'lamt n pt_1 pt_2 ... pt_n body_type
     (let ([count (safe-vref expr (add1 pos))])
       (if (not count)
           (error 'typecheck "bad form: ~a" expr)
           (let*-values ([(new-tenv npos atypes)
                          (for/fold ([tenv_ tenv]
                                     [p (+ 2 pos)]
                                     [tv (vector)])
                                    ([_ (in-range count)])        ;; extend env
                            (let-values ([(sym) (safe-vref expr p)]
                                         [(type npos) (get-type expr (add1 p))])
                              (values (extend-env tenv_ sym type)
                                      npos
                                      (vector-append tv type))))]
                         [(tbody npos2) (typecheck expr npos new-tenv)]) ;; tyepcheck body
             (values (vector-append (vector 'lamt count) atypes
                                    tbody)
                     npos2))))]
    [(eq? 'app tag)
     (let*-values ([(lamt npos) (typecheck expr (add1 pos))]
                   [(arg_num) (safe-vref expr (add1 npos))])
       (cond
         [(not (eq? (safe-vref lamt 0) 'lamt))
          (error 'typecheck "no type ~a~n" expr)]
           ;; check params of lamt against args types. for loop here or something
         [else (let-values ([(b lampos_ npos) (let loop ([npos (add1 npos)]
                                                         [lamtpos 0]
                                                         [count arg_num])
                                                (cond
                                                  [(< count 1) (values #t lamtpos npos)]
                                                  [else
                                                   (let-values ([(t_ np) (typecheck expr npos tenv)]
                                                                [(pt_ lnp) (get-type lamt lamtpos)])
                                                     (if (type-equal? pt_
                                                                      t_)
                                                         (loop np lnp (sub1 count))
                                                         (values #f lamtpos npos)))]))])
                 (if b
                     (values (get-type lamt lampos_) npos)
                     (error 'typecheck "no type: ~a~n" expr)))]))]
    [(symbol? tag)
     (values (apply-env tenv tag) (add1 pos))]
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
  (let-values ([(type _) (typecheck expr pos tenv)])
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
  (define len (vector-length exprs))
  (define seq-size (ceiling (/ len pcount)))
  
  (for-each
   touch
   (for/list ([pc (in-range (min pcount len))])
     (future (λ ()
               (for ([e (in-vector exprs (* pc seq-size)
                                   (min len (* (+ 1 pc) seq-size)))])
                 (typecheck-expr e)))))))