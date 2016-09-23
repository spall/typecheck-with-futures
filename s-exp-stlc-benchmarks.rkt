#lang racket

(require "s-exp-stlc.rkt"
         "stlc-sexp-gen.rkt"
         "exprs.rkt")

;; tests
#|
(typecheck-expr 5)
(typecheck-expr 'true)
(typecheck-expr 'false)
(typecheck-expr `(lambda (x : bool) 5))
(typecheck-expr `((lambda (x : bool) 5) false))
(typecheck-expr `((lambda (x : bool) x) true))
(typecheck-expr `((lambda (x : int) (lambda (y : bool) x)) 22))
(typecheck-expr `(lambda (x : int) ((lambda (y : bool) x) true)))
(typecheck-expr (gen-well-formed-sexp 1000))
|#

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
   (for/list ([pc (in-range (min (pcount len)))])
     (future (λ ()
               (for ([e (in-vector exprs (* pc seq-size)
                                   (min len (* (+ 1 pc) seq-size)))])
                 (typecheck-expr e)))))))

;; gen-well-formed-sexp depth argnum
(define expr1 (gen-well-formed-sexp 5 20))
(define expr2 (gen-well-formed-sexp 10 50))
(define expr3 (gen-well-formed-sexp 20 50))
(define expr4 (gen-well-formed-sexp 20 100))


;; 10 exprs
(define 10-exprs1 (make-vector 10 expr1))
(define 10-exprs2 (make-vector 10 expr2))
(define 10-exprs3 (make-vector 10 expr3))
(define 10-exprs4 (make-vector 10 expr4))
(displayln "typecheck 10 expressions")

(displayln "time size 5/20")
(displayln "sequential")
(time (typecheck-sequential 10-exprs1))
(displayln "naive parallel")
(time (naive-typecheck-parallel 10-exprs1))
(displayln "better parallel")
(time (better-typecheck-parallel 10-exprs1))

(displayln "time size 10/50")
(displayln "sequential")
(time (typecheck-sequential 10-exprs2))
(displayln "naive parallel")
(time (naive-typecheck-parallel 10-exprs2))
(displayln "better parallel")
(time (better-typecheck-parallel 10-exprs2))

(displayln "time size 20/50")
(displayln "sequential")
(time (typecheck-sequential 10-exprs3))
(displayln "naive parallel")
(time (naive-typecheck-parallel 10-exprs3))
(displayln "better parallel")
(time (better-typecheck-parallel 10-exprs3))

(displayln "time size 20/100")
(displayln "sequential")
(time (typecheck-sequential 10-exprs4))
(displayln "naive parallel")
(time (naive-typecheck-parallel 10-exprs4))
(displayln "better parallel")
(time (better-typecheck-parallel 10-exprs4))


;; 50 exprs
(define 50-exprs1 (make-vector 50 expr1))
(define 50-exprs2 (make-vector 50 expr2))
(define 50-exprs3 (make-vector 50 expr3))
(define 50-exprs4 (make-vector 50 expr4))
(displayln "typecheck 50 expressions")

(displayln "time size 5/20")
(displayln "sequential")
(time (typecheck-sequential 50-exprs1))
(displayln "naive parallel")
(time (naive-typecheck-parallel 50-exprs1))
(displayln "better parallel")
(time (better-typecheck-parallel 50-exprs1))

(displayln "time size 10/50")
(displayln "sequential")
(time (typecheck-sequential 50-exprs2))
(displayln "naive parallel")
(time (naive-typecheck-parallel 50-exprs2))
(displayln "better parallel")
(time (better-typecheck-parallel 50-exprs2))

(displayln "time size 20/50")
(displayln "sequential")
(time (typecheck-sequential 50-exprs3))
(displayln "naive parallel")
(time (naive-typecheck-parallel 50-exprs3))
(displayln "better parallel")
(time (better-typecheck-parallel 50-exprs3))

(displayln "time size 20/100")
(displayln "sequential")
(time (typecheck-sequential 50-exprs4))
(displayln "naive parallel")
(time (naive-typecheck-parallel 50-exprs4))
(displayln "better parallel")
(time (better-typecheck-parallel 50-exprs4))

;; 100 exprs
(define 100-exprs1 (make-vector 100 expr1))
(define 100-exprs2 (make-vector 100 expr2))
(define 100-exprs3 (make-vector 100 expr3))
(define 100-exprs4 (make-vector 100 expr4))
(displayln "typecheck 50 expressions")

(displayln "time size 5/20")
(displayln "sequential")
(time (typecheck-sequential 100-exprs1))
(displayln "naive parallel")
(time (naive-typecheck-parallel 100-exprs1))
(displayln "better parallel")
(time (better-typecheck-parallel 100-exprs1))

(displayln "time size 10/50")
(displayln "sequential")
(time (typecheck-sequential 100-exprs2))
(displayln "naive parallel")
(time (naive-typecheck-parallel 100-exprs2))
(displayln "better parallel")
(time (better-typecheck-parallel 100-exprs2))

(displayln "time size 20/50")
(displayln "sequential")
(time (typecheck-sequential 100-exprs3))
(displayln "naive parallel")
(time (naive-typecheck-parallel 100-exprs3))
(displayln "better parallel")
(time (better-typecheck-parallel 100-exprs3))

(displayln "time size 20/100")
(displayln "sequential")
(time (typecheck-sequential 100-exprs4))
(displayln "naive parallel")
(time (naive-typecheck-parallel 100-exprs4))
(displayln "better parallel")
(time (better-typecheck-parallel 100-exprs4))










