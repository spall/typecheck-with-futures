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

(displayln "done generating")

;; bulk of time is spent generating

#|
(typecheck-expr `(lambda (x : bool) x))
(time (typecheck-expr (gen-well-formed-sexp 5000)))
(time (typecheck-expr (gen-well-formed-sexp 5000)))
(time (typecheck-expr (gen-well-formed-sexp 5000)))
(time (typecheck-expr (gen-well-formed-sexp 5000)))
|#

#|
(define (parallel-typecheck-expr exprs)
  (for-each
   touch
   (map (lambda (e) (future (lambda () (typecheck-expr e))))
        exprs)))
|#
#|
(for ([_ (in-range 1000)])
  (for-each (lambda (e) (time (typecheck-expr e)))
            large-expression))

(displayln "running in parallel")
|#

#|
(typecheck-expr expr1)
(typecheck-expr expr2)
|#

#|
(for ([_ (in-range 1000)])
(for-each
       touch
       (map (lambda (e) (future (lambda () (typecheck-expr e))))
            (list expr1))))
|#

(define expr (gen-well-formed-sexp 290))
(for ([_ (in-range 1000)])
(define f (future (lambda () (typecheck-expr expr))))
(map cons '(1 2 3) '(1 2 3))
  (touch f))



#|
(for ([_ (in-range 1000)])
  (displayln (typecheck-expr expr1)))
|#








