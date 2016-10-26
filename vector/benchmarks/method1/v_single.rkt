#lang racket

(require
  "../../method1/vector-stlc.rkt"
  "../../method1/sexp-to-vector.rkt"
  profile)

;; expressions of size 10/50
              
(define expr (gen-vector-stlc-exprs 10 50))

;;(define expr (vector 'app 'lambda 1 'x (vector 'int) 5 1 5))
(typecheck-expr expr)

#|
(define exprs (make-vector 10 expr))

(visualize-futures (better-typecheck-parallel exprs))|#
