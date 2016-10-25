#lang racket

(require
  "../vector-stlc.rkt"
         "../sexp-to-vector.rkt"
         profile)

;; expressions of size 10/50
              
(define expr (gen-vector-stlc-exprs 20 100))
;;(displayln expr)
(void (typecheck-expr expr))

#|
(define exprs (make-vector 10 expr))

(visualize-futures (better-typecheck-parallel exprs))|#