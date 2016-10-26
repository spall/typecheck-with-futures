#lang racket

(require
  "../s-exp-stlc.rkt"
         "../stlc-sexp-gen.rkt"
         future-visualizer)

;; expressions of size 10/50
              
(define expr (gen-well-formed-sexp 30 100))
(time (typecheck-expr expr))

#|
(define exprs (make-vector 10 expr))

(visualize-futures (better-typecheck-parallel exprs))|#
