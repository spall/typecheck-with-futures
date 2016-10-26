#lang racket

(require "benchmark.rkt"
         "../stlc-sexp-gen.rkt"
         future-visualizer)

(define expr (gen-well-formed-sexp 5 20))

(define 10-exprs (make-vector 10 expr))
(displayln "10 expressions")
(time-typechecker 10-exprs)

(define 50-exprs (make-vector 50 expr))
(displayln "50 expressions")
(time-typechecker 50-exprs)

(define 100-exprs (make-vector 100 expr))
(displayln "100 expressions")
(time-typechecker 100-exprs)

(define 200-exprs (make-vector 200 expr))
(displayln "200 expressions")
(time-typechecker 200-exprs)
