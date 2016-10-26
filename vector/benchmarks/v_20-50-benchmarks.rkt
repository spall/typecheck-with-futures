#lang racket

(require "vector-benchmark.rkt"
         "../sexp-to-vector.rkt"
         future-visualizer)

(define expr (gen-vector-stlc-exprs 20 50))
(displayln "expressions size 20/50")

(define 10-exprs (make-vector 10 expr))
(displayln "10 expressions")
(time-sequential-typechecker 10-exprs)

(define 50-exprs (make-vector 50 expr))
(displayln "50 expressions")
(time-sequential-typechecker 50-exprs)

(define 100-exprs (make-vector 100 expr))
(displayln "100 expressions")
(time-sequential-typechecker 100-exprs)

(define 200-exprs (make-vector 200 expr))
(displayln "200 expressions")
(time-sequential-typechecker 200-exprs)

(define 400-exprs (make-vector 400 expr))
(displayln "400 expressions")
(time-sequential-typechecker 400-exprs)