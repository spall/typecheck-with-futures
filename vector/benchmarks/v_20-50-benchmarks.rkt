#lang racket

(require "vector-benchmark.rkt"
         "../sexp-to-vector.rkt"
         future-visualizer)

(define sexp-expr (gen-well-formed-sexp 20 50))
(define vec-expr (sexp->vector sexp-expr))
(displayln "expressions size 20/50")

(define 10-exprs (make-vector 10 sexp-expr))
(define v_10-exprs (make-vector 10 vec-expr))
(displayln "10 expressions")
(compare-to-sexp 10-exprs v_10-exprs)

(define 50-exprs (make-vector 50 sexp-expr))
(define v_50-exprs (make-vector 50 vec-expr))
(displayln "50 expressions")
(compare-to-sexp 50-exprs v_50-exprs)

(define 100-exprs (make-vector 100 sexp-expr))
(define v_100-exprs (make-vector 100 vec-expr))
(displayln "100 expressions")
(compare-to-sexp 100-exprs v_100-exprs)

(define 200-exprs (make-vector 200 sexp-expr))
(define v_200-exprs (make-vector 200 vec-expr))
(displayln "200 expressions")
(compare-to-sexp 200-exprs v_200-exprs)

(define 400-exprs (make-vector 400 sexp-expr))
(define v_400-exprs (make-vector 400 vec-expr))
(displayln "400 expressions")
(compare-to-sexp 400-exprs v_400-exprs)