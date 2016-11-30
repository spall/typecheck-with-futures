#lang racket

(require "vector-benchmark.rkt"
         "../../method2/sexp-to-vector.rkt"
         future-visualizer)

(define sexp-expr (gen-well-formed-sexp 150 500))
(define-values (vec-expr _ types b) (sexp->vector sexp-expr (vector) 0))
(displayln "expressions size 20/50")

(define 1-exprs (make-vector 1 sexp-expr))
(define v_1-exprs (make-vector 1 vec-expr))
(define v_1-types (make-vector 1 types))
(displayln "1 expressions")
(compare-to-sexp 1-exprs v_1-exprs v_1-types)

(define 10-exprs (make-vector 10 sexp-expr))
(define v_10-exprs (make-vector 10 vec-expr))
(define v_10-types (make-vector 10 types))
(displayln "10 expressions")
(compare-to-sexp 10-exprs v_10-exprs v_10-types)

(define 50-exprs (make-vector 50 sexp-expr))
(define v_50-exprs (make-vector 50 vec-expr))
(define v_50-types (make-vector 50 types))
(displayln "50 expressions")
(compare-to-sexp 50-exprs v_50-exprs v_50-types)

(define 100-exprs (make-vector 100 sexp-expr))
(define v_100-exprs (make-vector 100 vec-expr))
(define v_100-types (make-vector 100 types))
(displayln "100 expressions")
(compare-to-sexp 100-exprs v_100-exprs v_100-types)

(define 200-exprs (make-vector 200 sexp-expr))
(define v_200-exprs (make-vector 200 vec-expr))
(define v_200-types (make-vector 200 types))
(displayln "200 expressions")
(compare-to-sexp 200-exprs v_200-exprs v_200-types)

(define 400-exprs (make-vector 400 sexp-expr))
(define v_400-exprs (make-vector 400 vec-expr))
(define v_400-types (make-vector 400 types))
(displayln "400 expressions")
(compare-to-sexp 400-exprs v_400-exprs v_400-types)