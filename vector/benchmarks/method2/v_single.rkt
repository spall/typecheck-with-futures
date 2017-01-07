#lang racket

(require
  "../../method2/vector-stlc.rkt"
  "../../method2/sexp-to-vector.rkt"
  profile
  future-visualizer)

(define sexp (gen-well-formed-sexp 2 2))

(define-values (vec len types pos) (sexp->vector sexp (vector) 0))

(typecheck-expr vec types)


(visualize-futures (naive-typecheck-parallel (vector vec) (vector types)))
