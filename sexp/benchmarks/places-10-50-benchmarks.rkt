#lang racket

(require "../s-exp-stlc.rkt"
         "../stlc-sexp-gen.rkt"
         future-visualizer)

(define expr (gen-well-formed-sexp 10 50))

(define 10-exprs (make-vector 10 expr))
(displayln "10 expressions")
(displayln "sequential")
(time (typecheck-sequential 10-exprs))
(collect-garbage)(collect-garbage)
(displayln "places parallel")
(time (places-typecheck-parallel 10-exprs))
(collect-garbage)(collect-garbage)

(define 50-exprs (make-vector 50 expr))
(displayln "50 expressions")
(displayln "sequential")
(time (typecheck-sequential 50-exprs))
(collect-garbage)(collect-garbage)
(displayln "places parallel")
(time (places-typecheck-parallel 50-exprs))
(collect-garbage)(collect-garbage)

(define 100-exprs (make-vector 100 expr))
(displayln "100 expressions")
(displayln "sequential")
(time (typecheck-sequential 100-exprs))
(collect-garbage)(collect-garbage)
(displayln "places parallel")
(time (places-typecheck-parallel 100-exprs))
(collect-garbage)(collect-garbage)

(define 200-exprs (make-vector 200 expr))
(displayln "200 expressions")
(displayln "sequential")
(time (typecheck-sequential 200-exprs))
(collect-garbage)(collect-garbage)
(displayln "places parallel")
(time (places-typecheck-parallel 200-exprs))
