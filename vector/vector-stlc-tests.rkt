#lang racket

(require "vector-stlc.rkt")

(type? (vector 'int) 0)
(type? (vector 'bool) 0)
(type? (vector 'ntype) 0)
(type? (vector 'lamt 2 'int 'bool 'ntype) 0)
(type? (vector 'lamt 2 'int 'bool 'bool 'lamt 1 'int 'int) 0)
(type? (vector 'b) 0)
(type? (vector 'lamt 2 'int) 0)

(displayln "type-equal tests")
(type-equal? (vector 'int) 0 (vector 'int) 0)
(type-equal? (vector 'bool) 0 (vector 'bool) 0)
(type-equal? (vector 'ntype) 0 (vector 'ntype) 0)
(type-equal? (vector 'int) 0 (vector 'bool) 0)
(type-equal? (vector 'lamt 2 'int 'bool 'bool 'lamt 1 'int 'int) 0
             (vector 'lamt 2 'int 'bool 'bool 'lamt 1 'int 'int) 0)
(type-equal? (vector 'lamt 2 'int 'bool 'bool 'lamt 1 'int 'int) 0
             (vector 'lamt 2 'int 'bool 'ntype) 0)


