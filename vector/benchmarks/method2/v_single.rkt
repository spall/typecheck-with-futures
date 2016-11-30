#lang racket

(require
  "../../method2/vector-stlc.rkt"
  "../../method2/sexp-to-vector.rkt"
  profile)

;; expressions of size 10/50

(define sexp (gen-well-formed-sexp 2 2))
;;(define sexp '((lambda #((1 : int)) (begin #(true 12 false 6 true false 26 false 64 true 67 true 92 63 34 true true true 68 true))) 3))
;;(define sexp '(((lambda #((1 : bool)) (lambda #((2 : ((int int) int)))
  ;;                                      (begin #(true false false false 89 false true 74 37 true false 4 true false false true 79 true true 61)))) true)
    ;;           (lambda #((3 : int)) (lambda #((4 : int)) 31))))
(define-values (vec len types pos) (sexp->vector sexp (vector) 0))

;;(define expr (vector 'app 'lambda 1 'x (vector 'int) 5 1 5))
(displayln sexp)
(displayln vec)
(displayln types)
(typecheck-expr vec types)

;; lambda args are backwarsd?

#|
(define exprs (make-vector 10 expr))

(visualize-futures (better-typecheck-parallel exprs))|#

;;(((lambda #((1 : int) (2 : int)) (lambda #((3 : int) (4 : int)) (begin #(false false 3 false true false 28 true false 45 false 7 44 true false false true 23 false false)))) 12 71) 66 37)