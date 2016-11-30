#lang racket

(require "../../method2/vector-stlc.rkt"
         (only-in "../../../sexp/s-exp-stlc.rkt"
                  [typecheck-sequential sexp-t-seq]))


(provide 
         compare-to-sexp)

(define (compare-to-sexp sexp-exprs vec-exprs types)
  (define ITER 10)
  (define l-sexp-exprs (list sexp-exprs))
  (define l-vec-exprs (list vec-exprs types))
  (let*-values ([(ssum vsum ssumgc vsumgc) (for/fold ([ssum_ 0][vsum_ 0][ssumgc_ 0][vsumgc_ 0])
                                       ([_ (in-range ITER)])
                               (let-values ([(a b srt gc)
                                             (time-apply sexp-t-seq l-sexp-exprs)]
                                            [(c d vrt egc)
                                             (begin
                                               (collect-garbage) (collect-garbage) (collect-garbage)
                                               (time-apply typecheck-sequential l-vec-exprs))])
                                 (collect-garbage) (collect-garbage) (collect-garbage)
                                 (values (+ ssum_ srt) (+ vsum_ vrt) (+ gc ssumgc_) (+ egc vsumgc_))))]
                [(savg) (/ ssum ITER)]
                [(vavg) (/ vsum ITER)]
                [(sgcavg) (/ ssumgc ITER)]
                [(vgcavg) (/ vsumgc ITER)])
    (printf "vector version: ~v%~n avg gc sexp: ~v~n  avg gc vector: ~v~n"
            (if (= 0 savg)
                0
                (exact->inexact (* 100 (/ (- vavg savg) savg))))
            sgcavg vgcavg
            )))
