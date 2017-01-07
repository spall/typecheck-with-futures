#lang racket

(require "../../method2/vector-stlc.rkt"
         (only-in "../../../sexp/s-exp-stlc.rkt"
                  [typecheck-sequential sexp-t-seq]
                  [naive-typecheck-parallel sexp-naive]
                  [better-typecheck-parallel sexp-better]))


(provide 
         compare-to-sexp
         compare-to-parallel-sexp
         time-parallel-typechecker)

(define (compare-to-sexp sexp-exprs vec-exprs types)
  (define ITER 20)
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

(define (compare-to-parallel-sexp sexp-exprs vec-exprs types)
  (define ITER 10)
  (define list-sexprs (list sexp-exprs))
  (define list-exprs (list vec-exprs types))
  (let*-values ([(ssum nsum bsum) (for/fold ([ssum_ 0] [nsum_ 0] [bsum_ 0])
                                            ([_ (in-range ITER)])
                                    (let-values ([(a b srt c) (time-apply sexp-naive list-sexprs)]
                                                 [(f e nrt d) (begin (collect-garbage) (collect-garbage) (collect-garbage)
                                                                     (time-apply naive-typecheck-parallel list-exprs))]
                                                 [(g h brt i) (begin (collect-garbage) (collect-garbage) (collect-garbage)
                                                                     (time-apply better-typecheck-parallel list-exprs))])
                                      (collect-garbage)(collect-garbage)(collect-garbage)
                                      (values (+ ssum_ srt) (+ nsum_ nrt) (+ bsum_ brt))))]
                [(savg) (/ ssum ITER)]
                [(navg) (/ nsum ITER)]
                [(bavg) (/ bsum ITER)])
    (if (= 0 savg)
        (printf "sexp parallel average is zero")
        (printf "sexp naive: ~v~n naive vector: ~v%    better vector: ~v%~n"
                (exact->inexact savg)
                (exact->inexact (* 100 (/ (- navg savg) savg)))
                (exact->inexact (* 100 (/ (- bavg savg) savg)))))))

;; parallel timing
(define (time-parallel-typechecker exprs types)
  (define ITER 10)
  (define list-exprs (list exprs types))
  (let*-values ([(ssum nsum bsum) (for/fold ([ssum_ 0] [nsum_ 0] [bsum_ 0])
                                            ([_ (in-range ITER)])
                                    (let-values ([(a b srt c) (time-apply typecheck-sequential list-exprs)]
                                                 [(f e nrt d) (begin (collect-garbage) (collect-garbage) (collect-garbage)
                                                                     (time-apply naive-typecheck-parallel list-exprs))]
                                                 [(g h brt i) (begin (collect-garbage) (collect-garbage) (collect-garbage)
                                                                     (time-apply better-typecheck-parallel list-exprs))])
                                      (collect-garbage)(collect-garbage)(collect-garbage)
                                      (values (+ ssum_ srt) (+ nsum_ nrt) (+ bsum_ brt))))]
                [(savg) (/ ssum ITER)]
                [(navg) (/ nsum ITER)]
                [(bavg) (/ bsum ITER)])
    (if (= 0 savg)
        (printf "sequential average is zero")
        (printf "sequential: ~v~n naive: ~v%    better: ~v%~n"
                (exact->inexact savg)
                (exact->inexact (* 100 (/ (- navg savg) savg)))
                (exact->inexact (* 100 (/ (- bavg savg) savg)))))))
