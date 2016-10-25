#lang racket

(require "../vector-stlc.rkt")
(provide time-typechecker
         time-sequential-typechecker)

(define (time-typechecker exprs)
  (define ITER 10)
  (define list-exprs (list exprs))
  (let*-values ([(ssum nsum bsum) (for/fold ([ssum_ 0]
                                                  [nsum_ 0]
                                                  [bsum_ 0])
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
    (printf "naive: ~v%    better: ~v%~n"
            (exact->inexact (* 100 (/ (- navg savg) savg)))
            (exact->inexact (* 100 (/ (- bavg savg) savg))))))

(define (time-sequential-typechecker exprs)
  (define ITER 1)
  (define list-exprs (list exprs))
  (let*-values ([(ssum gcsum) (for/fold ([ssum_ 0]
                                         [gcsum_ 0])
                                        ([_ (in-range ITER)])
                                (let-values ([(a b srt gc)
                                              (time-apply typecheck-sequential list-exprs)])
                                  (collect-garbage) (collect-garbage) (collect-garbage)
                                  (values (+ ssum_ srt) (+ gcsum_ gc))))])
    (printf "avg sequential: ~v~n  avg gc: ~v~n"
            (exact->inexact (/ ssum ITER))
            (exact->inexact (/ gcsum ITER)))))