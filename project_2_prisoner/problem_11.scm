; Problem 11: Procedure MAKE-COMBINED-STRATEGIES:

(define (MAKE-COMBINED-STRATEGIES 2-player-strat0 2-player-strat1 combiner)
  (lambda (my-hist other-hist-1 other-hist-2)
    (combiner (2-player-strat0 my-hist other-hist-1)
              (2-player-strat1 my-hist other-hist-2))))
