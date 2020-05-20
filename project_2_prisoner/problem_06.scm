; Problem 6: Procedure MAKE-ROTATING-STRATEGY:

(define (MAKE-ROTATING-STRATEGY strat0 strat1 freq0 freq1)
  (lambda (my-history other-history)
    (if (< (remainder (length other-history) (+ freq0 freq1)) freq0)
        (strat0 my-history other-history)
        (strat1 my-history other-history))))
