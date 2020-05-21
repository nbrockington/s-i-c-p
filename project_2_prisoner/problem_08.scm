; Problem 8: Procedure GENTLE:

(define	(GENTLE	strat gentleness-factor)
  (lambda (my-history other-history)
    (if	(and (string=? (strat my-history other-history) "d")
      	     (>	(random	1.0) gentleness-factor))
      	"d" "c")))
