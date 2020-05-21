; Problem 8: Procedure GENTLE:

(define	(GENTLE	strat gentleness-factor)
  (lambda (my-history other-history)
    (if	(and (string=? (strat my-history other-history) "d")
      	     (>	(random	1.0) gentleness-factor))
      	"d" "c")))


; Procedure SLIGHTLY-GENTLE-NASTY:

(define SLIGHTLY-GENTLE-NASTY (GENTLE NASTY 0.1))

; Procedure SLIGHT-GENTLE-EYE-FOR-EYE:

(define SLIGHTLY-GENTLE-EYE-FOR-EYE (GENTLE EYE-FOR-EYE 0.1))
