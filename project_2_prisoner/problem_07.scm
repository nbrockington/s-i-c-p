; Problem 7: Procedure MAKE-HIGHER-ORDER-SPASTIC:

(define	(MAKE-HIGHER-ORDER-SPASTIC list-of-strategies)
  (lambda (my-history other-history)
    ((list-ref list-of-strategies (remainder (length other-history)
      	      	      	      	      	     (length list-of-strategies)))
       my-history
       other-history)))
