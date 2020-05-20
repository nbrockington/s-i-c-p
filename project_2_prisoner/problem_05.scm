; Problem 5: Procedure EYE-FOR-N-EYES

(define (EYE-FOR-N-EYES n)
  (define (previous-n-defected? history n)
    (define (check-defected-iter history n count)
      (cond ((= n count) #t)
            ((empty-history? history) #f)
            ((string=? "c" (car history)) #f)
            (else (check-defected-iter (cdr history) n (+ 1 count)))))
    (check-defected-iter history n 0))
  (lambda (my-history other-history)
    (if (previous-n-defected? other-history n)
        "d" "c")))
