; Problem 13: Procedure get-probability-of-c:

(define (get-probability-of-c summary)
  (define (prob-calc-helper list)
    (if	(= 0 (caddr list))
      	'()
        (* 1.0 (/ (car list) (caddr list)))))
  (let ((prob-of-cc-c (prob-calc-helper	(select-cc summary)))
        (prob-of-cd-c (prob-calc-helper	(select-cd summary)))
      	(prob-of-dd-c (prob-calc-helper	(select-dd summary))))
    (list prob-of-cc-c prob-of-cd-c prob-of-dd-c)))


; Some selectors for the history-summary data structure: 

(define (select-cc summary)
  (car summary))

(define (select-cd summary)
  (cadr summary))

(define (select-dd summary)
  (caddr summary))
