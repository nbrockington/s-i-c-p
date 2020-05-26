; Problem 14: Edited (test-entry) to give missing data benefit of doubt:

(define (test-entry index trial)
  (cond ((null? index) (null? trial))
        ((null? trial) #f)
        ((or (or (eq? (car trial) '()) (eq? (car index) '()))
             (= (car index) (car trial)))
         (test-entry (cdr index) (cdr trial)))
        (else #f)))


; Procedure (could-be-soft-eye-for-eye?):

(define (could-be-soft-eye-for-eye? hist-0 hist-1 hist-2)
  (test-entry (list 1 1 0)
              (get-probability-of-c (make-history-summary hist-0
                                                          hist-1
                                                          hist-2))))

; Procedure (dont-tolerate-fools), which uses provided procedure
; (could-he-be-a-fool?):

(define (could-he-be-a-fool? hist0 hist1 hist2)
  (test-entry (list 1 1 1)
              (map (lambda (elt)
                      (cond ((null? elt) 1)
                            ((= elt 1) 1)
                            (else 0)))
                   (get-probability-of-c (make-history-summary hist0
                                                               hist1
                                                               hist2)))))

(define (DONT-TOLERATE-FOOLS my-hist other-hist-1 other-hist-2)
  (cond ((< (length my-hist) 10) "c")
        ((and (could-he-be-a-fool? other-hist-1 my-hist other-hist-2)
              (could-he-be-a-fool? other-hist-2 my-hist other-hist-1))
               "d")
               (else "c")))
               
               
               
