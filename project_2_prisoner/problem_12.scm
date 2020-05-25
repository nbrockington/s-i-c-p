; Problem 12: Procedure make-history-summary, i.e., the constructor for the history-summary data structure:

(define (make-history-summary hist-0 hist-1 hist-2)
  (define (make-summary-helper hist-0 prev-1 prev-2
                               cc-c cc-d cd-c cd-d dd-c dd-d)
    (cond ((empty-history? prev-1)
           (list (list cc-c cc-d (+ cc-c cc-d))
                 (list cd-c cd-d (+ cd-c cd-d))
                 (list dd-c dd-d (+ dd-c dd-d))))
          ((and (string=? (car prev-1) "c")
                (string=? (car prev-2) "c"))
           (if (string=? (car hist-0) "c")
               (make-summary-helper (cdr hist-0) (cdr prev-1) (cdr prev-2)
                 (+ 1 cc-c) cc-d cd-c cd-d dd-c dd-d)
               (make-summary-helper (cdr hist-0) (cdr prev-1) (cdr prev-2)
                 cc-c (+ 1 cc-d) cd-c cd-d dd-c dd-d)))
          ((and (string=? (car prev-1) "d")
                (string=? (car prev-2) "d"))
           (if (string=? (car hist-0) "c")
               (make-summary-helper (cdr hist-0) (cdr prev-1) (cdr prev-2)
                 cc-c cc-d cd-c cd-d (+ 1 dd-c) dd-d)
               (make-summary-helper (cdr hist-0) (cdr prev-1) (cdr prev-2)
                 cc-c cc-d cd-c cd-d dd-c (+ 1 dd-d))))
          (else
           (if (string=? (car hist-0) "c")
               (make-summary-helper (cdr hist-0) (cdr prev-1) (cdr prev-2)
                 cc-c cc-d (+ 1 cd-c) cd-d dd-c dd-d)
               (make-summary-helper (cdr hist-0) (cdr prev-1) (cdr prev-2)
                 cc-c cc-d cd-c (+ 1 cd-d) dd-c dd-d)))))
  (make-summary-helper hist-0 (cdr hist-1) (cdr hist-2) 0 0 0 0 0 0))


; Some selectors for the history-summary data structure: 

(define (select-cc summary)
  (car summary))

(define (select-cd summary)
  (cadr summary))

(define (select-dd summary)
  (caddr summary))
