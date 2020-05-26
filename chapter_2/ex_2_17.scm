; Define a procedure last-pair that returns the list that contains
; only the last element of a given (nonempty) list:

(define (last-pair items)
  (if (= (length items) 1)
      (car items)
      (last-pair (cdr items))))
