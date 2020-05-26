; Define a procedure last-pair that returns the list that contains
; only the last element of a given (nonempty) list:

(define (last-pair list)
  (if (= (length list) 1)
      (car list)
      (last-pair (cdr list))))
