; Exercise 1.3.  Define a procedure that takes three numbers as arguments 
; and returns the sum of the squares of the two larger numbers.

(define (maxsq x y z)
   (cond ((and (< z x) (< z y)) (+ (* x x) (* y y)) )
         ((and (< y x) (< y z)) (+ (* x x) (* z z)) )
         (else (+ (* y y) (* z z)))))
