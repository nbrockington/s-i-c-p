;   The following pattern of numbers is called Pascal's triangle. The
;   numbers at the edge of the triangle are all 1, and each number
;   inside the triangle is the sum of the two numbers above it. Write
;   a procedure that computes elements of Pascal's triangle by means
;   of a recursive process.

; NB. In this procedure, inputs n and r are taken to refer to the
; binomial coefficeint "nCr" where 0 ≤ r ≤ n.


(define (pascal n r)
   (cond ((= r 0) 1)
         ((= r n) 1)
         (else (+ (pascal (- n 1) (- r 1)) (pascal (- n 1) r)))))




