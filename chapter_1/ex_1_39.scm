; A continued fraction representation of the tangent function was
; published in 1770 by the German mathematician J.H. Lambert:
;
; tan x = (x / (1 - (x^2 / (3 - (x^2 / (5 - ...
;
; where x is in radians. Define a procedure (tan-cf x k) that computes
; an approximation to the tangent function based on Lambert's
; formula. k specifies the number of terms to compute, as in exercise
; 1.37.

; Using the iterative version of (cont-frac) from Exercise 1.37:

(define (cont-frac-iter n d k)
   (define (iter frac i)
      (cond ((= i 0) frac)
            (else (iter (/ (n i) (+ (d i) frac)) (- i 1)))))
   (iter 0.0 k))


; Defining (tan-cf):

(define (tan-cf x k)
   (define (n-seq i)
      (if (= i 1)
          x
          (- 0 (* x x))))
   (define (d-seq i)
      (- (* 2 i) 1))
   (cont-frac-iter n-seq d-seq k))





