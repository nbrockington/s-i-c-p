; Newton's method for cube roots is based on the fact that if y is an
; approximation to the cube root of x, then a better approximation is
; given by the value ((x/y^2) + (2y))/3. Use this formula to implement
; a cube-root procedure analogous to the square-root procedure.

; NB. Force argument to be floating point by use of at least one
; decimal place.



(define (cube-iter new-guess old-guess x)
   (if (good-enough? new-guess old-guess)
        new-guess
       (cube-iter (improve new-guess x) new-guess x)))


(define (improve guess x)
   (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))


(define (good-enough? new-guess old-guess)
   (= new-guess old-guess))


(define (good-enough-alt? new-guess old-guess)
   (< (/ (abs (- new-guess old-guess)) old-guess) 0.001)) 


(define (cbrt x)
   (cube-iter 1 1.1 x))




