; Show that the golden ratio (phi) (section 1.2.2) is a fixed point of
; the transformation x -> 1 + 1/x, and use this fact to compute (phi)
; by means of the fixed-point procedure.

; (Phi) is the solution to x^2 = x + 1. After division by x, this
; equation is transfored to x = 1 + 1/x, and (phi) is still a
; solution, as required. Hence, this equation can be used for
; fixed-point iteration to estimate the value of (phi).


; Code provided in the textbook:

(define tolerance 0.00000001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


(define (close-enough? x y)
  (< (abs (- x y)) 0.001))


(define (average x y)
   (/ (+ x y) 2))


; To find (phi):

(fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.1)

;Value: 1.6180364726455159
