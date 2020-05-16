; Simpson's Rule is a more accurate method of numerical integration
; than the method illustrated above. Using Simpson's Rule, the
; integral of a function f between a and b is approximated as
;
; (h/3)*[y_0 + 4*y_1 + 2*y_2 + 4*y_3+...+2*y_(n-2) + 4*y_(n-1) + y_n]
;
; where h = (b - a)/n, for some even integer n, and y_k = f(a +
; kh). (Increasing n increases the accuracy of the approximation.)
; Define a procedure that takes as arguments f, a, b, and n and
; returns the value of the integral, computed using Simpson's
; Rule. Use your procedure to integrate cube between 0 and 1 (with n =
; 100 and n = 1000), and compare the results to those of the integral
; procedure shown above.


(define (simpson-integral f a b n)  
   (define h (/ (- b a) n))
   (define l-b a)
   (define (inc a) (+ a h))
   (define (scaled-y x)
      (define (calc-scale k)
         (cond ((or (= k 0) (= k n)) (f x))
               ((not (even? k)) (* 4.0 (f x)))
               (else (* 2.0 (f x)))))
      (calc-scale (/ (- x l-b) h)))
   (if (even? n)
       (* (sum scaled-y a inc b) (/ h 3.0))
       0))


; Procedures (sum) and (even?) provided in book:

(define (sum term a next b)
   (if (> a b) 
       0
       (+ (term a) 
          (sum term (next a) next b))))


(define (even? n)
   (= (remainder n 2) 0))



