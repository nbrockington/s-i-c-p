; In 1737, the Swiss mathematician Leonhard Euler published a memoir
; De Fractionibus Continuis, which included a continued fraction
; expansion for e - 2, where e is the base of the natural
; logarithms. In this fraction, the N_i are all 1, and the D_i are
; successively 1, 2, 1, 1, 4, 1, 1, 6, 1, 1, 8, .... Write a program
; that uses your (cont-frac) procedure from exercise 1.37 to approximate
; e, based on Euler's expansion.


; Using the iterative version of (cont-frac) from Exercise 1.37:

(define (cont-frac-iter n d k)
   (define (iter frac i)
      (cond ((= i 0) frac)
            (else (iter (/ (n i) (+ (d i) frac)) (- i 1)))))
   (iter 0 k))


; Defining N_i:

(define (n-seq i) 1)


; Defining D_i:

(define (d-seq i) 
   (if (= 2 (remainder i 3))
       (* 2.0 (/ (+ i 1) 3))
       1.0))


; Estimating constant e based on Euler's expansion:

(define (approx-e k)
   (+ 2 (cont-frac-iter n-seq d-seq k)))


