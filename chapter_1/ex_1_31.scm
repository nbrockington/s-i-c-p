; a.  The sum procedure is only the simplest of a vast number of
; similar abstractions that can be captured as higher-order
; procedures. Write an analogous procedure called product that
; returns the product of the values of a function at points over a
; given range. Show how to define factorial in terms of product. Also
; use product to compute approximations to pi using the formula
; 
; pi/4 = (2 * 4 * 4 * 6 * 6 * 8 ...)/(3 * 3 * 5 * 5 * 7 * 7 * ...)
; 


; Linear recursive (product) procedure:

(define (product term a next b)
   (if (> a b)
       1
       (* (term a)
          (product term (next a) next b))))


(define (identity x) x)
(define (inc x) (+ x 1))


(define (factorial n) 
   (product identity 1 inc n))


; Approximation to pi by (a la John Wallis):

(define (inc-by-2 x) (+ x 2))

(define (approx-pi n)
   (* 4.0
      (/ (* 2 
            (product square 4 inc-by-2 (* 2 n))
            (* 2 (+ n 1)))
         (product square 3 inc-by-2 (+ 1 (* n 2))))))



; b. If your product procedure generates a recursive process, write
; one that generates an iterative process. If it generates an
; iterative process, write one that generates a recursive process.


(define (producti term a next b)
   (define (product-iter a result)
      (if (> a b)
          result
          (product-iter (next a) (* (term a) result))))
   (product-iter a 1))








