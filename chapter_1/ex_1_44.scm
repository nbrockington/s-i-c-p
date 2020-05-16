; The idea of smoothing a function is an important concept in signal
; processing. If f is a function and dx is some small number, then the
; smoothed version of f is the function whose value at a point x is
; the average of f(x - dx), f(x), and f(x + dx). Write a procedure
; smooth that takes as input a procedure that computes f and returns a
; procedure that computes the smoothed f. It is sometimes valuable to
; repeatedly smooth a function (that is, smooth the smoothed function,
; and so on) to obtained the n-fold smoothed function. Show how to
; generate the n-fold smoothed function of any given function using
; smooth and repeated from exercise 1.43.


; Defining (smooth f) which smooths a function f:

(define dx 0.00001)

(define (average-of-three a b c)
   (/ (+ a b c) 3.0))

(define (smooth f)
   (lambda (x) (average-of-three (f (- x dx))
                                 (f x)
                                 (f (+ x dx)))))


; Procedure (compose f g) from Exercise 1.42:

(define (compose f g)
   (lambda (x) (f (g x))))


; Procedure (repeated f n) from Exercise 1.43:

(define (repeated f n)
   (define (repeated-iter f rep-f i)
      (if (= i n)
          rep-f
          (repeated-iter f (compose f rep-f) (+ 1 i))))
   (repeated-iter f f 1))


; Defining (n-fold-smooth f n) which smooths a function f n-fold
; times:

(define (n-fold-smooth f n)
   ((repeated smooth n) f))
