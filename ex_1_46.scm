; Several of the numerical methods described in this chapter are
; instances of an extremely general computational strategy known as
; iterative improvement. Iterative improvement says that, to compute
; something, we start with an initial guess for the answer, test if
; the guess is good enough, and otherwise improve the guess and
; continue the process using the improved guess as the new
; guess. Write a procedure iterative-improve that takes two procedures
; as arguments: a method for telling whether a guess is good enough
; and a method for improving a guess. Iterative-improve should return
; as its value a procedure that takes a guess as argument and keeps
; improving the guess until it is good enough. Rewrite the sqrt
; procedure of section 1.1.7 and the fixed-point procedure of section
; 1.3.3 in terms of iterative-improve.

; Defining (iterative-improve):

(define (iterative-improve good-enough? improve)
   (lambda (x)
      (define (iter guess)
         (newline)
         (display guess)
         (if (good-enough? guess)
             guess
             (iter (improve guess))))
   (iter x)))


; Trouble-shooting

(define (equals-4? guess) (= guess 4))

(define (inc guess) (+ 1 guess))

((iterative-improve equals-4? inc) 0)


; Rewriting the (sqrt) procedure from Section 1.1.7 in terms of
; (iterative-improve):

(define (sqrt-ii x)
   (define (close-enough? guess)
      (< (abs (- (square guess) x)) 0.00001))
   (define (improve-sqrt guess)
      (/ (+ guess (/ x guess)) 2.0))
   ((iterative-improve close-enough? improve-sqrt) 1.0))

; Test:

(sqrt-ii 81)

; Answer: 9.000000000007091 (CORRECT)


; Rewriting the (fixed-point) procedure from Section 1.3.3 in terms of
; (iterative-improve):

(define (fixed-point-ii f guess)
   (define (close-enough? guess)
      (< (abs (- guess (f guess))) 0.00001))
   (define (improve-fp guess)
      (f guess))
   ((iterative-improve close-enough? improve-fp) guess))

; Test 1, fixed point of cos:

(fixed-point-ii cos 1.0)

;Value: .7390893414033928 (CORRECT)

; Test 2, fixed point of y = sin(y) + cos(y):

(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)

;Value: 1.2587315962971173 (CORRECT)



