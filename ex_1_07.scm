; An alternative strategy for implementing good-enough? is to watch
; how guess changes from one iteration to the next and to stop when
; the change is a very small fraction of the guess. Design a
; square-root procedure that uses this kind of end test. Does this
; work better for small and large numbers?



(define (sqrt-iter new-guess old-guess x)
   (if (good-enough? new-guess old-guess)
       new-guess
       (sqrt-iter (improve new-guess x) new-guess x)))


(define (improve guess x)
   (average guess (/ x guess)))


(define (average x y)
   (/ (+ x y) 2))


(define (good-enough? new-guess old-guess)
   (< (/ (abs (- old-guess new-guess)) old-guess) 0.001))


(define (sqrti x)
   (sqrt-iter 1 1.1 x))
