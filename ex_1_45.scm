; We saw in section 1.3.3 that attempting to compute square roots by
; naively finding a fixed point of y -> x/y does not converge, and
; that this can be fixed by average damping. The same method works for
; finding cube roots as fixed points of the average-damped y ->
; x/y2. Unfortunately, the process does not work for fourth roots -- a
; single average damp is not enough to make a fixed-point search for y
; -> x/y3 converge. On the other hand, if we average damp twice (i.e.,
; use the average damp of the average damp of y -> x/y3) the
; fixed-point search does converge. Do some experiments to determine
; how many average damps are required to compute nth roots as a
; fixed-point search based upon repeated average damping of y ->
; x/yn-1. Use this to implement a simple procedure for computing nth
; roots using fixed-point, average-damp, and the repeated procedure of
; exercise 1.43. Assume that any arithmetic operations you need are
; available as primitives.


; Procedure (fixed-point) defined in book, amended to print the guess
; used in each iteration:

(define tolerance 0.00001)

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (newline)
    (display guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))


; Procedure (average-damp f) defined in book:

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (average a b)
   (/ (+ a b) 2.0))


; Defining square roots, cube roots and fourth roots with a single
; average-damp:

(define (sqrt x) 
   (fixed-point (average-damp (lambda (y) (/ x y))) 
                1.0))

(define (cube-root x)
   (fixed-point (average-damp (lambda (y) (/ x (square y))))
                1.0))

(define (fourth-root-x x) ; Which does not work, infinite recursion
   (fixed-point (average-damp (lambda (y) (/ x (* y y y))))
                1.0))

(define (fourth-root x) ; Which does work with 2x average damping
   (fixed-point ((repeated average-damp 2) (lambda (y) (/ x (* y y y))))
                1.0))


; Procedure (repeated f n) from Exercise 1.43:

(define (compose f g)
   (lambda (x) (f (g x))))

(define (repeated f n)
   (define (repeated-iter f rep-f i)
      (if (= i n)
          rep-f
          (repeated-iter f (compose f rep-f) (+ 1 i))))
   (repeated-iter f f 1))


; Procedure (e-nth-root-of-x) to experiment with how many (r) average
; damps are required to compute nth root of x, using the fixed-point
; search based upon repeated average-damping of y -> x/(y^(n-1)):

(define (e-nth-root-of-x n x r)
   (fixed-point ((repeated average-damp r) 
                    (lambda (y) (/ x (expt y (- n 1)))))
                1.0))


; Experiments show that the number of repeats (r) required to
; calculate the "n"-th root is the floor of log_2(n).
;
; Defining pprocedure (nth-root-of-x) accordingly:

(define (nth-root-of-x n x)
   (let ((r (floor-log-2 n)))
      (fixed-point ((repeated average-damp r)
                       (lambda (y) (/ x (expt y (- n 1)))))
                   0.1)))

(define (floor-log-2 a)
   (define (iter a count)
      (if (< a 2)
          count
          (iter (/ a 2) (+ count 1))))
   (iter a 0))

