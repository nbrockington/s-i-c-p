; Implement Monte Carlo integration as a procedure estimate-integral
; that takes as arguments a predicate P, upper and lower bounds x1,
; x2, y1, and y2 for the rectangle, and the number of trials to
; perform in order to produce the estimate. Your procedure should use
; the same monte-carlo procedure that was used above to estimate
; pi. Use your estimate-integral to produce an estimate of by
; measuring the area of a unit circle.

; You will find it useful to have a procedure that returns a number
; chosen at random from a given range. The following random-in-range
; procedure implements this in terms of the random procedure used in
; section 1.2.6, which returns a nonnegative number less than its
; input:

; Provided procedure random-in-range:
(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (random range))))

; Provided procedure monte-carlo:
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))



; estimate-integral:
(define (estimate-integral P x1 x2 y1 y2 trials)
  ( * (monte-carlo trials P) (* (- x1 x2) (- y1 y2) )))


; predicate to test whether a randomly chosen coordinates within the
; unit square lies within the unit circle:
(define (unit-circle-p) 
  (let ((x (random-in-range -1.0 1.0)) 
        (y (random-in-range -1.0 1.0)))
    (< (+ (expt x 2) (expt y 2)) 1)))


; Estimation of pi: 
(estimate-integral unit-circle-p 1.0 -1.0 1.0 -1.0 100000)

;Value: 3.14156

