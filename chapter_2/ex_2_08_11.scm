; Solutions to Exercises 2.8 - 2.16: Extended Project 

; Programming questions only

; (Exercise 2.8)

; Using reasoning analogous to Alyssa's, describe how the difference
; of two intervals may be computed. Define a corresponding subtraction
; procedure, called sub-interval.

; The difference of two intervals x and y, i.e. the interval of x - y,
; will have a lower bound of LB(x) - UB(Y) and upper bound of UB(x) -
; LB(y).
;
; Defining procedure (sub-interval) accordingtly:

(define (sub-interval x y)
   (make-interval (- (lower-bound x) (upper-bound y))
                  (- (upper-bound x) (lower-bound y))))


; Constructor and selectors from Exercise 2.7

(define (make-interval a b) (cons a b))

(define (lower-bound interval) (car interval))

(define (upper-bound interval) (cdr interval))


; (Exercise 2.10)

; Ben Bitdiddle, an expert systems programmer, looks over Alyssa's
; shoulder and comments that it is not clear what it means to divide
; by an interval that spans zero. Modify Alyssa's code to check for
; this condition and to signal an error if it occurs.

; Procedure (mul-interval) from book:

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

; Amended (div-interval):

(define (div-interval x y)
   (if (spans-zero y)
       (display "Error: Divisor spans zero")
       (mul-interval x 
                     (make-interval (/ 1.0 (upper-bound y))
                                    (/ 1.0 (lower-bound y))))))

(define (spans-zero y)
   (< (* (upper-bound y) (lower-bound y)) 0))


; (Exercise 2.11) 

; In passing, Ben also cryptically comments: ``By testing the signs of
; the endpoints of the intervals, it is possible to break mul-interval
; into nine cases, only one of which requires more than two
; multiplications.'' Rewrite this procedure using Ben's suggestion.

; New procedure (mul-interval):

(define (mul-interval a b)
   (cond ((> (lower-bound a) 0)
          (cond ((> (lower-bound b) 0) (make-interval
                                          (* (lower-bound a) (lower-bound b))
                                          (* (upper-bound a) (upper-bound b))))
                ((< (upper-bound b) 0) (make-interval
                                          (* (upper-bound a) (lower-bound b))
                                          (* (lower-bound a) (upper-bound b))))
                (else (make-interval 
                         (* (upper-bound a) (lower-bound b))
                         (* (upper-bound a) (upper-bound b))))))
         ((< (upper-bound a) 0)
          (cond ((> (lower-bound b) 0) (make-interval 
                                          (* (lower-bound a) (upper-bound b))
                                          (* (upper-bound a) (lower-bound b))))
                ((< (upper-bound b) 0) (make-interval 
                                          (* (upper-bound a) (upper-bound b))
                                          (* (lower-bound a) (lower-bound b))))
                (else (make-interval 
                         (* (lower-bound a) (upper-bound b))
                         (* (lower-bound a) (lower-bound b))))))
         (else 
          (cond ((> (lower-bound b) 0) (make-interval 
                                          (* (lower-bound a) (upper-bound b))
                                          (* (upper-bound a) (upper-bound b))))
                ((< (upper-bound b) 0) (make-interval
                                          (* (upper-bound a) (lower-bound b))
                                          (* (lower-bound a) (lower-bound b))))
                (else (let ((p1 (* (upper-bound a) (lower-bound b)))
                            (p2 (* (lower-bound a) (upper-bound b)))
                            (p3 (* (upper-bound a) (upper-bound b)))
                            (p4 (* (lower-bound a) (lower-bound b))))
                         (make-interval (min p1 p2) (max p3 p4))))))))

      
                         




