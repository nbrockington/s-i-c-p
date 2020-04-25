; Implement a representation for rectangles in a plane. (Hint: You may
; want to make use of exercise 2.2.) In terms of your constructors and
; selectors, create procedures that compute the perimeter and the area
; of a given rectangle. Now implement a different representation for
; rectangles. Can you design your system with suitable abstraction
; barriers, so that the same perimeter and area procedures will work
; using either representation?


; Procedures to compute perimeter and area of rectange by using the
; (lengths of) its two dimensions. These work with either
; representation of rectangles define below, where (rect-dim-1) and
; (rect-dim2) are defined differently for each representation.

(define (rect-perimeter rect)
   (+ (* 2 (rect-dim-1 rect))
      (* 2 (rect-dim-2 rect))))

(define (rect-area rect)
   (* (rect-dim-1 rect) (rect-dim-2 rect)))


; REPRESENTATION 1: 

; Rectangle as a combination of two line segments * which must be
; oriented at right angles to each other and share a point*. We use
; the line segment representation procedures from Exercise 2.2, copied
; in at the bottom of this page.

; Define constructor (make-rectangle) and selectors (first-segment)
; and (second-segment):

(define (make-rectangle segment1 segment2)
       (cons segment1 segment2))

(define (first-segment rect)
   (car rect))

(define (second-segment rect)
   (cdr rect))


; Define procedures (rect-dim-1) and (rect-dim-2) that extract the
; lengths of the two dimensions of the rectangle:

(define (rect-dim-1 rect)
   (segment-length (first-segment rect)))

(define (rect-dim-2 rect)
   (segment-length (second-segment rect)))

(define (segment-length segment)
   (let ((x1 (x-point (start-segment segment)))
         (y1 (y-point (start-segment segment)))
         (x2 (x-point (end-segment segment)))
         (y2 (y-point (end-segment segment))))
        (sqrt (+ (expt (- x2 x1) 2)
                 (expt (- y2 y1) 2)))))



; REPRESENTATION 2:

; Rectangle ABCD defined by the coordinates of three of its vertices
; A, B and C (after possible relabelling but with vertex order
; maintained).

; Define constructor (make-rectangle) and selectors (first-coord),
; (second-coord) and (third-coord):

(define (make-rectangle coord-1 coord-2 coord-3)
   (cons coord-1 (cons coord-2 coord-3)))

(define (first-coord rect)
   (car rect))

(define (second-coord rect)
   (car (cdr rect)))

(define (third-coord rect)
   (cdr (cdr rect)))

; Define procedures (rect-dim-1) and (rect-dim-2) that extract the
; lengths of the two dimensions of the rectangle, using the segment
; procedures from Exercise 2.2 and the (segment-length) procedure
; defined above::

(define (rect-dim-1 rect)
   (segment-length
      (make-segment (first-coord rect)
                    (second-coord rect))))

(define (rect-dim-2 rect)
   (segment-length
      (make-segment (second-coord rect)
                    (third-coord rect))))


; ******
; Procedures from Exercise 2.2, used in Representation 1:
; Defining constructor (make-segment):

(define (make-segment start-segment end-segment)
   (cons start-segment end-segment))

; Defining selectors (start-segment) and (end-segment):

(define (start-segment segment)
   (car segment))

(define (end-segment segment)
  (cdr segment))

; Defining constructor (make-point):

(define (make-point x-point y-point)
   (cons x-point y-point))

; Defining selectors (x-point) and (y-point):

(define (x-point point)
   (car point))

(define (y-point point)
   (cdr point))