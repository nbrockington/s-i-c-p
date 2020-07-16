; Give an O(n) implementation of union-set for sets represented as
; ordered lists.

(define (union-set set1 set2) 
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else (let ((x1 (car set1)) (x2 (car set2)))
		(cond ((= x1 x2)
		       (cons x1 (union-set (cdr set1)
					   (cdr set2))))
		      ((< x1 x2)
		       (cons x1 (union-set (cdr set1) set2)))
		      (else (cons x2 (union-set set1 (cdr set2)))))))))

; As for the provided procedure "intersection-set", in each step of
; the "union-set" procedure above, the problem is reduced to computing
; the union of smaller sets: either one or both of set1 and set2 are
; reduced in length by one. Hence, the largest number of steps is the
; sum of the cardinalities of the sets, so the procedure runs in O(n)
; time.


