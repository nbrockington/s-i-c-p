; Give an implementation of adjoin-set using the ordered
; representation. By analogy with element-of-set? show how to take
; advantage of the ordering to produce a procedure that requires on
; the average about half as many steps as with the unordered
; representation.


(define (adjoin-set x set)
  (cond ((null? set) (cons x set))
	((= x (car set)) set)
	((< x (car set)) (cons x set))
	(else (cons (car set) (adjoin-set x (cdr set))))))

; In the above procedure, the item x is compared with ascending
; elements in the set while it is greater than the first element of
; the set. If the end of the set is encountered, or the next set
; element is greater than x, then we know that x is not in the set so
; x is consed onto the set and the procedure ends. As for
; "element-of-set?" below, on average we would expect to have to
; examine about half of the elements of the set before an element
; greater than x is encountered; hence, the procedure runs in O(n/2)
; time, that is, half as many steps as the unordered representation.
; (Surely this would be even faster with binary search: O(log n)?)




; Given representations of sets as ordered lists: 

(define (element-of-set? x set)
  (cond ((null? set) false)
	((= x (car set)) true)
	((< x (car set)) false)
	(else (element-of-set? x (cdr set)))))

(define (intersection-set set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
	(cond ((= x1 x2)
	       (cons x1
		     (intersection-set (cdr set1)
				       (cdr set2))))
	      ((< x1 x2)
	       (intersection-set (cdr set1) set2))
	      ((< x2 x1)
	       (intersection-set set1 (cdr set2)))))))

