; Use the results of exercises 2.63 and 2.64 to give (n)
; implementations of union-set and intersection-set for sets
; implemented as (balanced) binary trees.

(define (union-set set1 set2)
  (let ((list1 (tree->list-2 set1))
	(list2 (tree->list-2 set2)))
    (list->tree (union-set-helper list1 list2))))

(define (union-set-helper set1 set2)
  (cond ((null? set1) set2)
	((null? set2) set1)
	(else (let ((x1 (car set1)) (x2 (car set2)))
		(cond ((= x1 x2)
		       (cons x1 (union-set-helper (cdr set1)
						  (cdr set2))))
		      ((< x1 x2)
		       (cons x1 (union-set-helper (cdr set1) set2)))
		      (else (cons x2 (union-set-helper set1 (cdr set2)))))))))


(define (intersection-set set1 set2) 
  (let ((list1 (tree->list-2 set1))
	(list2 (tree->list-2 set2)))
    (list->tree (intersection-set-helper list1 list2))))

; Provided in book:

(define (intersection-set-helper set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
	(cond ((= x1 x2)
	       (cons x1
		     (intersection-set-helper (cdr set1)
					      (cdr set2))))
	      ((< x1 x2)
	       (intersection-set-helper (cdr set1) set2))
	      ((< x2 x1)
	       (intersection-set-helper set1 (cdr set2)))))))

; These union-set and intersection-set procedures have O(n) growth
; because the conversion of the balanced trees to ordered lists is
; O(n), the union/intersection operation for ordered lists are O(n),
; and the conversion of the resultant ordered list to balanced tree is
; also O(n).



; Representation of sets as binary trees:

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
    (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
	((< x (entry set))
	 (element-of-set? x (left-branch set)))
	((> x (entry set))
	 (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
	((= x (entry set)) set)
	((< x (entry set))
	 (make-tree (entry set)
		    (adjoin-set x (left-branch set))
		    (right-branch set)))
	((> x (entry set))
	 (make-tree (entry set)
		    (left-branch set)
		    (adjoin-set x (right-branch set))))))

(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
	      (cons (entry tree)
		    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
	result-list
	(copy-to-list (left-branch tree)
		      (cons (entry tree)
			    (copy-to-list (right-branch tree)
					  result-list)))))
  (copy-to-list tree '()))


; (define a '(7 (3 (1 () ()) (5 () () )) (9 () (11 () () ))))

; (define b '(3 (1 () () ) (7 (5 () () ) (9 () (11 () () )))))

; (define c '(5 (3 (1 () () ) ()) (9 (7 () () ) (11 () () ))))


(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
	(let ((left-result (partial-tree elts left-size)))
	  (let ((left-tree (car left-result))
		(non-left-elts (cdr left-result))
		(right-size (- n (+ left-size 1))))
	    (let ((this-entry (car non-left-elts))
		  (right-result (partial-tree (cdr non-left-elts)
					      right-size)))
	      (let ((right-tree (car right-result))
		    (remaining-elts (cdr right-result)))
		(cons (make-tree this-entry left-tree right-tree)
		      remaining-elts))))))))