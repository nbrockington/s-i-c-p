; We specified that a set would be represented as a list with no
; duplicates. Now suppose we allow duplicates. For instance, the set
; {1,2,3} could be represented as the list (2 3 2 1 3 2 2). Design
; procedures element-of-set?, adjoin-set, union-set, and
; intersection-set that operate on this representation. How does the
; efficiency of each compare with the corresponding procedure for the
; non-duplicate representation? Are there applications for which you
; would use this representation in preference to the non-duplicate
; one?


; New "element-of-set?" -- same as before and running in O(n) time,
; where n is length of set S, but note bene that the length of the set
; is potentially much longer than in the previous representation as
; dulplicate elements are allowed:

(define (element-of-set? x set)
  (cond ((null? set) #f)
	((equal? x (car set)) #t)
	(else (element-of-set? x (cdr set)))))


; New "adjoin-set" -- since duplicates are allowed, we don't need to
; check whether x is already in set S, so we can just cons x straight
; on to S, so this procedure now runs in constant time O(1), instead
; of O(n):

(define (adjoin-set x set) 
  (cons x set))


; New "union-set" -- again, since duplicates are allowed, we don't
; need to check whether any element of set1 is already in set2, so we
; can just cons all elements of set1 onto set 2, running in O(n) time
; where n is length of set 1, instead of O(n^2) time:

(define (union-set set1 set2)
  (if (null? set1)
      set2
      (union-set (cdr set1) (cons (car set1) set2))))


; New "intersection-set" -- this is exactly the same procedure as
; before and runs in O(n^2) time. Given the potential increase in
; length n of the set lists since duplicates are allowed, this could
; result in much longer run times.:

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
	((element-of-set? (car set1) set2)
	 (cons (car set1) (intersection-set (cdr set1) set2)))
	(else (intersection-set (cdr set1) set2))))


; Overall, "adjoin-set" and "union-set" have decreased time complexity
; while "element-of-set?" and "intersection-set" have the same time
; complexity but with potentially longer lists that could result in
; increased run times. Therefore, I would only use this representation
; in preference to the non-duplicate one for applications where the
; set constructions "adjoin-set" and "union-set" are more frequently
; used and "element-of-set?" and "intersection-set" are less
; frequently used.