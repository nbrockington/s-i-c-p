; Two lists are said to be equal? if they contain equal elements
; arranged in the same order. For example,
; 
; (equal? '(this is a list) '(this is a list))
; 
; is true, but
; 
; (equal? '(this is a list) '(this (is a) list))
; 
; is false. To be more precise, we can define equal? recursively in
; terms of the basic eq? equality of symbols by saying that a and b
; are equal? if they are both symbols and the symbols are eq?, or if
; they are both lists such that (car a) is equal? to (car b) and (cdr
; a) is equal? to (cdr b). Using this idea, implement equal? as a
; procedure.


; Procedure "my-equal?" :

(define (my-equal? x y)
  (cond ((and (null? x) (null? y)) #t)
	((and (not (list? x)) (not (list? y)))
	 (eq? x y))
	((and (list? x) (list? y))
	 (and (my-equal? (car x) (car y))
	      (my-equal? (cdr x) (cdr y))))
	(else #f)))