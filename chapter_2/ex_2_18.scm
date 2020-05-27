; Define a procedure reverse that takes a list as argument and returns                
; a list of the same elements in reverse order:                                       

(define	(reverse items)
  (define (reverse-helper from-list to-list)
    (if	(= (length from-list) 0)
      	to-list
      	(reverse-helper (cdr from-list) (cons (car from-list) to-list))))
  (reverse-helper items '()))
  
  
; Alternative procedure:

(define (reverse x)
  (cond ((null? x) x)
        (else (append (reverse (cdr x))
                      (list (car x))))))
