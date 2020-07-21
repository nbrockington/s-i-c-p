; Implement the lookup procedure for the case where the set of records
; is structured as a binary tree, ordered by the numerical values of
; the keys.

 (define (lookup given-key set-of-records)
   (if (null? set-of-records) 
	#f
 	(let ((current-key (key (car set-of-records))))
	  (cond ((equal? given-key current-key) 
		 (car set-of-records))
		((< given-key current-key)
		 (lookup given-key (cadr set-of-records)))
		(else (lookup given-key (caddr set-of-records)))))))


(define (key indiv-record) (car indiv-record))


; Testing with example set of records structured as a binary tree:

(define recs '((5 horse) ((3 dog) ((1 cat) () ()) ()) ((9 fish) ((7 bird) () () ) ((11 snail) () () ) )))

; 1 ]=> (lookup 3 recs)
;Value: (3 dog)

; 1 ]=> (lookup 9 recs)
;Value: (9 fish)

; 1 ]=> (lookup 10 recs)
;Value: #f

