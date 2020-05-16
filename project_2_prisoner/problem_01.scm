;; note that you will need to write extract-entry                                   

(define (extract-entry game association-list)
   (cond ((null? association-list)
            (display "Error: This game play not found in list."))
         ((equal? game (car (car association-list)))
            (car association-list))
         (else (extract-entry game (cdr association-list)))))
