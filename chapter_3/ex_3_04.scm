; Modify the make-account procedure of exercise 3.3 by adding another
; local state variable so that, if an account is accessed more than
; seven consecutive times with an incorrect password, it invokes the
; procedure call-the-cops.


(define (make-account balance password)
  (let ((count 0))
    (define (withdraw amount)
      (if (>= balance amount)
	  (begin (set! balance (- balance amount))
		 balance)
	  "Insufficient funds"))
    (define (deposit amount)
      (set! balance (+ balance amount))
      balance)
    (define (incorrect-password amount)
      "Incorrect password")
    (define (call-the-cops amount)
      "The cops are on their way")
    (define (dispatch p m)
      (if (eq? p password)
	  (begin (set! count 0)
		 (cond ((eq? m 'withdraw) withdraw)
		       ((eq? m 'deposit) deposit)
		       (else (error "Unknown request -- MAKE-ACCOUNT"
				    m))))
	  (begin (set! count (+ count 1))
		 (if (> count 7)
		     call-the-cops
		     incorrect-password))))
    dispatch))