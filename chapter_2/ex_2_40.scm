; Define a procedure unique-pairs that, given an integer n, generates
; the sequence of pairs (i,j) with 1<= j< i<= n. Use unique-pairs to
; simplify the definition of prime-sum-pairs given above.

; Defining new procedure "unique-pairs":

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (unique-pairs n)
  (define (enumerate-interval low high)
    (if (> low high) 
	'()
	(cons low (enumerate-interval (+ low 1) high))))
  (define (flatmap proc seq)
    (accumulate append '() (map proc seq)))
  (flatmap (lambda (i)
	     (map (lambda (j) (list i j))
		  (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))


; Defining "prime-sum-pairs" using "unique-pairs":

(define (prime-sum-pairs n)
  (map (lambda (x) (list (car x) (cadr x) (+ (car x) (cadr x))))
       (filter (lambda (x) (prime? (+ (car x) (cadr x))))
	       (unique-pairs n))))


; Testing for primality in O(sqrt(n)) time:

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
	((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

