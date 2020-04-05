; Most Lisp implementations include a primitive called runtime that
; returns an integer that specifies the amount of time the system has
; been running (measured, for example, in microseconds). The following
; timed-prime-test procedure, when called with an integer n, prints n
; and checks to see if n is prime. If n is prime, the procedure prints
; three asterisks followed by the amount of time used in performing
; the test.

; Using this procedure, write a procedure search-for-primes that
; checks the primality of consecutive odd integers in a specified
; range. Use your procedure to find the three smallest primes larger
; than 1000; larger than 10,000; larger than 100,000; larger than
; 1,000,000. Note the time needed to test each prime. Since the
; testing algorithm has order of growth of (n), you should expect that
; testing for primes around 10,000 should take about 10 times as long
; as testing for primes around 1000. Do your timing data bear this
; out? How well do the data for 100,000 and 1,000,000 support the n
; prediction? Is your result compatible with the notion that programs
; on your machine run in time proportional to the number of steps
; required for the computation?

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))


; Search-for-primes procedure: where operands l and n represent the                              
; lower bound and number of primes to be found, respectively (i.e.,                            
; for this questions, n should be 3).

(define (search-for-primes l n)
   (cond ((= n 0) '())
         ((prime? l) (sub-sfp l n))
         (else (search-for-primes (+ l 1) n))))

(define (sub-sfp l n)
   (timed-prime-test l)
   (search-for-primes (+ l 1) (- n 1)))

