; Demonstrate that the Carmichael numbers listed in footnote 47 really
; do fool the Fermat test. That is, write a procedure that takes an
; integer n and tests whether an is congruent to a modulo n for every
; a<n, and try your procedure on the given Carmichael numbers.

(define (pass-fermat? n)
   (define (check-fermat n base)
      (cond ((= base n) true)
            ((fermat-test n base) (check-fermat n (+ base 1)))
            (else false)))
   (check-fermat n 1))
          

; Amended (fermat-test) function to accept two arguments:

(define (fermat-test n base)
   (= (expmod base n n) base))


; Provided expmod function:

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m)))) 
