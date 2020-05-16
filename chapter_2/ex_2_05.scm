; Show that we can represent pairs of nonnegative integers using only
; numbers and arithmetic operations if we represent the pair a and b
; as the integer that is the product (2^a)(3^b). Give the
; corresponding definitions of the procedures cons, car, and cdr.

; By the Fundamental Theorem of Arithmetic, every positive integer can
; be represented as a product of prime numbers and this representation
; is unique (up to order of factors). Since 2 and 3 are prime numbers,
; there is a unique representation of the product (2^a)(3^b) and so
; (a) and (b) can be unambiguously recovered by prime factorisation.


(define (new-cons x y)
   (* (expt 2 x)
      (expt 3 y)))

(define (new-car z)
   (find-exponent z 2 0))

(define (new-cdr z)
   (find-exponent z 3 0))


(define (find-exponent z d count)
   (if (> (remainder z d) 0)
       count
       (find-exponent (/ z d) d (+ 1 count))))


