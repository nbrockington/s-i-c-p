; You can obtain an even more general version of accumulate (exercise
; 1.32) by introducing the notion of a filter on the terms to be
; combined. That is, combine only those terms derived from values in
; the range that satisfy a specified condition. The resulting
; filtered-accumulate abstraction takes the same arguments as
; accumulate, together with an additional predicate of one argument
; that specifies the filter. Write filtered-accumulate as a
; procedure. Show how to express the following using
; filtered-accumulate:


(define (filtered-accumulate combiner null-value predicate? term a next b)
   (define (filt-acc-iter a result)
      (cond ((> a b) result)
            ((predicate? a) (filt-acc-iter (next a)
                                           (combiner (term a) result)))
            (else (filt-acc-iter (next a) 
                                 result))))
   (filt-acc-iter a null-value))


(define (inc x) (+ x 1))


; a) the sum of the squares of the prime numbers in the interval a to
; b (assuming that you have a prime? predicate already written)

(define (sum-of-squares-of-primes a b)
   (filtered-accumulate + 0 prime? square a inc b))


; b) the product of all the positive integers less than n that are
; relatively prime to n (i.e., all positive integers i < n such that
; GCD(i,n) = 1)

; ANS: First need to create unary predicate (coprime-to-n?) for a
; given number n:

(define (product-coprime-to-n n)
   (define (coprime-to-n? x) (coprime? x n))
   (filtered-accumulate * 1 coprime-to-n? identity 1 inc n))

; where:

(define (coprime? x n)
   (define (gcd a b)
     (if (= b 0)
         a
         (gcd b (remainder a b))))
   (= (gcd x n) 1))


(define (identity x) x)


; Some useful predicates from previous exercises, used here:

(define (even? n)
   (= (remainder n 2) 0))


(define (prime? n)
   (if (= 1 n)
       #f
       (= n (smallest-divisor n))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

(define (next d)
   (if (= d 2)
       3
       (+ d 2)))

(define (divides? a b)
  (= (remainder b a) 0))


