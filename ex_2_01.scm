; Define a better version of make-rat that handles both positive and
; negative arguments. Make-rat should normalize the sign so that if
; the rational number is positive, both the numerator and denominator
; are positive, and if the rational number is negative, only the
; numerator is negative.


(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


(define (make-rat n d)
   (let ((g (gcd n d)))
      (let ((a (/ n g))
            (b (/ d g)))
         (if (is-neg? b)
             (cons (- a) (- b))
             (cons a b)))))


(define (is-neg? x) (< x 0))