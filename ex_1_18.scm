; Using the results of exercises 1.16 and 1.17, devise a procedure
; that generates an iterative process for multiplying two integers in
; terms of adding, doubling, and halving and uses a logarithmic number
; of steps.

(define (multi a b)
   (define (mult-iter a b c)
      (cond ((= b 0) c)
            ((even? b) (mult-iter (double a) (/ b 2) c))
            (else (mult-iter a (- b 1) (+ c a)))))
   (mult-iter a b 0))