; a) a. An infinite continued fraction is an expression of the form
;
; f = (N_1 / (D_1 + (N_2 / (D_2 + (N_3 / (D_3 + ...
;
; As an example, one can show that the infinite continued fraction
; expansion with the N_i and the D_i all equal to 1 produces 1/(phi),
; where (phi)is the golden ratio (described in section 1.2.2). One way
; to approximate an infinite continued fraction is to truncate the
; expansion after a given number of terms. Such a truncation -- a
; so-called k-term finite continued fraction -- has the form
;
; f = (N_1 / (D_1 + (N_2 / ... + (N_k / D_k)...)
;
; Suppose that (n) and (d) are procedures of one argument (the term
; index i) that return the N_i and D_i of the terms of the continued
; fraction. Define a procedure (cont-frac) such that evaluating
; (cont-frac n d k) computes the value of the k-term finite continued
; fraction. Check your procedure by approximating 1/ using
;
; (cont-frac (lambda (i) 1.0)
;           (lambda (i) 1.0)
;           k)
;
;for successive values of k. How large must you make k in order to get
;an approximation that is accurate to 4 decimal places?


; Recursive definition of (cont-frac):

(define (cont-frac n d k)
   (define (cont-frac-rec i)
      (cond ((> i k) 0)
            (else (/ (n i) (+ (d i) (cont-frac-rec (+ i 1)))))))
   (cont-frac-rec 1))


; Procedure to calculate smallest value of k required to approximate
; (phi) to 4 decimal places with (cont-frac) as above:

(define (approx-phi k)
   (newline)
   (display k)
   (let ((est-phi (/ 1 (cont-frac (lambda (i) 1.0) (lambda (i) 1.0) k))))
      (if (< (abs (- est-phi 1.6180)) 0.00005)
          est-phi
          (approx-phi (+ k 1)))))

; Answer: k = 11 (Value: 1.6179775280898876 rounds to 1.6180 4 d.p.)


; b. If your cont-frac procedure generates a recursive process, write
; one that generates an iterative process. If it generates an
; iterative process, write one that generates a recursive process.

; Iterative definition of (cont-frac) NB. The iterative version works
; by starting from the innermost N_k/D_k fraction and then building
; the rest of the continued fraction around it:

(define (cont-frac-iter n d k)
   (define (iter frac i)
      (cond ((= i 0) frac)
            (else (iter (/ (n i) (+ (d i) frac)) (- i 1)))))
   (iter 0 k))



