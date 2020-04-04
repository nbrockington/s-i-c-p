; A function f is defined by the rule that f(n) = n if n < 3 and f(n)
; = f(n - 1) + 2f(n - 2) + 3f(n - 3) if n > 3. Write a procedure that
; computes f by means of a recursive process. Write a procedure that
; computes f by means of an iterative process.


; Recursive f

(define (frec n)
   (cond ((< n 3) n)
         (else (+ (frec (- n 1)) (* 2 (frec (- n 2))) (* 3 (frec (- n 3)))))))


; Iterative f

(define (fiter n)
   (if (< n 3)
       n
       (iter 2 1 0 n)))


(define (iter a b c count)
   (if (< count 3)
       a
       (iter (+ a (* 2 b) (* 3 c)) a b (- count 1))))

      






 
