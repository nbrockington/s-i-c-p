; Let f and g be two one-argument functions. The composition f after g
; is defined to be the function x f(g(x)). Define a procedure compose
; that implements composition. For example, if inc is a procedure that
; adds 1 to its argument,
; 
; ((compose square inc) 6)
; 49


; Procedure to compose two input functions f and g:

(define (compose f g)
   (lambda (x) (f (g x))))


; Check: 

(define (inc n) (+ 1 n))

((compose square inc) 6)

;Value: 49