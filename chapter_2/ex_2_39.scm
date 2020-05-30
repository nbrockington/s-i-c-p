; Complete the following definitions of reverse (exercise 2.18) in
; terms of fold-right and fold-left from exercise 2.38:

(define nil '())

(define (reverse-fr sequence)
  (fold-right (lambda (x y) (append y (list x))) nil sequence))

(define (reverse-fl sequence)
  (fold-left (lambda (x y) (append (list y) x)) nil sequence))


