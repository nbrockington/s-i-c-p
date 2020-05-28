; Fill in the missing expressions to complete the following
; definitions of some basic list-manipulation operations as
; accumulations:

(define nil '())

(define (map-2 p sequence)
  (accumulate (lambda (x y)(cons (p x) y)) nil sequence))

(define (append-2 seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length-2 sequence)
  (accumulate (lambda (x y) (+ 1 y)) 0 sequence))

; The provided procedure (accumulate):

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
