; Suppose we represent vectors v = (v_i) as sequences of numbers, and
; matrices m = (m_i,j) as sequences of vectors (the rows of the
; matrix). For example, the matrix
;  
; [1 2 3 4]
; |4 5 6 6|
; [6 7 8 9]
; 
; is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)). With
; this representation, we can use sequence operations to concisely
; express the basic matrix and vector operations.
; 
; We can define the dot product as:

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

; Fill in the missing expressions in the following procedures for
; computing the other matrix operations.

(define (matrix-*-vector m v)
  (map (lambda (r) (dot-product v r)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (r) (matrix-*-vector cols r)) m)))


; Procedure (accumulate-n) from Exercise 2.36:

(define nil '())

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))