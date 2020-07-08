; Write a procedure to find all ordered triples of distinct positive
; integers i, j, and k less than or equal to a given integer n that
; sum to a given integer s.

; Define "unique-triplets":

(define (enumerate-interval low high)
  (if (> low high) 
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (unique-pairs n)
  (flatmap (lambda (i)
	     (map (lambda (j) (list i j))
		  (enumerate-interval 1 (- i 1))))
	   (enumerate-interval 1 n)))

(define (triplets-for-n n)
  (map (lambda (x) (cons n x))
       (unique-pairs (- n 1))))

(define (unique-triplets n)
  (flatmap triplets-for-n
	   (enumerate-interval 1 n)))

; Define "s-sum-triplets":

(define (s-sum-triplets s n)
  (define (triplet-sum-to-s? x)
    (= s (+ (car x) (cadr x) (caddr x))))
  (filter triplet-sum-to-s?
	  (unique-triplets n)))


			  
