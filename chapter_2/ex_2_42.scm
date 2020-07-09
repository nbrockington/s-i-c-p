; The ``eight-queens puzzle'' asks how to place eight queens on a
; chessboard so that no queen is in check from any other (i.e., no two
; queens are in the same row, column, or diagonal). One possible
; solution is shown in figure 2.8. One way to solve the puzzle is to
; work across the board, placing a queen in each column. Once we have
; placed k - 1 queens, we must place the kth queen in a position where
; it does not check any of the queens already on the board. We can
; formulate this approach recursively: Assume that we have already
; generated the sequence of all possible ways to place k - 1 queens in
; the first k - 1 columns of the board. For each of these ways,
; generate an extended set of positions by placing a queen in each row
; of the kth column. Now filter these, keeping only the positions for
; which the queen in the kth column is safe with respect to the other
; queens. This produces the sequence of all ways to place k queens in
; the first k columns. By continuing this process, we will produce not
; only one solution, but all solutions to the puzzle.
;
; We implement this solution as a procedure queens, which returns a
; sequence of all solutions to the problem of placing n queens on an
; n× n chessboard. Queens has an internal procedure queen-cols that
; returns the sequence of all ways to place queens in the first k
; columns of the board.
; 
; In this procedure rest-of-queens is a way to place k - 1 queens in
; the first k - 1 columns, and new-row is a proposed row in which to
; place the queen for the kth column. Complete the program by
; implementing the representation for sets of board positions,
; including the procedure adjoin-position, which adjoins a new
; row-column position to a set of positions, and empty-board, which
; represents an empty set of positions. You must also write the
; procedure safe?, which determines for a set of positions, whether
; the queen in the kth column is safe with respect to the
; others. (Note that we need only check whether the new queen is safe
; -- the other queens are already guaranteed safe with respect to each
; other.)

; Provided procedures: 

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence) (accumulate op initial (cdr sequence)))))

(define (flatmap procedure sequence)
  (accumulate append '() (map procedure sequence)))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
	(list empty-board)
	(filter 
	 (lambda (positions) (safe? k positions))
	 (flatmap 
	  (lambda (rest-of-queens) 
	    (map (lambda (new-row)
		   (adjoin-position new-row k rest-of-queens))
		 (enumerate-interval 1 board-size)))
	  (queen-cols (- k 1))))))
  (queen-cols board-size))

; Implementing representation of sets of board positions as a list of
; the different ways to place queens in the first k columns of the
; board. Each way of placing queens is itself a list containing
; tuples, where each tuple (m n) indicates the row (m) and column (n)
; of each queen's position. New positions are consed on.

(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (cons (list new-row k) rest-of-queens))

(define (safe? k positions) 
  (and (not (same-row? k positions)) (not (same-diagonal? k positions))))

(define (get-kth-position k positions)
  (list-ref positions (- (length positions) k )))

(define (positions-without-k k positions)
  (define (remove-kth-helper k-index positions n)
    (cond ((null? positions) positions)
	  ((= k-index n) (remove-kth-helper k-index (cdr positions) (+ 1 n)))
	  (else (cons (car positions) 
		      (remove-kth-helper k-index (cdr positions) (+ 1 n))))))
  (remove-kth-helper (- (length positions) k) positions 0))
    

(define (same-row? k positions)
  (define (same-row-helper row-val positions)
    (cond ((null? positions) False)
	  ((= row-val (caar positions)) True)
	  (else (same-row-helper row-val (cdr positions)))))
  (same-row-helper (car (get-kth-position k positions))
		   (positions-without-k k positions)))

(define (same-diagonal? k positions)
  (define (same-diagonal-helper kth-position other-positions)
    (cond ((null? other-positions) False)
	  ((on-a-diagonal? kth-position (car other-positions)) True)
	  (else (same-diagonal-helper kth-position (cdr other-positions)))))
  (same-diagonal-helper (get-kth-position k positions)
			(positions-without-k k positions)))

(define (on-a-diagonal? pos1 pos2)
  (let ((x1 (car pos1))
	(x2 (car pos2))
	(y1 (cadr pos1))
	(y2 (cadr pos2)))
    (= (abs (- x1 x2)) (abs (- y1 y2)))))







