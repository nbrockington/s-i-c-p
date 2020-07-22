; The following procedure takes as its argument a list of
; symbol-frequency pairs (where no symbol appears in more than one
; pair) and generates a Huffman encoding tree according to the Huffman
; algorithm.

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

; Make-leaf-set is the procedure given above that transforms the list
; of pairs into an ordered set of leaves. Successive-merge is the
; procedure you must write, using make-code-tree to successively merge
; the smallest-weight elements of the set until there is only one
; element left, which is the desired Huffman tree. (This procedure is
; slightly tricky, but not really complicated. If you find yourself
; designing a complex procedure, then you are almost certainly doing
; something wrong. You can take significant advantage of the fact that
; we are using an ordered set representation.)

; ANS:

(define (successive-merge list-of-trees)
  (cond ((= 1 (length list-of-trees)) (car list-of-trees))
	(else (successive-merge
	       (adjoin-set (make-code-tree (car list-of-trees)
					   (cadr list-of-trees))
			   (cddr list-of-trees))))))

; TESTING:

;1 ]=> (generate-huffman-tree '((A 4) (B 3) (C 1) (D 1)))

;Value: ((leaf a 4) (((leaf d 1) (leaf c 1) (d c) 2) (leaf b 3) (d c
;b) 5) (a d c b) 9)

;1 ]=> (generate-huffman-tree '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1)))

;Value: ((leaf a 8) ((((leaf h 1) (leaf g 1) (h g) 2) ((leaf f 1)
;(leaf e 1) (f e) 2) (h g f e) 4) (((leaf d 1) (leaf c 1) (d c) 2)
;(leaf b 3) (d c b) 5) (h g f e d c b) 9) (a h g f e d c b) 17)

; CORRECT - Examples from book are replicate up to number of bits used
; to encode each letter based on frequency.


; Procedures for Huffman encoding trees, from book:

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
	right
	(append (symbols left) (symbols right))
	(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
	'()
	(let ((next-branch
	       (choose-branch (car bits) current-branch)))
	  (if (leaf? next-branch)
	      (cons (symbol-leaf next-branch)
		    (decode-1 (cdr bits) tree))
	      (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
	((= bit 1) (right-branch branch))
	(else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
	((< (weight x) (weight (car set))) (cons x set))
	(else (cons (car set)
		    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
	(adjoin-set (make-leaf (car pair)    ; symbol
			       (cadr pair))  ; frequency
		    (make-leaf-set (cdr pairs))))))

