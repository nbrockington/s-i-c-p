; We can represent a set as a list of distinct elements, and we can
; represent the set of all subsets of the set as a list of lists. For
; example, if the set is (1 2 3), then the set of all subsets is (()
; (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)). Complete the following
; definition of a procedure that generates the set of subsets of a set
; and give a clear explanation of why it works:

(define nil '())

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest 
                (map (lambda (elem) (cons (car s) elem))
                     rest)))))

; ANS. This procedure works because it recursively constructs two
; copies of the list of subsets of the cdr of a set, and attaches the
; car element onto each subset of the second of these lists, and then
; appends the lists together. In the case of the set containing only
; one element x, this procedure creates two copies of the list of
; subsets of the cdr of the set; the cdr of the set in this case is
; "nil" and hence the only item in each list of susbsets is '(). The
; procedure then attaches x onto each element of one of the lists
; (creating '(x)) and appends the two lists together, creating '(()
; (x)). One can imagine that in the case of a set of two elements, (x
; y), the "rest" in the first call of subsets will be '(() (y))
; according to the explanation of the one-element set above. Then, x
; will be attached onto each element of the second of the two copies
; of this "rest", the copies then being appending together, making
; '(() (y) (x) (xy)). Hence, for any given set, all possible subsets,
; those containing the car element and those not containing the car
; element, are recursively constructed.