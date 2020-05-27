; A binary mobile consists of two branches, a left branch and a right
; branch. Each branch is a rod of a certain length, from which hangs
; either a weight or another binary mobile. We can represent a binary
; mobile using compound data by constructing it from two branches (for
; example, using list):

(define (make-mobile left right)
  (list left right))

; A branch is constructed from a length (which must be a number)
; together with a structure, which may be either a number
; (representing a simple weight) or another mobile:

(define (make-branch length structure)
  (list length structure))

; a.  Write the corresponding selectors left-branch and right-branch,
; which return the branches of a mobile, and branch-length and
; branch-structure, which return the components of a branch.

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cadr branch))

; b.  Using your selectors, define a procedure total-weight that
; returns the total weight of a mobile.

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (branch-weight b)
  (if (not (pair? (branch-structure b)))
      (branch-structure b)
      (total-weight (branch-structure b))))


; c.  A mobile is said to be balanced if the torque applied by its
; top-left branch is equal to that applied by its top-right branch
; (that is, if the length of the left rod multiplied by the weight
; hanging from that rod is equal to the corresponding product for the
; right side) and if each of the submobiles hanging off its branches
; is balanced. Design a predicate that tests whether a binary mobile
; is balanced.

(define (is-balanced? x)
  (define (torque b)
    (* (branch-length b) (branch-weight b)))
  (if (not (pair? x))
      #t
      (and (eq? (torque (left-branch x)) (torque (right-branch x)))
           (is-balanced? (branch-structure (left-branch x)))
           (is-balanced? (branch-structure (right-branch x))))))
       

; d.  Suppose we change the representation of mobiles so that the
; constructors are

(define (make-mobile left right)
  (cons left right))
(define (make-branch length structure)
  (cons length structure))

; How much do you need to change your programs to convert to the new
; representation?

; Only the selectors right-branch and branch-structure need to be
; changed to use cdr instead of cadr; all other procedures will work
; correctly.

(define (right-branch mobile) 
  (cdr mobile))

(define (branch-structure branch)
  (cdr branch))


(define m1 (make-mobile (make-branch 2 6) (make-branch 3 4)))

(define m2 (make-mobile (make-branch 1 5) (make-branch 5 1)))

(define m3 (make-mobile (make-branch 6 m1) (make-branch 10 m2)))

; 1 ]=> (total-weight m3)
; Value: 16

; 1 ]=> (is-balanced? m3)
; Value: #t
