; Suppose we want to modify the differentiation program so that it
; works with ordinary mathematical notation, in which + and * are
; infix rather than prefix operators. Since the differentiation
; program is defined in terms of abstract data, we can modify it to
; work with different representations of expressions solely by
; changing the predicates, selectors, and constructors that define the
; representation of the algebraic expressions on which the
; differentiator is to operate.
;
; a. Show how to do this in order to differentiate algebraic
; expressions presented in infix form, such as (x + (3 * (x + (y +
; 2)))). To simplify the task, assume that + and * always take two
; arguments and that expressions are fully parenthesized.

; Adapting the predicates, selectors and constructors for part (a):

(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s) (caddr s))

(define (product? x) (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p) (caddr p))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list m1 '* m2))))

;TEST: 
;1 ]=> (deriv '(x + (3 * (x + (y + 2)))) 'x)
;Value: 4

; Adapting predicates, selectors and constructors for part (b):

(define (sum? exp) 
 (if (eq? #f (memq '+ exp)) #f #t))

(define (addend s) (make-exp (pre-list '+ s)))

(define (augend s) (make-exp (post-list '+ s)))

(define (product? exp)
  (if (eq? #f (memq '* exp))
      #f
      (not (sum? exp))))

(define (multiplier p) (make-exp (pre-list '* p)))

(define (multiplicand p) (make-exp (post-list '* p)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
	((=number? a2 0) a1)
	((and (number? a1) (number? a2)) (+ a1 a2))
	(else (append (make-exp-list a1) '(+) (make-exp-list a2)))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
	((=number? m1 1) m2)
	((=number? m2 1) m1)
	((and (number? m1) (number? m2)) (* m1 m2))
	(else (append (make-sum-exp-list m1) '(*) (make-sum-exp-list m2)))))

; Extra helper functions defined to support the predicates, selectors
; and constructors:

(define (make-exp-list exp)
  (if (list? exp) exp (list exp)))

(define (make-sum-exp-list exp)
  (cond ((not (list? exp)) (list exp))
	((sum? exp) (list exp))
	(else exp)))

(define (pre-list op exp)
  (if (eq? (car exp) op)
      '()
      (cons (car exp) (pre-list op (cdr exp)))))

(define (post-list op exp) (cdr (memq op exp)))

(define (make-exp exp-list)
  (if (> (length exp-list) 1)
      exp-list
      (car exp-list)))



; Provided "deriv" procedure:

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv (multiplicand exp) var))
           (make-product (deriv (multiplier exp) var)
                         (multiplicand exp))))
        (else
         (error "unknown expression type -- DERIV" exp))))


(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (=number? exp num)
  (and (number? exp) (= exp num)))


