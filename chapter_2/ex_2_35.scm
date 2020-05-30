; Redefine count-leaves from section 2.2.2 as an accumulation:

; Procedure count-leaves from Section 2.2.2:

(define (prev-count-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))


; Procedure count-leaves redefined as an accumulation:

(define (count-leaves t)
  (accumulate (lambda (x y) (+ (length x) y))
              0 
              (map create-flat-list t)))


; Calling the following procedures:

(define (length x) 
  (if (null? x) 0 (+ 1 (length (cdr x)))))

(define (create-flat-list x)
  (cond ((null? x) x)
        ((not(pair? x)) (list x))
        (else (append (create-flat-list (car x))
                      (create-flat-list (cdr x))))))

; NB. (Create-flat-list) is equivalent to (enumerate-tree) from the
; text)
;
; NB2. This seems a silly way to do it, since if you have
; (enumerate-tree), you can just use (length (enumerate-tree)).
;
; A much more satisfying solution from
; http://mngu2382.github.io/sicp/chapter2/01-exercise09.html
; 
; (define (count-leaves tree)
;    (accumulate +
;                0
;                (map (lambda (x) 1) (enumerate-tree tree))))
