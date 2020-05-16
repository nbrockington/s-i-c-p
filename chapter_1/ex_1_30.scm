; The sum procedure above generates a linear recursion. The procedure
; can be rewritten so that the sum is performed iteratively. Show how
; to do this by filling in the missing expressions in the following
; definition:

; Linear recursive (sum) procedure:

; (define (sum term a next b)
;   (if (> a b)
;      0
;      (+ (term a)
;         (sum term (next a) next b))))


; New, iterative (sum) procedure

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ (term a) result))))
  (iter a 0))


(define (inc a)
   (+ a 1))


; Test: should return "55"

(sum square 1 inc 5)
