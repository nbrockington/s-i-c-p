; Problem 10: A sampler of 3-player strategies:

(define (PATSY-3 my-hist other-hist-1 other-hist-2)
  "c")

(define (NASTY-3 my-hist other-hist-1 other-hist-2)
  "d")

(define (RANDOM-3 my-hist other-hist-1 other-hist-2)
  (if (= (random 2) 0)
      "c"
      "d"))

; Written by me:

(define (TOUGH-EYE-FOR-EYE my-hist other-hist-1 other-hist-2)
  (cond ((empty-history? my-hist) "c")
        ((or (string=? (most-recent-play other-hist-1) "d")
             (string=? (most-recent-play other-hist-2) "d"))
              "d")
              (else "c")))

(define (SOFT-EYE-FOR-EYE my-hist other-hist-1 other-hist-2)
  (cond ((empty-history? my-hist) "c")
        ((and (string=?  (most-recent-play other-hist-1) "d")
              (string=?  (most-recent-play other-hist-2) "d"))
              "d")
              (else "c")))
