; Extra Credit: The Three-Player Prisoner's Dilemma Tournament:
; Submitted procedure: SMART-EYE-FOR-EYE: 

(define (SMART-EYE-FOR-EYE my-hist other-hist-1 other-hist-2)
  (cond ((empty-history? my-hist) "c")
	((= (length my-hist) 109) "d")
	((string=? (most-recent-play other-hist-1) (most-recent-play other-hist-2))
         (most-recent-play other-hist-1))
        (else (if (= (random 2) 0) "c" "d"))))
