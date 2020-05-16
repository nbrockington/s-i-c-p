; Procedure for strategy eye-for-two-eyes

(define (EYE-FOR-TWO-EYES my-history other-history)
  (if (or (empty-history? my-history)
          (empty-history? (rest-of-plays my-history)))
      "c"
      (if (and (string=? (most-recent-play other-history) "d")
               (string=? (car (rest-of-plays other-history)) "d"))
          "d" "c")))
