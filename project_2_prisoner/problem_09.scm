;;                                                                                    
;;  The play-loop procedure takes as its  arguments two prisoner's                    
;;  dilemma strategies, and plays an iterated game of approximately                   
;;  one hundred rounds.  A strategy is a procedure that takes                         
;;  two arguments: a history of the player's previous plays and                       
;;  a history of the other player's previous plays.  The procedure                    
;;  returns either a "c" for cooperate or a "d" for defect.                           
;;                                                                                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                  

;;;;;;;;;;;;;;;;;;;;;;;;;                                                             
;;                                                                                    
;; code to use in 3 player game                                                       
;;                                                                                    

(define (play-loop-3 strat0 strat1 strat2)
  (define (play-loop-iter strat0 strat1 strat2 count hist0 hist1 hist2 limit)
    (cond ((= count limit) (print-out-results-3 hist0 hist1 hist2 limit))
            (else (let ((result0 (strat0 hist0 hist1 hist2))
                        (result1 (strat1 hist1 hist2 hist0))
                        (result2 (strat2 hist2 hist0 hist1)))
                  (display (cons result0 (cons result1 (cons result2 '()))))
                  (newline)
                  (play-loop-iter strat0 strat1 strat2 (+ count 1)
                                  (extend-history result0 hist0)
                                  (extend-history result1 hist1)
                                  (extend-history result2 hist2)
                                  limit)))))
  (play-loop-iter strat0 strat1 strat2 0
                  the-empty-history the-empty-history the-empty-history
                  (+ 90 (random 21))))
                  
                  
                  
(define *game-association-list-3*
  (list (list (list "c" "c" "c") (list 4 4 4))
        (list (list "c" "c" "d") (list 2 2 5))
        (list (list "c" "d" "c") (list 2 5 2))
        (list (list "d" "c" "c") (list 5 2 2))
        (list (list "c" "d" "d") (list 0 3 3))
        (list (list "d" "c" "d") (list 3 0 3))
        (list (list "d" "d" "c") (list 3 3 0))
        (list (list "d" "d" "d") (list 1 1 1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;                                                  
;;                                                                                    
;;  The following procedures are used to compute and print                            
;;  out the players' scores at the end of an iterated game                            
;;                                                                                    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 



(define (print-out-results-3 hist0 hist1 hist2 number-of-games)
  (let ((scores (get-scores-3 hist0 hist1 hist2)))
    (newline)
    (display "Player 1 Score:  ")
    (display (* 1.0 (/ (car scores) number-of-games)))
    (newline)
    (display "Player 2 Score:  ")
    (display (* 1.0 (/ (cadr scores) number-of-games)))
    (newline)
    (display "Player 3 Score:  ")
    (display (* 1.0 (/ (caddr scores) number-of-games)))
    (newline)))


(define (get-scores-3 hist0 hist1 hist2)
  (define (get-scores-helper hist0 hist1 hist2 score0 score1 score2)
    (cond ((empty-history? hist0) (list score0 score1 score2))
            (else (let ((game (make-play (most-recent-play hist0)
                                         (most-recent-play hist1)
                                         (most-recent-play hist2))))
                       (get-scores-helper (rest-of-plays hist0)
                                          (rest-of-plays hist1)
                                          (rest-of-plays hist2)
                                          (+ (get-player-points 0 game) score0)
                                          (+ (get-player-points 1 game) score1)
                                          (+ (get-player-points 2 game) score2))))))
  (get-scores-helper hist0 hist1 hist2 0 0 0))
  

(define (get-player-points num game)
  (list-ref (get-point-list game) num))

(define (get-point-list game)
  (cadr (extract-entry game *game-association-list-3*)))

;; note that you will need to write extract-entry                                     

(define (extract-entry game association-list)
   (cond ((null? association-list) (display "Error: This game not found in list."))
         ((equal? game (car (car association-list))) (car association-list))
         (else (extract-entry game (cdr association-list)))))

(define make-play list)

(define the-empty-history '())

(define extend-history cons)
(define empty-history? null?)

(define most-recent-play car)
(define rest-of-plays cdr)

