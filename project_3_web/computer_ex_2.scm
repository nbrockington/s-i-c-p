; My procedure search-with-cycles that handles cycles by keeping track                          
; of already-visited notes in argument already-done:                                            

(define (search-with-cycles initial-state goal? successors merge graph)
  (define (search-inner still-to-do already-done)
    (if (null? still-to-do)
        #f
        (let ((current (car still-to-do)))
          (cond ((memq current already-done)
                 (search-inner (cdr still-to-do) already-done))
                (else
                 (if *search-debug*
                     (write-line (list 'now-at current)))
                 (if (goal? current)
                     #t
                     (search-inner
                      (merge (successors graph current) (cdr still-to-do))
                      (cons current already-done))))))))
  (search-inner (list initial-state) '() ))


; A depth-first search that handles cycles:                                                     
(define (DFS start goal? graph)
  (search-with-cycles start
                      goal?
                      find-node-children
                      (lambda (new old) (append new old))
                      graph))


; Testing: visits each node at most once when cannot find the goal:                             

(DFS 'a (lambda (node) (eq? node 'x)) test-cycle)                                        
;(now-at a)                                                                                     
;(now-at b)                                                                                    
;(now-at c)                                                                                     
;Value: #f                                                                                      

(DFS 'http://sicp.csail.mit.edu/ (lambda (node) (eq? node 'x)) the-web)                  
;(now-at http://sicp.csail.mit.edu/)                                                            
;(now-at http://sicp.csail.mit.edu/schemeimplementations)                                       
;(now-at http://sicp.csail.mit.edu/getting-help.html)                                           
;(now-at http://sicp.csail.mit.edu/lab-use.html)                                                
;(now-at *the-goal*)                                                                            
;(now-at http://sicp.csail.mit.edu/psets)                                                       
;Value: #f                                                                                      


; A breadth-first search that handles cycles:                                                   
(define (BFS start goal? graph)
  (search-with-cycles start
                      goal?
                      find-node-children
                      (lambda (new old) (append old new))
                      graph))
                      
                      
; Testing: visits each note at most once, but in different order:                               

(BFS 'a (lambda (node) (eq? node 'x)) test-cycle)                                        
;(now-at a)                                                                                     
;(now-at b)                                                                                     
;(now-at c)                                                                                     
;Value: #f                                                                                      

(BFS 'http://sicp.csail.mit.edu/ (lambda (node) (eq? node 'x)) the-web)                  
;(now-at http://sicp.csail.mit.edu/)                                                            
;(now-at http://sicp.csail.mit.edu/schemeimplementations)                                       
;(now-at http://sicp.csail.mit.edu/psets)                                                       
;(now-at http://sicp.csail.mit.edu/getting-help.html)                                           
;(now-at http://sicp.csail.mit.edu/lab-use.html)                                                
;(now-at *the-goal*)                                                                            
;Value: #f        
