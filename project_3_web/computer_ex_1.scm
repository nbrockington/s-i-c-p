; Provided procedure DFS-simple for depth-first search of the graph:

(define (DFS-simple start goal? graph)
  (search start
          goal?
          find-node-children
          (lambda (new old) (append new old))
          graph))

