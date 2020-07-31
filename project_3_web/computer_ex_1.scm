; Provided procedure DFS-simple for depth-first search of the graph:
; (Additional provided code from search.scm and generate.scm needed.)

(define (DFS-simple start goal? graph)
  (search start
          goal?
          find-node-children
          (lambda (new old) (append new old))
          graph))


; My procedure BFS-simple for breadth-first search of the graph:

(define (BFS-simple start goal? graph)
  (search start
          goal?
          find-node-children
          (lambda (new old) (append old new))
          graph))

; Only the fourth argument, which controls the merging method, has                              
; changed, whereby (append new old) has become (append old new).  This                          
; changes it from depth-first search to bread-first search because now                          
; the children of the previously-interogated node are earlier in the                            
; search list than the children of the new node. This ensure that                               
; nodes in the same generated are all searched first, before any of                             
; their children are search i.e. breadth-first search. 

; Testing: 

(BFS-simple 'a (lambda (node) (eq? node 'l)) test-graph)
;(now-at a)
;(now-at b)
;(now-at i)
;(now-at m)
;(now-at c)
;(now-at d)
;(now-at e)
;(now-at h)
;(now-at j)
;(now-at k)
;(now-at l)
;Value: #t

(DFS-simple 'a (lambda (node) (eq? node 'l)) test-graph)
;(now-at a)
;(now-at b)
;(now-at c)
;(now-at d)
;(now-at e)
;(now-at f)
;(now-at g)
;(now-at h)
;(now-at i)
;(now-at j)
;(now-at k)
;(now-at l)
;Value: #t

