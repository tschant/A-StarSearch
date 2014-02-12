#lang racket
(require data/heap)
(require racket/trace)
(require srfi/1)
;structure used for the priority queue
(struct node (action state prev g h f handle))
;sorting algorithm for priority queue
(define (node<=? x y)
  (<= (node-f x) (node-f y)))
;Coordinates used for locations within the list
(define coords '((0 0) (0 1) (0 2) (1 0) (1 1) (1 2) (2 0) (2 1) (2 2)))

;creates a list of all the valid swaps
  (define (move state h)
    ;finds the zero with the current state
    (define (find-blank list)
      (cond ([not (null? list)]
             (list-index (curry equal? 0) (flatten list)))))
    ;determines if it is a valid swap and the direction
    (define (swap n source target h)
      (cond ([and (< target 9) (>= target 0)] 
             ;short hand for all the coords that were passed (potential valid moves)
             (let ([x1 (first (list-ref coords source))]
                   [x2 (first (list-ref coords target))]
                   [y1 (second (list-ref coords source))]
                   [y2 (second (list-ref coords target))]
                   ;quick ref for the list
                   [list (node-state n)])
               (cond ([not (null? list)]
                      ;check if x1 = x2 (left/right move)
                      ;true -> swap
                      (cond ([= x1 x2]
                             ;swap x1 for x2
                             (let ([curN (swap-tile list (list-ref (list-ref list x1) y1) (list-ref (list-ref list x2) y2))])
                               (node `(,(list-ref (list-ref list x2) y2) . ,(LR? y1 y2)) curN
                                     n (+ 1 (node-g n)) (h curN) (+ 1 (h curN) (node-g n)) 0)
                               ))
                            ;check if y1 = y2 (up/down move)
                            ;true -> swap
                            ([= y1 y2]
                             ;swap y1 for y2
                             (let ([curN (swap-tile list (list-ref (list-ref list x1) y1) (list-ref (list-ref list x2) y2))])
                               (node `(,(list-ref (list-ref list x2) y2) . ,(UD? x1 x2)) curN
                                     n (+ 1 (node-g n)) (h curN) (+ 1 (h curN) (node-g n)) 0)
                               ))
                            )))))
            ))
    ;determines if the move was left, right, up or down
    (define (LR? a b)
      (if (> a b) 'R 'L))
    (define (UD? a b)
      (if (> a b) 'D 'U))
    ;this recreates the list with the correct swap
    (define (swap-tile state source target)
      (cond ([null? state] '())
            (else
             (cond ([list? state]
                    (cons (swap-tile (car state) source target)
                          (swap-tile (cdr state) source target)))
                   (else (cond ([= state source] target)
                               ([= state target] source)
                               (else state)))))))
    ;starts the move function
    ;finds the zero, adds 1 and 3 to that location and swaps if valid move
    (cond ([not (null? (node-state state))]
           (let ([y (find-blank (node-state state))])
             (map (λ(x) 
                    (list (swap state y (+ y x) h)
                          (swap state y (- y x) h) )
                    ) '(1 3)))))
    )
  
(define (A*-graph-search curState goal? move heuristic)
  (define exploredHash (make-hash))
  (define frontQ (make-heap node<=?))
  (heap-add! frontQ (node 'start curState '() 0 (heuristic curState) (heuristic curState) 0))
  (let ([num-nodes 0])  
  (define (loop)
      (cond [(=  0 (heap-count frontQ))
             (list curState 'Impossible num-nodes)]
            [else 
             ;removes the first element of the queue
             (define n (heap-min frontQ))
             (heap-remove-min! frontQ)

             ;checks if the first element is the goal state
             (cond ((goal? (node-state n))
                    (list n num-nodes))
                   (else 
                    ;keeps track of how many times the loop was successfully called to find more moves
                    (set! num-nodes (+ 1 num-nodes))
                    ;if it isn't find the possible moves
                    (for-each 
                     (λ(x) (for-each (λ(y) 
                                       (cond [(not (or (void? y) (hash-has-key? exploredHash (node-state y))))
                                              ;add all generated moves to the hash table and to the heap
                                              (hash-set! exploredHash (node-state y) y)
                                              (heap-add! frontQ y)
                                              ]))  x))
                     (move n heuristic))
                    (loop)))
             ])) 
    (loop)))
;this prints the path that was found by A* along with the number of nodes visited
(define (print-solution node-list)
  (let ([num-nodes (cdr node-list)]
        [n (car node-list)])
    (if [equal? 'Impossible (car num-nodes)]
           (printf "No solution could be found for: ~a\n ~a nodes visited\n" n (cadr num-nodes))
          (cond ([empty? (node-prev n)]
                 (printf "~a nodes visited \n\t~a\n" (car num-nodes) `(,(node-action n) ,(node-state n) ,(node-g n) ,(node-h n) ,(node-f n))))
                (else 
                 (print-solution (list (node-prev n) (car num-nodes)))
                 (printf "\t~a\n" `(,(node-action n) ,(node-state n) ,(node-g n) ,(node-h n) ,(node-f n)))
                 )))))

(define (tile-puzzle puzzle goal)
  
  ;redefines goal to be goal-state that will be used instead
  (define goal-state goal)
  (cond ([null? goal]
         ;if tile-puzzle was called with null goal, then a default goal-state is assigned
         (set! goal-state '((1 2 3)(4 5 6)(7 8 0)))))
  ;goal function
  (define (goal? state)
    (if (equal? state goal-state)
        true
        false))
  
  ;heuristic functions
  ;h-null function
  (define (h-null state) 0)
  ;find the manhattan distance of the state that was called with
  (define (Man-Dist state)
    (cond ([not (null? state)]
           (let ([x (map (λ(x) (list-index (curry equal? x) (flatten state))) '(1 2 3 4 5 6 7 8))]
                 [y (map (λ(x) (list-index (curry equal? x) (flatten goal-state))) '(1 2 3 4 5 6 7 8))])
             
             (foldl + 0 (map (λ(a b) 
                               (+ (abs (- (first (list-ref coords b)) (first (list-ref coords a)))) 
                                  (abs (- (second (list-ref coords b)) (second (list-ref coords a)))))
                               ) x y))
             ))))
  ;executes both h-null and Man-Dist on the current puzzle
  
  (cond [(null? puzzle)
      (printf "Cannot solve a null puzzle")]
      ;(for-each (λ(x) (printf "~a\n" x) (print-solution (time (A*-graph-search puzzle goal? move x)))) (list h-null Man-Dist))
      (else
       (printf "H-Null: \n")
        (print-solution (time (A*-graph-search puzzle goal? move h-null)))
        (printf "\nManhattan-Distance: \n")
        (print-solution (time (A*-graph-search puzzle goal? move Man-Dist))))
      ))

(define test1 '((1 2 3) (4 5 6) (0 7 8)))
(define test2 '((6 4 2) (1 5 3) (7 0 8)))
(define test3 '((6 4 2) (8 5 3) (1 0 7)))
(define test4 '((6 4 7) (8 5 0) (3 2 1)))
(define test5 '((8 0 7) (6 5 4) (3 2 1)))
(define test6 '((1 2 3) (4 5 6) (8 7 0))) 
(define (test-all)
  (define test-puzzles (list test1 test2 test3 test4 test5 test6))
  (for-each (λ(x) (tile-puzzle x '())) test-puzzles))