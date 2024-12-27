#lang racket

(require advent-of-code
         "./priority-queue.rkt")

 (define input (fetch-aoc-input (find-session) 2024 16 #:cache #true))

;; (define input
;;   "#################
;; #...#...#...#..E#
;; #.#.#.#.#.#.#.#.#
;; #.#.#.#...#...#.#
;; #.#.#.#.###.#.#.#
;; #...#.#.#.....#.#
;; #.#.#.#.#.#####.#
;; #.#...#.#.#.....#
;; #.#.#####.#.###.#
;; #.#.#.......#...#
;; #.#.###.#####.###
;; #.#.#...#.....#.#
;; #.#.#.#####.###.#
;; #.#.#.........#.#
;; #.#.#.#########.#
;; #S#.............#
;; #################")

(struct posn (x y direction) #:transparent)

(define (get-grid input)
  (define start #f)
  (define goal #f)
  (define grid
    (for/vector ([(line y) (in-indexed (string-split input "\n"))])
      (for/vector ([(c x) (in-indexed line)])
        (match c
          [#\S (set! start (posn x y 'right)) #\.]
          [#\E (set! goal (posn x y 'unused)) #\.]
          [_ c]))))
  (values grid start goal))

(define (in-grid? grid p)
  (define direction (posn-direction p))
  (define x (posn-x p))
  (define y (posn-y p))
  (and (>= x 0)
       (>= y 0)
       (< x (vector-length (vector-ref grid 0)))
       (< x (vector-length grid))))

(define (& grid p)
  (vector-ref (vector-ref grid (posn-y p)) (posn-x p)))

(define (get-possible-next-states grid p)
  (define x (posn-x p))
  (define y (posn-y p))
  (define next-state-candidates
    (match (posn-direction p)
      ['up    (list (posn x y 'left)
                    (posn x y 'right)
                    (posn x (- y 1) 'up))]
      ['down  (list (posn x y 'left)
                    (posn x y 'right)
                    (posn x (+ y 1) 'down))]
      ['left  (list (posn x y 'up)
                    (posn x y 'down)
                    (posn (- x 1) y 'left))]
      ['right (list (posn x y 'up)
                    (posn x y 'down)
                    (posn (+ x 1) y 'right))]))
  (for/list ([state next-state-candidates]
             #:when (and (in-grid? grid state)
                         (not (char=? #\# (& grid state)))))
    state))

;;(get-possible-next-states grid start)

(define-values (grid start goal) (get-grid input))

(struct node (cost posn parent) #:mutable)

(define (node< n1 n2)
  (< (node-cost n1) (node-cost n2)))

(define (posn-xy= p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))))

(define (posn= p1 p2)
  (and (= (posn-x p1) (posn-x p2))
       (= (posn-y p1) (posn-y p2))
       (symbol=? (posn-direction p1) (posn-direction p2))))


(define (find-shortest-path grid start goal)
  (define explored (mutable-set))
  (define i 0)
  (let loop ([frontier (make-priority-queue node< (node 0 start '()))])
    (cond
      [(= i 1000000) #f]
      [(priority-queue-empty? frontier) #f]
      [else
       (set! i (add1 i))
       ;;(printf "~a - "(priority-queue-length frontier))
       (define candidate-node (priority-queue-poll! frontier))
       (define posn (node-posn candidate-node))
       (define cost (node-cost candidate-node))
       ;;(define parent-posn (if (empty? (node-parent candidate-node)) '() (node-posn (node-parent candidate-node))))
       ;;(printf "~a: ~a   -    ~a~n" cost parent-posn posn)
       ;; (for ([n frontier]) (printf "    ~a   " (cons (node-cost n) (node-posn n))))
       ;; (printf "~n")
       ;;(printf "~a - ~a\n"(priority-queue-length frontier) posn)
       ;;(printf "~a - ~a~n" goal posn)
       (cond
         [(posn-xy= goal posn) candidate-node]
         [else
          (set-add! explored posn)
          (for ([action (get-possible-next-states grid posn)])
            (define new-cost (+ cost
                                (if (symbol=? (posn-direction posn)
                                           (posn-direction action))
                                 1
                                 1000)))
            (define already-contains? (priority-queue-contains? frontier
                                                                (λ (a)
                                                                  (posn= (node-posn a)
                                                                         action))))
            (cond [(not (or (set-member? explored action) already-contains?))
                   (priority-queue-add! frontier (node new-cost action candidate-node))]
                  [already-contains?
                   (priority-queue-update! frontier
                                           (λ (a) (posn= (node-posn a) action))
                                           (λ (a) (when (< new-cost (node-cost a))
                                                    (set-node-cost! a new-cost)
                                                    (set-node-parent! a candidate-node))))]
                  [else void]))
          (loop frontier)])])))


(define (extract-path a-node)
  (define cost (node-cost a-node))
  (let loop ([a-node a-node][path '()])
    (match a-node
      [(node _ _ '()) (values cost path)]
      [(node _ posn parent) (loop parent
                                  (cons posn path))])))

(define sp (time (find-shortest-path grid start goal)))

(define-values (cost path) (extract-path sp))
cost
;;path
