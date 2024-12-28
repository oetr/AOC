#lang racket

(require advent-of-code
         "./priority-queue.rkt")

(define input (fetch-aoc-input (find-session) 2024 16 #:cache #true))
;; (define input
;;   "###############
;; #.......#....E#
;; #.#.###.#.###.#
;; #.....#.#...#.#
;; #.###.#####.#.#
;; #.#.#.......#.#
;; #.#.#####.###.#
;; #...........#.#
;; ###.#.#####.#.#
;; #...#.....#.#.#
;; #.#.#.###.#.#.#
;; #.....#...#.#.#
;; #.###.#.#.#.#.#
;; #S..#.....#...#
;; ###############")

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
  ;;(define i 0)
  (define frontier (make-priority-queue node< (node 0 start '())))
  (let loop ([solutions '()]
             [best-cost (expt 2 32)])
    (cond
      [(priority-queue-empty? frontier) #f]
      [else
       (define candidate-node (priority-queue-poll! frontier))
       (define posn (node-posn candidate-node))
       (define cost (node-cost candidate-node))
       (cond
         [(posn-xy= goal posn)
          (if (<= cost best-cost)
              (loop (cons candidate-node solutions) cost)
              solutions)]
         [else
          (set-add! explored posn)
          (for ([action (get-possible-next-states grid posn)])
            (define new-cost (+ cost
                                (if (symbol=? (posn-direction posn)
                                              (posn-direction action))
                                    1
                                    1000)))
            (unless (set-member? explored action)
              (priority-queue-add! frontier (node new-cost action candidate-node))))
          (loop solutions best-cost)])])))


(define (extract-path a-node)
  (define cost (node-cost a-node))
  (let loop ([a-node a-node][path '()])
    (match a-node
      [(node _ _ '()) (values cost path)]
      [(node _ posn parent) (loop parent
                                  (cons posn path))])))

;; part 1
(define-values (part1-cost part1-path) (time (extract-path (car (find-shortest-path grid start goal)))))
;; cpu time: 2450 real time: 2450 gc time: 111
;; using the improvements from Russel&Norvig, will push this time down to 900ms:
;; update the node in the frontier if we encounter lower cost

;; part 2
(time
 (set-count
  (for/fold ([unique-tiles (set)]) ([solution (find-shortest-path grid start goal)])
    (define-values (_ path) (extract-path solution))
    (set-union unique-tiles (for/set ([tile path])
                              (cons (posn-x tile) (posn-y tile)))))))

;; cpu time: 2450 real time: 2450 gc time: 111
