#lang racket

(require advent-of-code
         "../2024/lib.rkt")

(define test-input ".......S.......
...............
.......^.......
...............
......^.^......
...............
.....^.^.^.....
...............
....^.^...^....
...............
...^.^...^.^...
...............
..^...^.....^..
...............
.^.^.^.^.^...^.
...............")

(define input (fetch-aoc-input (find-session) 2025 7 #:cache #true))

(struct grid (start splitters width height) #:transparent)

(define (parse-input input)
  (define lines (string-split input "\n"))
  (define height (length lines))
  (define width (string-length (first lines)))
  (define start (cons 1 (string-find (first lines) "S")))
  (define splitters
    (for*/fold ([splitters (set)])
               ([(line row) (in-indexed lines)]
                [(c col) (in-indexed line)]
                #:when (char=? #\^ c))
      (set-add splitters (cons row col))))
  (grid start splitters width height))

(define (in-grid? grid p)
  (define row (car p))
  (define col (cdr p))
  (and (>= row 0)
       (<= row (grid-width grid))
       (<= col (grid-height grid))
       (>= col 0)))

;; bfs
(define (propagate-beam world)
  (for/fold ([times-split 0]
             [tachyons (set (grid-start world))]
             [frontier (set (grid-start world))]
             #:result times-split)
            ([_ (in-range 1 (grid-height world))])
    (define-values (additional-times-split new-frontier)
      (for/fold ([times-split 0]
                 [new-tachyons (set)])
                ([tachyon frontier])
        (define row (add1 (car tachyon)))
        (define col (cdr tachyon))
        (if (set-member? (grid-splitters world) (cons row col))
            (values (+ 1 times-split)
                    (set-union new-tachyons
                               (for/set ([candidate (list (cons row (sub1 col))
                                                          (cons row (add1 col)))]
                                         #:when (in-grid? world candidate))
                                 candidate)))
            (values times-split
                    (set-add new-tachyons (cons row col))))))
    (values (+ times-split additional-times-split)
            (set-union tachyons new-frontier)
            new-frontier)))

;; part 1
(time (propagate-beam (parse-input input)))
;; cpu time: 5 real time: 5 gc time: 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (propagate world tachyon)
  (define row (add1 (car tachyon)))
  (define col (cdr tachyon))
  (define move-on (cons row col))
  (if (set-member? (grid-splitters world) move-on)
      ;; split in two
      (list (cons row (sub1 col))
            (cons row (add1 col)))
      (list move-on)))

(define (part2 input)
  (define world (parse-input input))

  (define/memoize (count-worlds tachyon)
    (if (> (car tachyon) 
           (grid-height world))
        1 ;; reached the end of the world, time to return
        (for/sum ([next (propagate world tachyon)])
          (count-worlds next))))
  
  (count-worlds (grid-start world)))

(time (part2 input))
;; cpu time: 5 real time: 5 gc time: 0
