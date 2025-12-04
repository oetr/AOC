#lang racket

(require advent-of-code)

(define input (fetch-aoc-input (find-session) 2025 4 #:cache #true))

(define test-input "..@@.@@@@.
@@@.@.@.@@
@@@@@.@.@@
@.@@@@..@.
@@.@@@@.@@
.@@@@@@@.@
.@.@.@.@@@
@.@@@.@@@@
.@@@@@@@@.
@.@.@@@.@.")

(define (parse-input input)
  (for/vector ([line (string-split input)])
    (for/vector ([c line])
      c)))

(define (matrix-ref mat row col)
  (vector-ref (vector-ref mat row) col))

(define matrix-rows vector-length)

(define (matrix-cols mat)
  (vector-length (vector-ref mat 0)))

(define (matrix-fold mat acc proc)
  (for*/fold ([acc acc])
             ([(row row-id) (in-indexed mat)]
              [(col col-id) (in-indexed row)])
    (proc mat acc col row-id col-id)))

(struct posn (r c) #:transparent)

(define (posn+ . posns)
  (posn (apply + (map posn-r posns))
        (apply + (map posn-c posns))))

(define (posn-filter predicate? posns)
  (for/list ([p posns]
             #:when (predicate? p))
    p))

(define 8-adjacent-coords
  (list (posn 0 -1) (posn 0 1) (posn 1 0) (posn -1 0)
        (posn -1 -1) (posn -1 1) (posn 1 -1) (posn 1 1)))

(define (8-adjacent? mat acc element row-id col-id)
  (define rows (matrix-rows mat))
  (define cols (matrix-cols mat))
  (define current-posn (posn row-id col-id))
  (cond
    ;; roll of paper 
    ([char=? element #\@]
     ;; filter positions that exist
     (define adjacent-paper-rolls
       (posn-filter (lambda (p)
                      (define adjacent-posn (posn+ current-posn p))
                      (define r (posn-r adjacent-posn))
                      (define c (posn-c adjacent-posn))
                      (and (>= r 0)
                           (< r rows)
                           (>= c 0)
                           (< c cols)
                           (char=? #\@
                                   (matrix-ref mat r c))))
                    8-adjacent-coords))
     (if (< (length adjacent-paper-rolls) 4)
         (+ acc 1)
         acc))
    ;; empty
    (else acc)))

(define (part1 input)
  (define grid (parse-input input))
  (matrix-fold grid 0 8-adjacent?))

(time (part1 input))
;; cpu time: 26 real time: 26 gc time: 0

