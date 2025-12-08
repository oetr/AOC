#lang racket

(require advent-of-code)

(define input (fetch-aoc-input (find-session) 2025 7 #:cache #true))

(define (add-beam beams x n)
  (define n-worlds (hash-ref beams x #f))
  (if n-worlds
      ;; beam already exists, add new nof worlds to already existing nof worlds 
      (hash-set beams x (+ n n-worlds))
      (hash-set beams x n)))

(define (propagate beams line)
  (for/fold ([result (hash)]
             [split-times 0])
            ([(beam n-worlds) beams])
    (match (string-ref line beam)
      [#\^ (values (add-beam
                    (add-beam result (sub1 beam) n-worlds)
                    (add1 beam) n-worlds)
                   (+ 1 split-times))]
      [_ (values (add-beam result beam n-worlds)
                 split-times)])))

(define (parse-and-solve input)
  (define start (string-find input "S"))
  ;; hash map  x -> n, where
  ;;   x - beam x-coordinate
  ;;   n - number of worlds in which that beam exists
  (for/fold ([beams (hash start 1)]
             [split-times 0]
             #:result (values split-times
                              (for/sum ([(_ n) beams]) n)))
            ([line (string-split input)])
    (define-values (new-beams new-split-count)
      (propagate beams line))
    (values new-beams (+ split-times new-split-count))))

;; part 1 and 2
(time (parse-and-solve input))
;; cpu time: 1 real time: 1 gc time: 0
