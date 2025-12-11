#lang racket

(require advent-of-code
         memo)

(define input (fetch-aoc-input (find-session) 2025 11 #:cache #true))

(define (parse input)
  (hash-set
   (for/hash ([line (string-split input "\n")])
    (define a-node (string-split line ": "))
    (define from (string->symbol (car a-node)))
    (define to (map string->symbol (string-split (cadr a-node) " ")))
    (values from to))
   'out empty))

(define (find-nof-paths nodes start end (needs-to-pass-dac-fft? #f))
  (define/memoize (loop node seen-dac? seen-fft?)
    (cond
      [(symbol=? end node)
       (if needs-to-pass-dac-fft?
           (if (and seen-dac? seen-fft?) 1 0)
           1)]
      [else
       (for/sum ([descendant (hash-ref nodes node empty)])
         (loop descendant
               (or seen-dac? (symbol=? node 'dac))
               (or seen-fft? (symbol=? node 'fft))))]))
  (loop start #f #f))

(define nodes (parse input))

;; part 1
(find-nof-paths nodes 'you 'out)

;; part 2
(find-nof-paths nodes 'svr 'out #t)
