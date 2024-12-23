#lang racket

(require advent-of-code
         threading)

(define input (fetch-aoc-input (find-session) 2024 22 #:cache #true))

(define start
  (~> input
      string-split
      (map string->number _)))

(define (mix+prune n secret)
  (modulo (bitwise-xor n secret) 16777216))

(define (compute secret)
  (define n1 (mix+prune (arithmetic-shift secret 6) secret))
  (define n2 (mix+prune (arithmetic-shift n1 -5) n1))
  (mix+prune (arithmetic-shift n2 11) n2))

(define (compute-n-th n steps)
  (for/fold ([secret n]) ([_ (in-range steps)])
    (compute secret)))

;; part 1
(time 
 (for/sum ([n (in-list start)])
   (compute-n-th n 2000)))
;; cpu time: 93 real time: 93 gc time: 0

;; part 2
(define (get-ones n)
  (modulo n 10))

(define (compute-sequence n steps)
  (define-values (last-secret sequence-rev)
    (for/fold ([secret n]
               [s '()]) ([_ steps])
      (define n1 (mix+prune (* secret 64) secret))
      (define n2 (mix+prune (truncate (/ n1 32)) n1))
      (define new-secret (mix+prune (* n2 2048) n2))
      (values new-secret
              (cons new-secret s))))
  (define sequence (reverse sequence-rev))
  (define s (cons (get-ones n) (map get-ones sequence)))
  (cons s
        (for/list ([n1 s]
             [n2 (cdr s)])
          (- n2 n1))))


(define (index-sequence diff)
  (define diffs (cdr diff))
  (for/fold ([index (hash)])
            ([n (drop (car diff) 4)]
             [d1 diffs]
             [d2 (cdr diffs)]
             [d3 (cddr diffs)]
             [d4 (cdddr diffs)])
    (define k (list d1 d2 d3 d4))
    (hash-set index k
              (cons n (hash-ref index k '())))))


(define diffs (for/list ([n start])
                (compute-sequence n 2000)))

(define indices (for/list ([diff diffs])
                  (index-sequence diff)))

;; get all possible sequences
(define all-prices-changes (for/fold ([indices (set)])
                                     ([idx indices])
                             (set-union indices
                                        (for/set ([(k v) (in-hash idx)])
                                          k))))

;; check for each price change
(define prices
  (for/hash ([price-change all-prices-changes])
    (values price-change (for/sum ([idx indices])
                           (last (hash-ref idx price-change '(0)))))))

(for/fold ([max-price 0])
          ([(s price) (in-hash prices)])
  (if (> price max-price)
      price
      max-price))


