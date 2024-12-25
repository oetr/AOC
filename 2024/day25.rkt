#lang racket

(require advent-of-code
         racket/struct)

(define input (fetch-aoc-input (find-session) 2024 25 #:cache #true))

(define-values (locks keys)
  (let ([locks-keys (string-split input "\n\n")])
    (for/fold ([locks '()]
               [keys '()])
              ([lock-key locks-keys])
      (define candidate (string-split lock-key "\n"))
      (define heights
        (for/fold ([counts (make-list 5 0)])
                  ([line (take (cdr candidate) 5)])
          (for/list ([c line]
                     [a-count counts])
            (if (char=? c #\#)
                (+ a-count 1)
                a-count))))
      (if (string=? "#####" (car candidate))
          (values (cons heights locks) keys) 
          (values locks (cons heights keys))))))

(define (dont-overlap? lock key)
  (andmap (λ (a b)
            (< (+ a b) 6)) lock key))

(for*/sum ([lock locks]
           [key keys]
           #:when (dont-overlap? lock key))
  1)

;; 59199 too high
;; had a bug: had #:unless  in the loop, and the name was overlap?
;; changing to #:when and dont-overlap? in function name fixed that!
