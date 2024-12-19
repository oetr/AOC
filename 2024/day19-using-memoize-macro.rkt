#lang racket

(require advent-of-code
         "./lib.rkt")

(define input (fetch-aoc-input (find-session) 2024 19 #:cache #true))

(define (parse-input input)
  (define data (string-split input "\n\n"))
  (values
   (string-split (car data) ", ")
   (string-split (cadr data) "\n")))

(define/memoize (match-design design patterns)
  (or (string=? "" design)
      (for/or ([pattern patterns]
               #:when (string-prefix? design pattern))
        (match-design (substring design (string-length pattern)) patterns))))

(define-values (patterns designs) (parse-input input))

(time (for/sum ([design designs]
                #:when (match-design design patterns))
        1))
;; cpu time: 39 real time: 39 gc time: 0

(define/memoize (match-all-designs design patterns)
  (if (string=? "" design)
      1
      (for/sum ([pattern patterns]
                #:when (string-prefix? design pattern))
        (match-all-designs (substring design (string-length pattern)) patterns))))

(time (for/sum ([design designs])
        (match-all-designs design patterns)))
;; cpu time: 114 real time: 114 gc time: 0
