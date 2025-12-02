#lang racket

(require advent-of-code)

(define input (fetch-aoc-input (find-session) 2025 2 #:cache #true))

;;(define input "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124\n")

(define data
  (for/list ([a-range (string-split (string-trim input) ",")])
    (map string->number (string-split a-range "-"))))

(define (sum-filtered-ranges predicate ranges)
  (for*/sum ([range (in-list ranges)]
             [num (in-range (first range) (add1 (second range)))]
             #:when (predicate num))
    num))

(define (halfs-equal? num)
  (define num-str (number->string num))
  (define len (string-length num-str))
  (and (even? len)
       (> len 0)
       (let ([mid (quotient len 2)])
         (string=? (substring num-str 0 mid)
                   (substring num-str mid)))))

;; part 1
(sum-filtered-ranges halfs-equal? data)

(define (substring-repeats? str pattern-len)
  (define pattern (substring str 0 pattern-len))
  (for/and ([offset (in-range pattern-len (string-length str) pattern-len)])
    (string=? pattern (substring str offset (+ offset pattern-len)))))

(define (has-repeating-number? num)
  (define num-str (number->string num))
  (define len (string-length num-str))
  (for/or ([pattern-len (in-range 1 (add1 (quotient len 2)))]
           #:when (> len 1)
           #:when (zero? (modulo len pattern-len)))
    (substring-repeats? num-str pattern-len)))

;; part 2
(sum-filtered-ranges has-repeating-number? data)
