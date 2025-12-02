#lang racket

(require advent-of-code)

(define input (fetch-aoc-input (find-session) 2025 2 #:cache #true))

;;(define input "11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124\n")

(define data
  (for/list ([a-range (string-split (string-trim input) ",")])
    (map string->number (string-split a-range "-"))))

;; part 1 brute force
(for/fold ([result 0]) ([a-range data])
  (+ result
     (for/fold ([result 0]) ([num (range (first a-range) (+ 1 (second a-range)))])
       (define n (number->string num))
       (define len (string-length n))
       (if (odd? len)
           result
           ;; left half equals right half
           (if (string=? (substring n 0 (/ len 2)) (substring n (/ len 2)))
               (+ result num)
               result)))))

;; part 2 brute force
(for/fold ([result 0]) ([a-range data])
  (+ result
     (for/fold ([result 0]) ([num (range (first a-range) (+ 1 (second a-range)))])
       (define n (number->string num))
       (define len (string-length n))
       (define sizes
         (for/list ([size (range 1 (+ 1 (/ len 2)))]
                    #:when (and (zero? (modulo len size)) (not (= 1 len))))
           size))
       (+ result
          (if (for/or ([size sizes])
                ;; found repetition
                (define s (substring n 0 size))
                (for/and ([offset (range size len size)])
                  (string=? s (substring n offset (+ offset size)))))
              num
              0)))))
