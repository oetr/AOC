#lang racket

(require advent-of-code)

(define input (fetch-aoc-input (find-session) 2025 3 #:cache #true))

(define test-input "987654321111111
811111111111119
234234234234278
818181911112111")

(define (parse-data input)
  (for/list ([line (string-split input)])
    (for/list ([digit-str (regexp-match* #px"[0-9]" line)])
      (string->number digit-str))))

(define (find-max2 data)
  (for/fold ([result 0])
            ([bank data])
    (+ result 
       (for/fold ([max2 (cons 0 0)]
                  #:result (let ([result (+ (* 10 (car max2)) (cdr max2))])
                             ;; (printf "res: ~a~n" result)
                             result))
                 ([joltage bank])
         (define 1st-max (car max2))
         (define 2nd-max (cdr max2))
         (if (> 2nd-max 1st-max)
             ;; move
             (cons 2nd-max joltage)
             (cons 1st-max (max 2nd-max joltage)))))))

;; part 1
(time (find-max2 (parse-data input)))
;; cpu time: 23 real time: 23 gc time: 0

;; part 2
(define (append-digit-with-max-flow nums new-num)
  (let loop ([n-1 (car nums)]
             [nums (cdr nums)]
             [result empty])
    ;;(printf "HERE: n-1: ~a nums: ~a result: ~a~n" n-1 nums result)
    (define n (car nums))
    (define len (length nums))
    (cond [(> n n-1) ;; shift and exit
           ;;(printf "   >~n")
           (if (= 1 len) ;; processing last digit
               (reverse (cons new-num (cons n result)))
               (append (reverse (cons n result)) (cdr nums) (list new-num)))]
          [(= 1 len) ;; check last digit and exit
           ;;(printf "   =1\n")
           (reverse (cons (max n new-num) (cons n-1 result)))]
          [else
           ;;(printf "   loop~n")
           (loop n (cdr nums) (cons n-1 result))])))

(define (digits->number digits)
  (string->number
   (string-join (map number->string digits)
                "")))

(define (find-max12 data)
  (for/sum ([bank data])
    (for/fold ([current-best (take bank 12)]
               #:result (digits->number current-best))
              ([n (drop bank 12)])
      (append-digit-with-max-flow current-best n))))


(time (find-max12 (parse-data input)))
;; cpu time: 27 real time: 27 gc time: 0
