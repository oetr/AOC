#lang racket

(require advent-of-code)

(define test-input "123 328  51 64 
 45 64  387 23 
  6 98  215 314
*   +   *   +")

(define input (fetch-aoc-input (find-session) 2025 6 #:cache #true))

(define (parse-input input
                     #:convert? [convert? #t])
  (for/vector ([line (string-split input "\n")])
    (for/vector ([element (string-split line)])
      (define num (string->number element))
      (if num
          (if convert? num element)
          (cond [(string=? "+" element) +]
                [(string=? "*" element) *])))))

(define (part1 input)
  (define data (parse-input input))
  (define rows (vector-length data))
  (define cols (vector-length (vector-ref data 0)))
  (define ops (vector-ref data (- rows 1)))
  (define result (for/vector ([op ops])
                   (if (eq? op +) 0 1)))
  (for* ([col cols]
         [row (- rows 1)])
    (define op (vector-ref ops col))
    (vector-set! result
                 col
                 (op 
                  (vector-ref result col)
                  (vector-ref (vector-ref data row) col))
                 ))
  (for/sum ([n result]) n))

(time (part1 input))
;; cpu time: 7 real time: 7 gc time: 5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (c->i char)
  (- (char->integer char) (char->integer #\0)))

(define (list->number lst)
  (define len (length lst))
  (for/sum ([(n i) (in-indexed lst)])
    (* (expt 10 (- len i 1)) n)))

(define (part2 input)
  ;; spaces are important here
  (define rows (string-split input "\n"))
  (define num-cols (string-length (car rows)))
  (define data (drop-right rows 1))
  (define ops 
    (for/list ([(op i) (in-indexed (last rows))]
               #:when (not (char=? op #\space)))
      (cons i
            (cond [(char=? #\+ op) +]
                  [(char=? #\* op) *]))))
  (for/sum ([op ops]
            [next-op (append (cdr ops) (list 'last))])
    (define from (car op))
    (define to (if (symbol? next-op)
                   num-cols
                   (sub1 (car next-op))))
    (define result 
      (apply (cdr op)
             (for/list ([i (in-range from to)])
               (define num
                 (list->number
                  (for/list ([row data]
                             #:do [(define n (string-ref row i))]
                             #:unless (char=? n #\space))
                    (c->i n))))
               num)))
    result))


(time (part2 input))
;; cpu time: 1 real time: 1 gc time: 0
