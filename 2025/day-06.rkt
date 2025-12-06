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
                  (vector-ref (vector-ref data row) col))))
  (for/sum ([n result]) n))

(time (part1 input))
;; cpu time: 7 real time: 7 gc time: 5

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (character->digit char)
  (- (char->integer char) (char->integer #\0)))

(define (digits->number digit-list)
  (for/fold ([result 0])
            ([digit digit-list])
    (+ (* result 10) digit)))

(define (char->operator chr)
  (match chr
    [#\+ +]
    [#\* *]
    [_ (error 'char->operator "unknown operator~a~n" chr)]))

(define (column-number col data)
  (digits->number
   (for/list ([row data]
              #:do [(define n (string-ref row col))]
              #:unless (char=? n #\space))
     (character->digit n))))

(define (extract-ranges offsets (inclusive-last-offset #f))
  (define to-offsets
    (if inclusive-last-offset
        (append (cdr offsets)
                (list (+ 1 inclusive-last-offset)))
        (cdr offsets)))
  (for/list ([from offsets]
             [to to-offsets])
    (cons from (sub1 to))))

(define (part2 input)
  (define lines (string-split input "\n"))
  (define num-cols (string-length (car lines)))
  (define data (drop-right lines 1))
  
  ;; extract operators and their offsets from last line
  (define-values (ops offsets)
    (for/fold ([ops empty]
               [offsets empty]
               #:result (values (reverse ops) (reverse offsets)))
              ([(op i) (in-indexed (last lines))]
               #:unless (char=? op #\space))
      (values (cons (char->operator op) ops)
              (cons i offsets))))
  
  (define ranges (extract-ranges offsets num-cols))
  
  (for/sum ([op ops]
            [range ranges])
    (define from (car range))
    (define to (cdr range))
    (define numbers
      (for/list ([offset (in-range from to)])
        (column-number offset data)))

    (apply op numbers)))

(time (part2 input))
;; cpu time: 1 real time: 1 gc time: 0
