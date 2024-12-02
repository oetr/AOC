#lang racket

;; read data
(define input (file->lines "inputs/day1.txt"))

(define-values (numbers1 numbers2) 
  (let loop ([input input] [l1 '()] [l2 '()])
    (cond
      [(null? input) (values l1 l2)]
      [else
       (define ns (string-split (car input)))
       (define n1 (string->number (car ns)))
       (define n2 (string->number (cadr ns)))
       (loop (cdr input) (cons n1 l1) (cons n2 l2))])))

;; part 1
(define sorted1 (sort numbers1 <))
(define sorted2 (sort numbers2 <))

(apply + (map (lambda (n1 n2) (abs (- n1 n2))) sorted1 sorted2))

;; part 2
(define h1 (make-hash))

(for ([n numbers2])
  (hash-set! h1 n (+ (hash-ref h1 n 0) 1)))

;; compute result
(for/fold ([result 0])
          ([n numbers1])
  (+ result (* n (hash-ref h1 n 0))))
