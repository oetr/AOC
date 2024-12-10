#lang racket

(require advent-of-code)

(define input (fetch-aoc-input (find-session) 2024 10 #:cache #true))
;; (define input
;; "89010123
;; 78121874
;; 87430965
;; 96549874
;; 45678903
;; 32019012
;; 01329801
;; 10456732")

(define (input->data input)
  (for/vector [(s (string-split input))]
    (for/vector ([c s])
      (- (char->integer c) (char->integer #\0)))))

(define (where m n)
  (for*/list ([(row row-i) (in-indexed m)]
              [(col col-i) (in-indexed row)]
              #:when (= col n))
    (cons row-i col-i)))

(define (get-sets m)
  (for/list ([i 10])
    (list->set (where data i))))

(define (adjacent? p1 p2)
  (or
   (and (= 1 (abs (- (car p1) (car p2))))
        (= (cdr p1) (cdr p2)))
   (and (= (car p1) (car p2))
        (= 1 (abs (- (cdr p1) (cdr p2)))))))

(module+ test
  (require rackunit)
  (check-true (adjacent? (cons 10 10) (cons 10 11)))
  (check-true (adjacent? (cons 10 10) (cons 11 10)))
  (check-true (adjacent? (cons 10 10) (cons 10 9)))
  (check-true (adjacent? (cons 10 10) (cons 9 10)))
  (check-false (adjacent? (cons 10 10) (cons 11 11)))
  (check-false (adjacent? (cons 10 10) (cons 9 9)))
  (check-false (adjacent? (cons 10 10) (cons 9 11)))
  (check-false (adjacent? (cons 10 10) (cons 11 9)))
  (check-false (adjacent? (cons 10 10) (cons 10 20))))

(define data (input->data input))
(define sets (get-sets data))

(define forward-diff
  (for/list ([s1 sets]
             [s2 (append (cdr sets) (list (list-ref sets (- (length sets) 2))))])
    (for*/set ([p1 s1]
               [p2 s2]
               #:when (adjacent? p1 p2))
      p1)))

(define (get-paths forward)
  (define data (reverse forward))
  (let loop ([data (cdr data)][acc (map list (set->list (car data)))])
    (cond ([empty? data] acc)
          (else
           (loop (cdr data)
                 (for*/list ([path acc]
                             [p2 (car data)]
                             #:when (adjacent? (car path) p2))
                   (cons p2 path)))))))

(define (get-score forward)
  (hash-count
   (for/fold ([h (make-immutable-hash)]) ([p (get-paths forward)])
     (hash-set h (cons (first p)
                       (last p))
               (cons p (hash-ref h p '()))))))

;; part 1
(get-score forward-diff)

;; part 2
(length (get-paths forward-diff))
