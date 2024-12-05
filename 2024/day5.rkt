#lang racket

(define input1 (file->string "inputs/day5.txt"))

(define part1-test
  "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47")

(define (parse-input1 input)
  (let ([split (string-split input "\n\n")])
    (values (for/fold ([h (make-hash)])
                      ([rule (string-split (car split) "\n")])
              (define v1v2 (map string->number (string-split rule "|")))
              (define left (car v1v2))
              (define right (cadr v1v2))
              (hash-set! h left (cons right (hash-ref h left '())))
              h)
            (let ([prints (string-split (cadr split) "\n")])
              (map (lambda (p)
                     (map string->number (string-split p ",")))
                   prints)))))

(define-values (rules prints) (parse-input1 input1))

(define (correct-order? p)
  (for/and ([n1 p]
            [i (length p)])
    (define rulz (hash-ref rules n1 '()))
    (for/and ([n2 (drop p (+ i 1))])
      (member n2 rulz))))

(apply +
       (for/list [(correct (filter correct-order? prints))]
         (list-ref correct (/ (- (length correct) 1) 2))))

