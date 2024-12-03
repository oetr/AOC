#lang racket

(define input (file->string "inputs/day3.txt"))

;; part 1
(define data
  (for/list ([mul (regexp-match* #px"mul\\([[:digit:]]+,[[:digit:]]+\\)" input)])
    (map string->number (string-split (substring mul 4 (- (string-length mul) 1)) ","))))

(apply + (map (lambda (a) (apply * a)) data))


