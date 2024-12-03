#lang racket

(define input (file->string "inputs/day3.txt"))

;; part 1
(define data
  (for/list ([mul (regexp-match* #px"mul\\([[:digit:]]+,[[:digit:]]+\\)" input)])
    (map string->number (string-split (substring mul 4 (- (string-length mul) 1)) ","))))

(apply + (map (lambda (a) (apply * a)) data))


;; part 2
(define (interpret-mul str)
  (map string->number (string-split (substring str 4 (- (string-length str) 1)) ",")))

(define data2
  (let loop ([instructions (regexp-match* #px"mul\\([[:digit:]]+,[[:digit:]]+\\)|do\\(\\)|don't\\(\\)" input)]
             [valid? #t]
             [result '()])
    (cond
      [(empty? instructions) (apply + (map (lambda (a) (apply * a)) result))]
      [else
       (define instr (car instructions))
       (cond
         [(string=? "do()" instr) (loop (cdr instructions) #t result)]
         [(string=? "don't()" instr) (loop (cdr instructions) #f result)]
         [valid? (loop (cdr instructions) valid? (cons (interpret-mul instr) result))]
         [else (loop (cdr instructions) valid? result)])])))
