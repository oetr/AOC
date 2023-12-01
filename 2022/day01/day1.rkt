#lang racket

(define-values
  (result _)
  (for/fold ([result '()]
             [calories-per-elf 0])
            ([line (file->lines "day1.txt")]) ;; added an extra empty line here
    (if (string=? line "")
        (values (cons calories-per-elf result) 0)
        (values result (+ calories-per-elf (string->number line))))))

(car (sort result >)) ;; part 1
(apply + (take (sort result >) 3)) ;; part 2
