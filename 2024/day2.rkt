#lang racket

(define inputs (file->lines "inputs/day2.txt"))

(define reports (for/list ([raw-levels inputs])
                  (map string->number (string-split raw-levels))))

;; part 1
(define (compute-direction a b)
  (cond [(> a b) 'decreasing]
        [(< a b) 'increasing]
        [else 'na]))

(define (safe? levels)
  (define (acc direction previous todo)
    (cond
      [(empty? todo) #t]
      [else
       (let* ([current (car todo)]
             [diff (- current previous)])
       (cond
         ;; levels within allowed ranges
         [(zero? diff) #f]
         [(> (abs diff) 3) #f]
         ;; direction is correct
         [(symbol=? direction 'start)
          (acc (compute-direction previous current)
               current (cdr todo))]
         [(symbol=? direction (compute-direction previous current))
          (acc direction current (cdr todo))]
         [else #f]))]))
  (acc 'start (car levels) (cdr levels)))

(length (filter safe? reports))


;; part 2
(define-values (safe unsafe) (partition safe? reports))

(define (drop-one lst idx)
  (for/list ([l lst]
             [i (length lst)]
             #:unless (= i idx))
    l))
            
;; brute force unsafe levels
(define unsafe-after-error-correction
  (for/list ([levels unsafe])
    (for/or ([i (length levels)])
      (safe? (drop-one levels i)))))

(+ (length safe)
   (length (filter identity unsafe-after-error-correction)))



