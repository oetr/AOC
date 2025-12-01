#lang racket

(require advent-of-code)

(define input (fetch-aoc-input (find-session) 2025 1 #:cache #true))

;; (define input "L68
;; L30
;; R48
;; L5
;; R60
;; L55
;; L1
;; L99
;; R14
;; L82")

(define data
  (for/list ([a-match (regexp-match* #rx"([L|R])([0-9]+)" input #:match-select rest)])
    (define n (string->number (second a-match)))
    (if (string=? "L" (first a-match))
        (- n)
        n)))

(define (rotate* instructions)
  (for/fold ([n-clicks 0]
             [dial 50]
             #:result n-clicks)
            ([instr instructions])
    (define new-dial (modulo (+ dial instr) 100))
    (values (+ n-clicks (if (zero? new-dial) 1 0))
            (if (< new-dial 0)
                (+ 100 new-dial)
                new-dial))))

;; part 1
(rotate* data)

(define (rotate-count-all-clicks* instructions)
  (for/fold ([n-clicks 0]
             [dial 50]
             #:result n-clicks)
            ([instr instructions])
    (when (zero? instr)
      (error 'rotate-count-all-clicks* "expected non-zero rotation!"))
    (define-values (potential-clicks potential-new-dial) (quotient/remainder (+ dial instr) 100))
    (define new-clicks
      (+ (abs potential-clicks)
         ;; landed on 0 when dialing Left
         (if (or (and (zero? potential-new-dial) (< instr 0))
                 ;; passed 0 after dialing Left
                 (and (< potential-new-dial 0) (> dial 0)))
             1
             0)))
    (values (+ n-clicks new-clicks)
            (if (< potential-new-dial 0)
                (+ 100 potential-new-dial)
                potential-new-dial))))

;; part 2
(rotate-count-all-clicks* data)
