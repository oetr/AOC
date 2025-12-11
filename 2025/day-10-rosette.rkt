#lang rosette

(require advent-of-code)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2 only
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define test-input "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}")

(define input (fetch-aoc-input (find-session) 2025 10 #:cache #true))

(struct machine (indicator buttons requirements) #:transparent)

(define (parse-indicator str)
  (for/vector ([chr (substring str 1 (sub1 (string-length str)))])
    (char=? #\# chr)))

(define (parse-buttons buttons)
  (for/list ([str buttons])
    (for/list ([digit (string-split (substring str 1 (sub1 (string-length str))) ",")])
      (string->number digit))))

(define (parse-requirements str)
  (for/vector ([digit (string-split (substring str 1 (sub1 (string-length str))) ",")])
    (string->number digit)))

(define (parse-input input)
  (for/list ([line (string-split input "\n")])
    (define data (string-split line " "))
    (machine (parse-indicator (car data))
             (parse-buttons (drop-right (cdr data) 1))
             (parse-requirements (last data)))))

(define (button->bitmap button len)
  (for/fold ([result (make-list len 0)])
            ([idx button])
    (list-set result idx 1)))

(define (bitmaps->matrix bitmaps)
  (apply map list bitmaps))



(define (make-int)
  (define-symbolic* int integer?)
  int)


(define (machine->matrix machine)
  (define n-joltages (sequence-length (machine-indicator machine)))
  (define bitmaps (for/list ([button (machine-buttons machine)])
                    (button->bitmap button n-joltages)))
  (define mat (bitmaps->matrix bitmaps))
  (define vars (for/list ([_ (length (machine-buttons machine))])
                 (make-int)))
  (define equations
    (for/list ([m mat]
               [req (machine-requirements machine)])
      (= req (for/fold ([result 0])
                       ([bit m]
                        [var vars])
               (+ result (* bit var))))))
  (define eqs (for/fold ([equations equations])
                        ([var vars])
                (cons (>= var 0) equations)))
  (define-symbolic* sum integer?)
  (define sum>0 (> sum 0))
  (solver-assert (current-solver)
                 (cons sum>0
                       (cons
                        (= sum
                           (for/fold ([sum 0]) ([var vars])
                             (+ sum var)))
                        eqs)))
  sum)

(define (part2 input)
  (define machines (parse-input input))
  (for/sum ([machine machines])
    (solver-clear (current-solver))
    (define sum (machine->matrix machine))
    ;; minimize until the sum doesn't change
    (let loop ([initial (evaluate sum (solver-check (current-solver)))]
               ;; weird, I would expect the minimizer to find the minimum sum right away
               ;; but it needs at least two times to get the right sum
               [times 1])
      (solver-minimize (current-solver) (list sum))
      (define after (evaluate sum (solver-check (current-solver))))
      (cond
        [(> times 0) (loop after (- times 1))]
        [else after]))))

(time (part2 input))
;; cpu time: 264 real time: 1502 gc time: 37

;; 17214 --- wrong answer (by mistake, I solved the same machine 1000 times!)
;; 15019 -- too high
;; 15017 -- correct after using solver-minimize twice!
