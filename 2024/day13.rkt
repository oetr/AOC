#lang rosette ;; using z3

(require advent-of-code)

(define input (fetch-aoc-input (find-session) 2024 13 #:cache #true))

(struct claw (ax ay bx by x y) #:transparent)

(define data
  (for/list [(claw-data (string-split input "\n\n"))]
    (apply claw
           (map string->number
                (cdr (regexp-match #rx".*?([0-9]+).*?([0-9]+).*?([0-9]+).*?([0-9]+).*?([0-9]+).*?([0-9]+)"
                                   claw-data))))))

(define (cost A B)
  (+ (* A 3) B))

(define (evaluate* sol . vars)
  (apply values (for/list ([v vars])
                  (evaluate v sol))))

(define (solve-one a-claw (add 0))
  (define-symbolic A? B? integer?)
  (solver-assert (current-solver)
                 (list (&& (= (+ (* A? (claw-ax a-claw))
                                 (* B? (claw-bx a-claw)))
                              (+ add (claw-x a-claw)))
                           (= (+ (* A? (claw-ay a-claw))
                                 (* B? (claw-by a-claw)))
                              (+ add (claw-y a-claw))))))
  (define sol (solver-check (current-solver)))

  (define results
    (let loop [(sol sol) (results '())]
      (cond
        [(sat? sol)
         (define-values (a b) (evaluate* sol A? B?))
         (solver-assert (current-solver)
                        ;; add a constraint that removes the current solution
                        ;; as it turns out this is not necessary, because for the claws given
                        ;; there is only one possible way to reach the prize
                        (list (not (and (= A? a) (= B? b)))))
         (loop (solver-check (current-solver))
               (cons (list a b (cost a b)) results))]
        [else results])))
  (solver-clear (current-solver))
  results)

(define (win-all data (add 0))
  (for/sum ([possible-ways (filter-not empty?
                                       (for/list ([a-claw data])
                                         (solve-one a-claw add)))])
    ;; it seems there is always only one solution, so using min is unnecessary
    (apply min (for/list ([way possible-ways])
                 (list-ref way 2)))))

;; part 1
(win-all data)

;; part 2
(win-all data 10000000000000)

