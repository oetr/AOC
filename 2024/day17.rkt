#lang rosette

(require rackunit)

;; the program is short, we convert it by hand
(define (run A-initial)
  (define A A-initial)
  (define B 0)
  (define C 0)
  (let loop ([out '()])
    (set! B (modulo A 8))
    (set! B (bitwise-xor B 7))
    (set! C (arithmetic-shift A (- B)))
    (set! A (arithmetic-shift A -3))
    (set! B (bitwise-xor B C))
    (set! B (bitwise-xor B 7))
    (define new-out (cons (modulo B 8) out))
    (cond [(zero? A) (reverse new-out)]
          [else (loop new-out)])))

;; part1
(run 62769524)

;; part 2
(define (symbolic-run A-initial expected width)
  (define A A-initial)
  (define B (bv 0 width))
  (define C (bv 0 width))
  (define mask (bv 7 width))
  (for/list ([v expected]) ;; hard-coded number of executions
    (set! B (bvsmod A (bv 8 width)))
    (set! B (bvxor B mask))
    (set! C (bvlshr A B))
    (set! A (bvlshr A (bv 3 width)))
    (set! B (bvxor B C))
    (set! B (bvxor B mask))
    (extract 2 0 B)))

(define expected (list 2 4 1 7 7 5 0 3 4 0 1 7 5 5 3 0))
(define-symbolic* A (bitvector 48))
(define vals (symbolic-run A expected 48))

(solver-assert (current-solver)
               (list (apply && (for/list ([v vals]
                          [expected-v expected])
                 (bveq (bv expected-v 3) v)))))

(define sol (solver-check (current-solver)))
(define quine-A (bitvector->natural (evaluate A sol)))
(check-equal? expected (run quine-A))
