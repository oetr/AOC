#lang racket

(require advent-of-code
         threading)

;; unused here
(define input (fetch-aoc-input (find-session) 2024 17 #:cache #true))

(define (bin n)
  (number->string n 2))

(define DEBUGGING #t)
(define (p step A B C)
  (when DEBUGGING
    (printf "~a:  A: ~a B: ~a C: ~a~n" step (bin A) (bin B) (bin C))))

;; the program is short, we convert it by hand
(define (run A-initial)
  (define A A-initial)
  (define B 0)
  (define C 0)
  (p 0 A B C)
  (let loop ([out '()])
    (set! B (modulo A 8))
    (p 1 A B C)
    (set! B (bitwise-xor B 7))
    (p 2 A B C)
    (set! C (arithmetic-shift A (- B)))
    (p 3 A B C)
    (set! A (arithmetic-shift A -3))
    (p 4 A B C)
    (set! B (bitwise-xor B C))
    (p 5 A B C)
    (set! B (bitwise-xor B 7))
    (p 6 A B C)
    (define new-out (cons (modulo B 8) out))
    (printf "out: ~a ~a ~n" (modulo B 8) (bin (modulo B 8)))
    (cond [(zero? A) (reverse new-out)]
          [else (loop new-out)])))

;; part 1
;;(apply (curry ~a #:separator ",") (run 62769524))
;; 2,1,...

;; part 2
;; using pen and paper for this one :(

;; expected
;; 2,4,1,7,7,5,0,3,4,0,1,7,5,5,3,0
;; 8 64 (expt 8 16) (expt 8 17)
;;(define from (- 2251799813685248 (truncate (/ 2251799813685248 10))))
(define from 0)
;; (for ([i (in-range from (+ 10000 from))])
;;   (define out (run i))
;;   (match out
;;     [(list head ... 5 5 3 0) (printf "~a * ~n" (list i (bin i) out))]
;;     [else (printf "~a~n" (list i (bin i) out))]))

;; 0
000 0
111 7

;; 3 0
111010 58
111100 60

(run 3857)
(run 3862)
#b111

;;(3857 111100010001 (5 5 3 0)) * 
;;(3862 111100010110 (5 5 3 0)) * 
;;(3865 111100011001 (5 5 3 0)) * 
;;(3889 111100110001 (5 5 3 0)) * 
;;(3894 111100110110 (5 5 3 0)) *

(define (append-bits n bits)
  (bitwise-ior (arithmetic-shift n (integer-length n))
               bits))

;; search from the back
(define target (list 2 4 1 7 7 5 0 3 4 0 1 7 5 5 3 0))
(let loop ([head (take target (- (length target) 1))]
           [tail (drop target (- (length target) 1))]
           [candidates (list->set (range 8))])
  (printf "~a - ~a~n" head tail)
  
  (define new-candidates (mutable-set))
  ;; append a few bits and note all outputs that produce tail
  (for ([candidate candidates])
    (for ([bits 8])
      (define new-n (append-bits candidate bits))
      (when (equal? (run new-n) tail)
        (set-add! new-candidates new-n))))
  (cond [(empty? head ) candidates]
        [else (loop (take head (- (length head) 1))
                    (cons (last head) tail)
                    new-candidates)]))
              
