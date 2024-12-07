#lang racket

(require advent-of-code
         threading)

(module+ test
  (require rackunit))

(define input (fetch-aoc-input (find-session) 2024 07 #:cache #true))

(define-values (tests weights)
  (for/fold ([tests '()][weights '()])
            ([line (string-split input "\n")])
    (define-values (t w)
      (let [(tv (string-split line ":"))]
        (values (car tv) (cadr tv))))
    (values (cons (string->number t) tests)
            (cons (~> w
                      string-trim
                      string-split
                      (map string->number _))
                  weights))))

(define (sat? test weights ops)
  (define result (for/fold ([result (car weights)])
                           ([w (in-list (cdr weights))]
                            [op (in-list ops)]
                            ;; if the current result is already greater, there's no need to continue
                            #:break (> result test))
                   (op result w)))
  (= test result))

(define (sat*? test weights possible-ops)
  (define all-ops (make-ops (sub1 (length weights))
                            possible-ops))
  (for/or [(ops (in-list all-ops))]
    (sat? test weights ops)))

(define (make-ops len lst)
  (for/fold ([acc '(())])
            ([_ len])
    (for*/list ([prefix (in-list lst)]
                [suffix (in-list acc)])
      (cons prefix suffix))))

(module+ test
  (test-equal? "make ops 2"
               (make-ops 2 '(0 1))
               '((0 0) (0 1) (1 0) (1 1))))

;; part 1
(module+ main
  (define total-calibration-results
    (for/sum ([test (in-list tests)]
              [w (in-list weights)]
              #:when (sat*? test w (list + *)))
      test)))

;; part 2
(define (concat a b)
  (string->number
   (string-append (number->string a)
                  (number->string b))))

(module+ main
  (define total-calibration-results2
    (time
     (for/sum ([test (in-list tests)]
               [w (in-list weights)]
               [i (length tests)]
               #:when (sat*? test w (list + * concat)))
       ;; show progress
       (when (zero? (modulo i 10))
         (printf "~a\n" i))
       test))))

