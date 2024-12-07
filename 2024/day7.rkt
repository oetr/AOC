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

;; part
(module+ main
  (define total-calibration-results
    (for/sum ([test (in-list tests)]
              [w (in-list weights)]
              #:when
              (ormap (lambda (ops)
                       (sat? test w ops))
                     (make-ops (sub1 (length w))
                               (list + *))))
      test)))

;; part2
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
               #:when
               (ormap (lambda (ops)
                        (sat? test w ops))
                      (make-ops (sub1 (length w))
                                ;; add the concat operator to the list of operators
                                (list + * concat))))
       ;; show progress
       (when (zero? (modulo i 10))
         (printf "~a\n" i))
       test))))

