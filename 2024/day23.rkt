#lang racket

(require advent-of-code)

(define input (fetch-aoc-input (find-session) 2024 23 #:cache #true))

(define connections
  (for/fold ([h (hash)]) ([connection (string-split input)])
    (define a-b (cdr (regexp-match #px"(.+)-(.+)" connection)))
    (define a (car a-b))
    (define b (cadr a-b))
    (hash-update (hash-update h a (curry cons b) '())
                 b
                 (curry cons a) '())))

;; part 1
(define (part1 connections)
  (define combs (combinations (hash-keys connections) 3))
  ;; remove not interconnected
  (define (interconnected? a b c h)
    (define ca (hash-ref h a))
    (define cc (hash-ref h c))
    (and (member b ca)
         (member c ca)
         (member b cc)))

  (define result
    (for/list ([c combs]
               #:when (and (apply interconnected? (append c (list connections)))
                           (or (string-prefix? (car c) "t")
                               (string-prefix? (cadr c) "t")
                               (string-prefix? (caddr c) "t"))))
      c))
  (length result))

(time (part1 connections))
;; cpu time: 19227 real time: 19229 gc time: 9199


;; part 2
;; need to check only one direction, since connections are always symmetrical
(define (connected? h a b)
  (member b (hash-ref h a)))

(define (find-largest-sets connections)
  (define (aux current servers)
    (cond [(empty? servers) current]
          [else
           (define server (car servers))
           (if (andmap (curry connected? connections server) current)
               (let ([with-server (aux (cons server current) (cdr servers))]
                     [without-server (aux current (cdr servers))])
                 (if (> (set-count with-server)
                        (set-count without-server))
                     with-server
                     without-server))
               (aux current (cdr servers)))]))
  (aux '() (hash-keys connections)))

(define result2 (time (find-largest-sets connections)))
;; cpu time: 12392 real time: 12392 gc time: 94
(apply (curry ~a #:separator ",") (sort result2 string<?))
