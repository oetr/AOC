#lang racket

(require advent-of-code)

(define input (fetch-aoc-input (find-session) 2025 5 #:cache #true))

(define test-input "3-5
10-14
16-20
12-18

1
5
8
11
17
32")

(define (parse-input input)
  (define ranges-and-ids (string-split input "\n\n"))
  (define ranges
    (for/list ([a-range (string-split (car ranges-and-ids))])
      (match (map string->number (string-split a-range "-"))
        [(list a b) (cons a b)])))
  (define ids
    (for/list ([n (string-split (cadr ranges-and-ids))])
      (string->number n)))
  (values ranges ids))

(define-values (ranges ids)
  (parse-input input))

(define-values (test-ranges test-ids)
  (parse-input test-input))

;; estimate number of hash entries
;; (for/sum ([r ranges])
;;   (- (cdr r) (car r)))

;; 447509460007074 --- too many!

(define (id-in-range? ranges id)
  (for/or ([a-range ranges])
    (and (>= id (car a-range))
         (<= id (cdr a-range)))))

;; part 1
(time (for/sum ([id ids]
                #:when (id-in-range? ranges id))
        1))
;; cpu time: 1 real time: 1 gc time: 0

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(struct node (from to)
  #:transparent
  #:mutable)

(define (node-eq? node1 node2)
  (and (= (node-from node1) (node-from node2))
       (= (node-to node1) (node-to node2))))

(define (in-range? num r)
  (and (>= num (node-from r))
       (<= num (node-to r))))

(define (ranges-overlap? node1 node2)
  (or (in-range? (node-from node1) node2)
      (in-range? (node-to node1) node2)
      (in-range? (node-from node2) node1)
      (in-range? (node-to node2) node1)))

;; merge both, update node1
(define (merge-ranges node1 node2)
  (unless (ranges-overlap? node1 node2)
    (error 'ranges-merge? ""))
  (define from (min (node-from node1) (node-from node2)))
  (define to (max (node-to node1) (node-to node2)))
  (node from to))

(define (find-first-mergable ranges)
  (let loop ([ranges ranges])
    (cond ([empty? ranges] empty)
          (else
           (define first (car ranges))
           (define second (for/first ([range (cdr ranges)]
                                      #:when (ranges-overlap? first range))
                            range))
           (if second
               (cons first second)
               (loop (cdr ranges)))))))

;; brute force, too lazy for trees
(define (find-ids* raw-ranges)
  ;; n^2 re-add everything until nothing merges anymore
  (define ranges (map (lambda (r) (node (car r) (cdr r)))
                      raw-ranges))
  (let loop ([ranges ranges])
    (define first-mergable (find-first-mergable ranges))
    (cond [(empty? first-mergable) ranges]
          [else
           ;; re-add everything but the two nodes who are now merged
           (define merged (merge-ranges (car first-mergable)
                                        (cdr first-mergable)))
           (loop (cons merged
                       (for/list ([range ranges]
                                  #:when (not (node-eq? range (car first-mergable)))
                                  #:when (not (node-eq? range (cdr first-mergable))))
                         range)))])))

(define (part2 input)
  (define-values (ranges ids)
    (parse-input input))
  (define merged (find-ids* ranges))
  (for/sum ([node merged])
    (add1 (- (node-to node) (node-from node)))))

(time (part2 input))
;; cpu time: 25 real time: 25 gc time: 0
