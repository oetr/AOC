#lang racket

(require advent-of-code
         data/union-find)

(define test-input "162,817,812
57,618,57
906,360,560
592,479,940
352,342,300
466,668,158
542,29,236
431,825,988
739,650,466
52,470,668
216,146,977
819,987,18
117,168,530
805,96,715
346,949,466
970,615,88
941,993,340
862,61,35
984,92,344
425,690,689")

(define input (fetch-aoc-input (find-session) 2025 8 #:cache #true))

(define (parse-input input)
  (for/vector ([line (string-split input)])
    (for/vector ([str-num (string-split line ",")])
      (string->number str-num))))

(define (distance v1 v2)
  (sqrt (for/sum ([c1 v1]
                  [c2 v2])
          (sqr (- c2 c1)))))

(struct distance-entry (id1 id2 distance) #:transparent)

(define (get-all-distances points)
  (sort
   (for*/list ([(p1 i) (in-indexed points)]
               [j (in-range (+ i 1) (vector-length points))])
     (define p2 (vector-ref points j))
     (distance-entry i j (distance p1 p2)))
   (lambda (a b)
     (< (distance-entry-distance a)
        (distance-entry-distance b)))))

(define (part1 input n (part2? #f))
  (define points (parse-input input))
  (define len (vector-length points))
  (define distances (get-all-distances points))
  (define circuits
    (for/vector ([i (vector-length points)])
      (uf-new i)))

  (define part2
    (let loop ([distances distances]
               [connected (set)]
               [n n])
      (define distance (car distances))
      (define id1 (distance-entry-id1 distance))
      (define id2 (distance-entry-id2 distance))
      (define dist (distance-entry-distance distance))
      (define circuit1 (vector-ref circuits id1))
      (define circuit2 (vector-ref circuits id2))
      (cond
        [(and (not part2?) (= n 1)) 'DONE]
        [(= len (set-count connected)) ]
        [(uf-same-set? circuit1 circuit2) ;; skipping, remove sub1 in the test-input
         (loop (cdr distances) connected (sub1 n))]
        [else
         (define new-set (set-add (set-add connected id1) id2))
         (if (= len (set-count new-set))
             (* (vector-ref (vector-ref points id1) 0)
                (vector-ref (vector-ref points id2) 0))
             (begin
               (uf-union! circuit1 circuit2)
               (loop (cdr distances) new-set (sub1 n))))])))

  (if part2?
      part2
      (sort (group-by identity
                      (for/list ([circuit circuits])
                        (uf-find circuit)))
            (lambda (a b) (> (length a) (length b))))))

(time (let ([groups (part1 input 1000)])
        (apply * (map length (take groups 3)))))
;; cpu time: 418 real time: 418 gc time: 50

;; 4480 - wrong
;; 123420 - Seriously WTF? In the actual task, we need to count skipped connections, but in the example,
;; we don't. This is BS.

(time (part1 input -1 #t))
;; cpu time: 422 real time: 422 gc time: 57
