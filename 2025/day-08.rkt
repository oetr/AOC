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
  (for/sum ([c1 v1]
            [c2 v2])
    (sqr (- c2 c1))))

(define (best-candidates circuits todo)
  ;;(printf "-----------------------~a~n" connected)
  (for*/fold ([best1 -1]
              [best2 -1]
              [min-distance (expt 2 64)]
              [best-v1 empty]
              [best-v2 empty]
              #:result (begin
                         (printf "~a-~a   ~a~n" best1 best2 min-distance)
                         (values best1 best2 best-v1 best-v2)))
             ([(v1 i) (in-hash todo)]
              [(v2 j) (in-hash todo)]
              #:unless (= i j)
              #:unless (uf-same-set? (vector-ref circuits i)
                                     (vector-ref circuits j))
              )
    (define dist (distance v1 v2))
    (printf "~a - ~a: [~a]  ~a, ~a   ~a       ~a - ~a~n" i j dist best1 best2 min-distance v1 v2)
    (if (< dist min-distance)
        (values i j dist v1 v2)
        (values best1 best2 min-distance best-v1 best-v2))))

(define (part1 input iterations)
  (define data (parse-input input))
  (define todo (for/hash ([(d i) (in-indexed data)])
                 (values d i)))
  (define len (vector-length data))
  (define circuits
    (for/vector ([i (vector-length data)])
      (uf-new i)))

  (let loop ([n 0]
             [todo todo])
    (printf "~a~n" n)
    (cond
      [(or (= n iterations) (zero? (hash-count todo)))
       (apply * (map length (take (sort (group-by identity
                              (for/list ([circuit circuits])
                                (uf-find circuit)))
                   (lambda (a b) (> (length a) (length b))))
             3)))]
      [else
       (define-values (best1 best2 v1 v2) (best-candidates circuits todo))
       (uf-union! (vector-ref circuits best1)
                  (vector-ref circuits best2))
       (printf "~a~n"
               (take (sort (group-by identity
                              (for/list ([circuit circuits])
                                (uf-find circuit)))
                   (lambda (a b) (> (length a) (length b))))
             3))
       (loop (+ n 1) (hash-remove (hash-remove todo v1) v2))])))


(part1 test-input 10)

