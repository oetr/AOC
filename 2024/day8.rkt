#lang racket

(require advent-of-code
         threading)

(define input (fetch-aoc-input (find-session) 2024 08 #:cache #true))

(define data (~> input
                 string-split))
(define COLS (string-length (car data)))
(define ROWS (length data))

(define (hash-update h index val)
  (hash-set h index (cons val (hash-ref h index '()))))

(define antennae-coordinates
  (for*/fold ([h (make-immutable-hash)])
             ([(line row) (in-indexed data)]
              [(ch col) (in-indexed line)])
    (if (char=? ch #\.)
        h
        (hash-update h (char->integer ch) (cons row col)))))

(define (point-delta p1 p2)
  (cons (- (car p1) (car p2))
        (- (cdr p1) (cdr p2))))

(define (point+ p1 p2)
  (cons 
   (+ (car p1) (car p2))
   (+ (cdr p1) (cdr p2))))

(define (point- p1 p2)
  (cons 
   (- (car p1) (car p2))
   (- (cdr p1) (cdr p2))))

(define (inside-the-map? coord rows cols)
  (and (>= (car coord) 0)
       (< (car coord) rows)
       (>= (cdr coord) 0)
       (< (cdr coord) cols)))

(define (add-antinodes! p1 p2 antinodes)
  (define delta (point-delta p2 p1))
  (define new1 (point+ p2 delta))
  (define new2 (point- p1 delta))
  (when (inside-the-map? new1 ROWS COLS) (set-add! antinodes new1))
  (when (inside-the-map? new2 ROWS COLS) (set-add! antinodes new2)))

(define (part1)
  (define antinodes (mutable-set))

  (for ([(_ coordinates) (in-hash antennae-coordinates)])
    (for ([pair (combinations coordinates 2)])
      (add-antinodes! (car pair) (cadr pair) antinodes)))

  (set-count antinodes))

(part1)

;; part 2
(define (add-antinodes2! p1 p2 antinodes)
  (define delta (point-delta p2 p1))

  (let loop ([p p1])
    (when (inside-the-map? p ROWS COLS)
      (set-add! antinodes p)
      (loop (point+ p delta))))

  (let loop ([p p2])
    (when (inside-the-map? p ROWS COLS)
      (set-add! antinodes p)
      (loop (point- p delta)))))

(define (part2)
  (define antinodes (mutable-set))

  (for ([(_ coordinates) (in-hash antennae-coordinates)])
    (for ([pair (combinations coordinates 2)])
      (add-antinodes2! (car pair) (cadr pair) antinodes)))

  (set-count antinodes))

(part2)
