#lang racket


(define input (file->lines "inputs/day4.txt"))

;; part 1, really not proud about this :) 
;; convert top list to vector
(define data
  (for/vector ([s input])
    s))

(define (eq-XMAS? d)
  (and (string? d) (string=? d "XMAS")))

(define XMASLEN (string-length "XMAS"))


;; search for MAS
(define (find-all-XMAS-ses data)
  (define result 0)
  (for ([line data]
        [line-i (sequence-length data)])
    (for ([char line]
          [i (string-length line)])
      
      (define right (when (> (string-length line) (+ i XMASLEN -1))
                      (substring line i (+ i XMASLEN))))
      (define left (when (>= i (- XMASLEN 1))
                     (list->string (for/list ([j (range i (- i XMASLEN) -1)])
                                     (string-ref line j)))))
      (define down (when (> (sequence-length data) (+ line-i XMASLEN -1))
                     (list->string (for/list ([j (range line-i (+ line-i XMASLEN))])
                                     (string-ref (vector-ref data j) i)))))
      (define up (when (>= line-i (- XMASLEN 1))
                   (list->string (for/list ([j (range line-i (- line-i XMASLEN) -1)])
                                   (string-ref (vector-ref data j) i)))))
      (define up-left (when (and (>= line-i (- XMASLEN 1))
                                 (>= i (- XMASLEN 1)))
                        (list->string (for/list ([x (range i (- i XMASLEN) -1)]
                                                 [y (range line-i (- line-i XMASLEN) -1)])
                                        (string-ref (vector-ref data y) x)))))
      (define up-right (when (and (>= line-i (- XMASLEN 1))
                                  (> (string-length line) (+ i XMASLEN -1)))
                         (list->string (for/list ([x (range i (+ i XMASLEN))]
                                                  [y (range line-i (- line-i XMASLEN) -1)])
                                         (string-ref (vector-ref data y) x)))))
      (define down-left (when (and (> (sequence-length data) (+ line-i XMASLEN -1))
                                   (>= i (- XMASLEN 1)))
                          (list->string (for/list ([x (range i (- i XMASLEN) -1)]
                                                   [y (range line-i (+ line-i XMASLEN))])
                                          (string-ref (vector-ref data y) x)))))
      (define down-right (when (and (> (sequence-length data) (+ line-i XMASLEN -1))
                                    (> (string-length line) (+ i XMASLEN -1)))
                          (list->string (for/list ([x (range i (+ i XMASLEN))]
                                                   [y (range line-i (+ line-i XMASLEN))])
                                          (string-ref (vector-ref data y) x)))))
      
      (when (eq-XMAS? right)
        (set! result (+ result 1)))
      (when (eq-XMAS? left)
        (set! result (+ result 1)))
      (when (eq-XMAS? down)
        (set! result (+ result 1)))
      (when (eq-XMAS? up)
        (set! result (+ result 1)))
      (when (eq-XMAS? up-left)
        (set! result (+ result 1)))
      (when (eq-XMAS? up-right)
        (set! result (+ result 1)))
      (when (eq-XMAS? down-left)
        (set! result (+ result 1)))
      (when (eq-XMAS? down-right)
        (set! result (+ result 1)))
      ))
  result)


(find-all-XMAS-ses data)

;; part 2
(define cases
  (list
   "MSAMS"
   "SSAMM"
   "MMASS"
   "SMASM"))

(define (find2 data)
  (define result 0)
  (for ([line data]
        [line1 (cdr data)]
        [line2 (cddr data)])
    (for ([c1 line]
          [c2 (substring line 2)]
          [c3 (substring line1 1)]
          [c4 line2]
          [c5 (substring line2 2)])
      (define maybe-xmas (list->string (list c1 c2 c3 c4 c5)))
      (when (for/or ([a-case cases])
              (string=? a-case maybe-xmas))
        (set! result (+ result 1)))))
  result)

(find2 input)
