#lang racket

(require advent-of-code
         threading
         racket/draw)

(define input (fetch-aoc-input (find-session) 2024 14 #:cache #true))

(struct robot (x y vx vy) #:transparent)

(define robots
  (for/list [(line (string-split input "\n"))]
    (~> (cdr (regexp-match #rx"p=([-0-9]+),([-0-9]+) v=([-0-9]+),([-0-9]+)" line))
        (map string->number _)
        (apply robot _))))

(define (robot-move a-robot seconds w h)
  (define vx (robot-vx a-robot))
  (define vy (robot-vy a-robot))
  (robot (modulo (+ (robot-x a-robot)
                    (* seconds vx))
                 w)
         (modulo (+ (robot-y a-robot)
                    (* seconds vy))
                 h)
         vx vy))

(define (robots-move robots seconds w h)
  (for/list ([r robots])
    (robot-move r seconds w h)))

(define (get-quadrant r w h)
  (define w-half (/ (- w 1) 2))
  (define h-half (/ (- h 1) 2))
  (define x (robot-x r))
  (define y (robot-y r))
  (match (list (< x w-half) (< y h-half))
    [(list #t #t) 0]
    [(list #f #t) 1]
    [(list #t #f) 2]
    [(list #f #f) 3]))

(define (has-quadrant? r w h)
  (and (not (= (robot-x r) (/ (- w 1) 2)))
       (not (= (robot-y r) (/ (- h 1) 2)))))

(define W 101)
(define H 103)

;; part 1
(~> (robots-move robots 100 W H)
    (filter (lambda (r) (has-quadrant? r W H)) _)
    (group-by (lambda (r) (get-quadrant r W H)) _)
    (map length _)
    (apply * _))

;; part 2
(define (robots-draw robots w h)
  (define bm (make-object bitmap% w h))
  (define dc (new bitmap-dc% [bitmap bm]))
  (define robot-color (make-object color% 0 0 255 1.0))
  (for ([r robots])
    (send dc set-pixel (robot-x r) (robot-y r) robot-color))
  bm)

;; look for a long sequence of robots beside each other
(define interesting 
  (for/fold ([interesting '()]) ([i 10000])
    (define grid 
      (for/vector ([_ H]) (for/vector ([_ W]) #\.)))
    (define moved-robots (robots-move robots i W H))
    (for ([r moved-robots])
      (vector-set! (vector-ref grid (robot-y r))
                   (robot-x r)
                   #\0))
    (define has-many-consequtive?
      (for/or ([row grid])
        (~> row
            vector->list
            list->string
            (regexp-match #rx"0000000000" _))))
    (if has-many-consequtive?
        (cons (cons i (robots-draw moved-robots W H)) interesting)
        interesting)))

interesting

;; (list
;;  (cons
;;   ;; redacted
;;    ))
