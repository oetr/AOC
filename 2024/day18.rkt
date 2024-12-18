#lang racket

(require advent-of-code
         threading)


(define input (fetch-aoc-input (find-session) 2024 18 #:cache #true))
(define WIDTH 71)
(define HEIGHT 71)

(define grid (for/vector ([_ HEIGHT])
               (for/vector ([_ WIDTH])
                 'safe)))

(define (! grid x y v)
  (vector-set! (vector-ref grid y)
               x v))

(define (!p grid p v)
  (vector-set! (vector-ref grid (cadr p))
               (car p) v))

(define (&p grid p)
  (vector-ref (vector-ref grid (cadr p)) (car p)))

(define bytes (for/list ([line (string-split input "\n")])
                (match (~> (regexp-match #px"(\\d+),(\\d+)" line)
                           rest
                           (map string->number _)
                           )
                  [(list x y) (list x y)])))

(define (posn+ p1 p2)
  (map + p1 p2))

(define (posn= p1 p2)
  (andmap = p1 p2))

(define (in-grid? grid p)
  (define x (cadr p))
  (define y (car p))
  (and (> x -1)
       (> y -1)
       (< x (vector-length (vector-ref grid 0)))
       (< y (vector-length grid))))

;; part 1
(define (get-safe-adj grid p)
  (for/fold ([adj '()])
            ([p (map (curry posn+ p) '((0 1) (0 -1) (1 0) (-1 0)))])
    (cond [(in-grid? grid p)
           (define v (&p grid p))
           (if (symbol=? 'safe v)
               (cons p adj)
               adj)]
          [else adj])))

(struct state (cost parent posn))

(define (priority-queue-insert states st)
  (let loop ([head '()][states states])
    (cond [(empty? states) (append head (list st))]
          [(eq? (car states) st) (append head (cons st states))]
          [else (loop (append head (list (car states))) (cdr states))])))

(define (priority-queue-member? pq posn)
  (for/or ([state pq])
    (posn= (state-posn state) posn)))


(define (priority-queue-replace-if-better pq st)
  (define posn (state-posn st))
  (define cost (state-cost st))
  (for/list ([s pq])
    (if (posn= posn (state-posn s))
        (if (< cost (state-cost s))
            st
            s)
        s)))

;; straight from russel-norvig
(define (uniform-cost-search grid start goal)
  (define explored (mutable-set))
  (let loop ([frontier (list (state 0 empty start))])
    (cond
      [(empty? frontier) #f]
      [else
       (define node (car frontier))
       (define p (state-posn node))
       (define cost (state-cost node))
       (cond [(posn= p goal) node]
             [else
              (set-add! explored (state-posn node))
              (define adj (get-safe-adj grid p))
              (define new-frontier
                (for/fold ([new-frontier (cdr frontier)]) ([child adj])
                  (define child-in-frontier? (priority-queue-member? new-frontier child))
                  (define new-state (state (+ cost 1) node child))
                  (cond [(not (or child-in-frontier? (set-member? explored child)))
                         (priority-queue-insert new-frontier new-state)]
                        [else
                         (priority-queue-replace-if-better new-frontier new-state)])))
              (loop new-frontier)])])))

(define (add-bytes-to-grid! bytes n)
  (for ([b bytes]
        [_ (+ n 1)])
    (! grid (car b) (cadr b) 'byte)))

;; part 1
(add-bytes-to-grid! bytes 1024)
(define path (uniform-cost-search grid '(0 0) '(70 70)))
(state-cost path)

;; part 2
(define done #f)
(for ([i (in-range 3030 (length bytes))]
      #:break done)
  (add-bytes-to-grid! bytes i)
  (define path (uniform-cost-search grid '(0 0) '(70 70)))
  (unless path
    (set! done #t)
    ;; had an off-by-1 error
    (printf "~a:~a~n" i (list-ref bytes i))))
