#lang racket

(require advent-of-code
         plot)

(define input (fetch-aoc-input (find-session) 2025 9 #:cache #true))

(define (parse-input input)
  (for/list ([line (string-split input)])
    (define nums (string-split line ","))
    (cons (string->number (car nums))
          (string->number (cadr nums)))))

(define data (parse-input input))

(define (area p1 p2)
  (* (+ 1 (abs (- (car p1) (car p2))))
     (+ 1 (abs (- (cdr p1) (cdr p2))))))

(define (distance p1 p2)
  (sqrt (+ (sqr (+ 1 (abs (- (car p1) (car p2)))))
           (sqr (+ 1 (abs (- (cdr p1) (cdr p2))))))))

;; part 1
(time (for*/fold ([max-area 0])
                 ([p1 data]
                  [p2 data]
                  #:do [(define candidate (area p1 p2))]
                  #:when (> candidate max-area))
        candidate))

;; cpu time: 3 real time: 3 gc time: 0


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(argmin car data)

(plot-new-window? #f)

(plot (points
       (for/list ([d (in-list data)])
         (list (car d) (cdr d)))))

(define (make-line-hash data proc)
  (for*/fold ([result (hash)])
             ([group (group-by proc data)]
              [point group])
    (hash-set result point (car (remove point group)))))


(define (walk-polygon data)
  (define horizontal-lines
    (make-line-hash data cdr))

  (define vertical-lines
    (make-line-hash data car))

  ;; pick a random point as a start
  (let loop ([start (car data)]
             [current (hash-ref horizontal-lines (car data))]
             [trail (list)]
             [horizontal? #f])
    (cond
      [(equal? current start) trail]
      [else
       (define next (if horizontal?
                     (hash-ref horizontal-lines current)
                     (hash-ref vertical-lines current)))
       (loop start next (cons next trail) (not horizontal?))])))

(define poly (walk-polygon data))

(define horizontal-lines
    (make-line-hash data cdr))

  (define vertical-lines
    (make-line-hash data car))

(plot (lines
       (for/list ([d (in-list poly)])
         (list (car d) (cdr d)))))

(define sorted-by-distance
  (sort data
        (lambda (x y)
          (> (distance x (hash-ref horizontal-lines x))
             (distance y (hash-ref horizontal-lines y))))
        ))

(hash-ref horizontal-lines (caddr sorted-by-distance))

(define (find-biggest-area data condition?)
  (for*/fold ([max-area 0])
             ([p1 data]
              [p2 data]
              #:do [(define candidate (area p1 p2))]
              #:when (> candidate max-area)
              #:when (condition? p1)
              #:when (condition? p2)
              #:when (condition? (cons (car p1) (cdr p2)))
              #:when (condition? (cons (car p2) (cdr p1))))
    candidate))

(struct line (l r) #:transparent)

(define (line-extend a-line x)
  (define l (line-l a-line))
  (define r (line-r a-line))
  (cond
    [(and (number? l) (number? r))
     (cond {(< x l) (line x r)}
           {(> x r) (line l x)}
           {else a-line})]
    [(number? l) (if (< x l)
                     (line x l)
                     (line l x))]
    [(number? r) (if (< x r) (line x r) (line r x))]
    [else (line x #f)]))

(define (in-inclusive-range* from to [step 1])
  (if (> from to)
      (in-inclusive-range to from step)
      (in-inclusive-range from to step)))


    
(define (fill-with-lines walk)
  (for/fold ([lines (hash)])
            ([p1 walk]
             [p2 (cons (last walk) walk)]
             #:when (= (car p1) (car p2)))
    (define y1 (cdr p1))
    (define y2 (cdr p2))
    (define x (car p1))
    (for/fold ([lines lines])
              ([y (in-inclusive-range* y1 y2)])
      (define a-line (hash-ref lines y #f))
      (cond [a-line (hash-set lines y (line-extend a-line x))]
            [(hash-set lines y (line x #f))]))))

(define all-lines (fill-with-lines poly))

(for ([(y l) all-lines])
  (unless (and (number? (line-l l))
               (number? (line-l l)))
    (printf "~a~n" l)
            ))
  
(define (upper? p)
  (>= (cdr p) 50238))

(define (lower? p)
  (<= (cdr p) 48572))

(take sorted-by-distance 4)

(define (in-line? p (and? lower?))
  (define line (hash-ref all-lines (cdr p)))
  (and (and? p)
       (>= (car p) (line-l line))
       (<= (car p) (line-r line))))

(find-biggest-area data in-line?)

1572047142 ;;- upper
1295526528 ;; lower

;; too high: 3064142297
4749929916
4631146730



