#lang racket

(require advent-of-code
         threading)

(define input (fetch-aoc-input (find-session) 2024 06 #:cache #true))

(define the-map (string-split input "\n"))
(define ROWS (length the-map))
(define COLS (string-length (car the-map)))

(define GUARD (char->integer #\^))
(define FLOOR (char->integer #\.))
(define OBSTACLE (char->integer #\#))
(define MARK (char->integer #\X))
(define DONE 0)

(define (copy-room room #:v (v 'default))
  (for/vector ([row room])
    (for/vector ([val row])
      (if (eq? v 'default)
          val
          v))))

;; get initial guard position
(define (get-val room row col)
  (~> room
      (vector-ref _ row)
      (vector-ref _ col)))

(define (set-room-val! room row col val)
  (define r (vector-ref room row))
  (vector-set! r col val))

(define (mark-room! room row col)
  (set-room-val! room row col MARK))

(define (guard-change-direction! state)
  (match (state-guard-direction state)
    ['up (set-state-guard-direction! state 'right)]
    ['right (set-state-guard-direction! state 'down)]
    ['down (set-state-guard-direction! state 'left)]
    ['left (set-state-guard-direction! state 'up)]))

(struct state (room guard-row guard-col guard-direction cycles) #:mutable)

;; make the room one tile bigger in all directions, filled with 0
(define proto-room
  (~> input
      (string-split _ "\n")
      (map (lambda~> string->bytes/utf-8 bytes->list
                     (cons DONE _)
                     (append _ (list DONE))
                     list->vector) _)
      list->vector
      (vector-append (vector (make-vector (+ 2 (string-length (car the-map))))) _)
      (vector-append _ (vector (make-vector (+ 2 (string-length (car the-map))))))))

(define (get-initial-guard-pose room)
  (define (next-row-col r c)
    (if (>= c COLS)
        (values (+ r 1) 1)
        (values r (+ c 1))))
  (let loop ([row 1] [col 1])
    (define val (get-val room row col))
    (define-values (row-next col-next)
      (next-row-col row col))
    (if (= val GUARD)
        (let [(r (vector-ref room row))]
          (values row col 'up))
        (loop row-next col-next))))

(define (set-guard-position! state row col)
  (set-state-guard-row! state row)
  (set-state-guard-col! state col))

(define (guard-move! state)
  (define-values (new-row new-col)
    (match (state-guard-direction state)
      ['up (values (- (state-guard-row state) 1) (state-guard-col state))]
      ['down (values (+ (state-guard-row state) 1) (state-guard-col state))]
      ['left (values (state-guard-row state) (- (state-guard-col state) 1))]
      ['right (values (state-guard-row state) (+ (state-guard-col state) 1))]))
  (define val (get-val (state-room state) new-row new-col))
  (cond
    [(= DONE val) #f]
    [(= FLOOR val) (set-guard-position! state new-row new-col)
                   (mark-room! (state-room state) new-row new-col)
                   #t]
    [(= MARK val) (set-guard-position! state new-row new-col) #t]
    [(= OBSTACLE val) (guard-change-direction! state) #t]
    [else (error 'guard-move! "unknown room value")]))

;; part 1
(define (simulate)
  (define room (copy-room proto-room))
  (define-values (guard-row guard-col guard-direction) (get-initial-guard-pose room))
  ;; markt the floor
  (mark-room! room guard-row guard-col)
  (define st (state room guard-row guard-col guard-direction 'unused))
  
  (let loop ()
    (when (guard-move! st) (loop)))
  
  (for*/fold ([result 0]) ([r room] [val r])
    (if (= val MARK)
        (+ result 1)
        result)))

;;(simulate)


;; part 2
(define (update-cycle! state)
  (define direction (state-guard-direction state))
  (define row (state-guard-row state))
  (define col (state-guard-col state))
  (define cycles (state-cycles state))
  (define cycles-in-pose (get-val (state-cycles state) row col))
  (if (member direction cycles-in-pose)
      #t
      (begin
        (set-room-val! cycles row col (cons direction cycles-in-pose))
        #f)))

(define (guard-move2! state)
  (define-values (new-row new-col)
    (match (state-guard-direction state)
      ['up (values (- (state-guard-row state) 1) (state-guard-col state))]
      ['down (values (+ (state-guard-row state) 1) (state-guard-col state))]
      ['left (values (state-guard-row state) (- (state-guard-col state) 1))]
      ['right (values (state-guard-row state) (+ (state-guard-col state) 1))]))
  (define val (get-val (state-room state) new-row new-col))
  (define cycle? (update-cycle! state))
  (cond
    [cycle? 'CYCLE]
    [(= DONE val) '#t]
    [(= FLOOR val) (set-guard-position! state new-row new-col)
                   (mark-room! (state-room state) new-row new-col)
                   #f]
    [(= MARK val) (set-guard-position! state new-row new-col) #f]
    [(= OBSTACLE val) (guard-change-direction! state) #f]
    [else (error 'guard-move! "unknown room value")]))

(define (simulate2)
  (define room (copy-room proto-room))
  (define-values (guard-row guard-col guard-direction) (get-initial-guard-pose room))
  ;; markt the floor
  (mark-room! room guard-row guard-col)
  (define st (state room guard-row guard-col guard-direction 'unused))
  
  (let loop ()
    (when (guard-move! st) (loop)))

  ;; mark the initial guard position again
  (set-room-val! room guard-row guard-col GUARD)

  ;; get the marked coordinates
  (define coordinates
    (for*/list ([row (in-range (vector-length room))]
                [col (in-range (vector-length (vector-ref room 0)))]
                #:when (= MARK (get-val room row col)))
      (cons row col)))
  
  (for/fold ([result 0]) ([insert-obstacle (in-list coordinates)]
                          [i (length coordinates)])
    (printf "~a: ~a    ~a - ~a~n" i result (car insert-obstacle) (cdr insert-obstacle))
    (define room (copy-room proto-room))
    (define-values (guard-row guard-col guard-direction) (get-initial-guard-pose room))
    ;; markt the floor
    (mark-room! room guard-row guard-col)
    (set-room-val! room (car insert-obstacle) (cdr insert-obstacle) OBSTACLE)
    (define st (state room guard-row guard-col guard-direction (copy-room proto-room #:v '())))

    (+ result
      (let loop ()
        (define result (guard-move2! st))
        (cond [(eq? result 'CYCLE) 1]
              [result 0]
              [else (loop)])))))

(simulate2)

