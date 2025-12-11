#lang racket

(require advent-of-code
         math/matrix
         math/array
         memo)

(define test-input "[.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
[...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
[.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}")

(define input (fetch-aoc-input (find-session) 2025 10 #:cache #true))

(struct machine (indicator buttons requirements) #:transparent)

(define (parse-indicator str)
  (for/vector ([chr (substring str 1 (sub1 (string-length str)))])
    (char=? #\# chr)))

(define (parse-buttons buttons)
  (for/list ([str buttons])
    (for/list ([digit (string-split (substring str 1 (sub1 (string-length str))) ",")])
      (string->number digit))))

(define (parse-requirements str)
  (for/vector ([digit (string-split (substring str 1 (sub1 (string-length str))) ",")])
    (string->number digit)))


(define (parse-input input)
  (for/list ([line (string-split input "\n")])
    (define data (string-split line " "))
    (machine (parse-indicator (car data))
             (parse-buttons (drop-right (cdr data) 1))
             (parse-requirements (last data)))))

(define (button-press indicator button)
  (define result (vector-copy indicator))
  (for ([i button])
    (vector-set! result i (not (vector-ref result i))))
  result)


(define (bfs machine)
  (define target (machine-indicator machine))
  (define buttons (machine-buttons machine))
  (let loop ([current (list (make-vector (vector-length target) #f))]
             [depth 0])
    (cond
      [(for/or ([indicator current]) (equal? target indicator))
       depth]
      [else
       (loop
        (for*/fold ([result empty])
                   ([indicator current]
                    [button buttons])
          (cons (button-press indicator button) result))
        (+ 1 depth))])))

(define (part1 input)
  (for/sum ([machine (parse-input input)])
    (bfs machine)))

;;(time (part1 input))
;; 401
;; cpu time: 1466 real time: 1466 gc time: 444


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; part 2
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (define (button-press-with-joltage indicator joltage button)
;;   ;;(printf "BUTTON: ~a - ~a                ~a\n" indicator joltage button)
;;   (define result (vector-copy indicator))
;;   (define result-joltage (vector-copy joltage))
;;   (define break? #f)
;;   (for ([i button] #:break break?)
;;     (vector-set! result i (not (vector-ref result i)))
;;     (define new-joltage (sub1 (vector-ref result-joltage i)))
;;     (if (< new-joltage 0)
;;         (set! break? #t)
;;         (vector-set! result-joltage i new-joltage)))
;;   (if break?
;;       (values #f #f)
;;       (values result result-joltage)))

(define (button-press-with-joltage joltage button)
  ;;(printf "BUTTON: ~a - ~a                ~a\n" indicator joltage button)
  (define result-joltage (vector-copy joltage))
  (define break? #f)
  (for ([i button] #:break break?)
    ;;(vector-set! result i (not (vector-ref result i)))
    (define new-joltage (sub1 (vector-ref result-joltage i)))
    (if (< new-joltage 0)
        (set! break? #t)
        (vector-set! result-joltage i new-joltage)))
  (if break?
      #f
      result-joltage))

;;(button-press-with-joltage #(#f #f #f #f) #(1 1 0 1) '(3))
(define (bfs+indicator+joltage machine)
  (define start-time (current-milliseconds))
  (define target-joltage (make-vector (vector-length (machine-requirements machine)) 0))
  (define buttons (machine-buttons machine))
  (define seen (mutable-set))
  (let loop ([frontier (set (vector-copy (machine-requirements machine)))]
             [depth 0])
    (printf "~a:   [~a] -- [~a] ~n" depth (set-count frontier) (set-count seen))
    (define pruned 0)
    (define result (mutable-set))
    (define done? #f)
    (define best (make-vector (vector-length target-joltage) (expt 2 32)))
    (define best-distance (for/sum ([v (in-vector best)]) v))
    (for* ([indicator-joltage (in-set frontier)]
           [button (in-list buttons)]
           #:break done?)
      (define new-joltage
           (button-press-with-joltage indicator-joltage
                                      button))
      (cond
        [new-joltage
         (define better? (for/sum ([v (in-vector new-joltage)]) v))
         (when (< better? best-distance)
           (set! best new-joltage)
           (set! best-distance better?))
         
         (when (equal? target-joltage new-joltage)
           (set! done? #t))
         (unless (set-member? seen new-joltage)
           (set-add! result new-joltage)
           (set-add! seen new-joltage))]
        [else
         (set! pruned (add1 pruned))]))
    (printf "~a~n" best)
    (cond
      [done? (+ depth +1)]
      [(= depth 1000) depth]
      ;;[(> (- (current-milliseconds) start-time) 30000) -1]
      [else 
        (loop
         (set best)
         (+ 1 depth))])))

(define (distance-to-zero joltages)
  (for/sum ([v (in-vector joltages)]) (/ v (vector-length joltages) 1.0)))

(define (max-distance-to-zero joltages)
  (for/fold ([m 0])
            ([j (in-vector joltages)]
             #:when (> j m))
    j))

(define (joltage> j1 j2 (scale-by 0.5))
  (define d1 (car j1))
  (define d2 (car j2))
  (> (+ (* scale-by d1) (* 2 (max-distance-to-zero (cdr j1))) (distance-to-zero (cdr j1)))
     (+ (* scale-by d2) (* 2 (max-distance-to-zero (cdr j2))) (distance-to-zero (cdr j2)))))

;; (define (joltage< j1 j2 (scale-by 0.5))
;;   (define d1 (car j1))
;;   (define d2 (car j2))
;;   (< (+ (* scale-by d1) (* 2 (max-distance-to-zero (cdr j1))) (distance-to-zero (cdr j1)))
;;      (+ (* scale-by d2) (* 2 (max-distance-to-zero (cdr j2))) (distance-to-zero (cdr j2)))))



(define (dfs machine)
  (define target-joltage (make-vector (vector-length (machine-requirements machine)) 0))
  (define buttons (machine-buttons machine))
  ;;(printf "BUTTONS: ~a~n" buttons)
  (define memo (make-hash))
  (define dead-end (mutable-set))
  (define seen (mutable-set))
  (define times 0)
  (define MAX-INT (expt 2 32))
  (let loop ([frontier (list (cons 0 (vector-copy (machine-requirements machine))))])
    (set! times (add1 times))
    (when (zero? (modulo times 1000000))
      (printf "~a: ~a    ~a, ~a~n" (length frontier) (car frontier) (set-count dead-end) (set-count seen)))
    (cond
      ;;[(> times 10000) (error 'done "DONE")]
      [(empty? frontier) MAX-INT]
      [else
       (define next (car frontier))
       (define depth (car next))
       (define current-joltage (cdr next))
       (define exists? (hash-ref memo current-joltage #f))
       (cond
         [(set-member? dead-end current-joltage) (loop (cdr frontier))]
         [(set-member? seen current-joltage) (loop (cdr frontier))]
         [(equal? target-joltage current-joltage)
          (printf "found: ~a~n" depth)
          (min depth (loop (cdr frontier)))
          ]
         [exists? exists?]
         [else
          (set-add! seen next)
          (define additional-frontier
            (for/fold ([frontier empty]
                       #:result (remove-duplicates frontier))
                      ([button (in-list buttons)])
              (define new-joltage
                (button-press-with-joltage current-joltage
                                           button))
              ;;(printf "~a -> ~a    ~a~n" current-joltage new-joltage button)
              (if (and new-joltage (not (set-member? seen (cons (add1 depth) new-joltage))))
                  (cons (cons (add1 depth) new-joltage)
                        frontier)
                  frontier)))
          (when (zero? (length additional-frontier))
            (set-add! dead-end current-joltage))
          (define new-frontier (for/fold ([frontier  (cdr frontier)])
                                         ([n (in-list (sort additional-frontier (lambda (a b) (joltage> a b 0.5))))])
                                 (cons n frontier)))
          (define min-depth (loop new-frontier))
                             ;;new-frontier))
          (hash-set! memo current-joltage min-depth)
          min-depth])])))



(define (joltage< j1 j2)
  (< (distance-to-zero j1)
     (distance-to-zero j2)))
  ;; (< (+ (* 2 (max-distance-to-zero j1)) (distance-to-zero j1))
  ;;    (+ (* 2 (max-distance-to-zero j2)) (distance-to-zero j2))))

(define (find-fewest-button-presses machine)
  (define target-joltage (machine-requirements machine))
  (define buttons (machine-buttons machine))
  (define MAX-INT (expt 2 32))
  (define seen (make-hash))
  (define counter 0)
  
  (define/memoize (loop depth current-joltage)
    ;;(set! counter (add1 counter))
    (define seen? (hash-ref seen current-joltage empty))
    ;;(printf "~a -- ~a   ~a   ~a~n" depth seen? current-joltage (hash-count seen))
    (when (empty? seen?)
      (hash-set! seen current-joltage depth))
    (cond
      ;; [(> counter 100000)
      ;;  ;;(printf "COUNTER\n")
      ;;  0]
      [(for/and ([j current-joltage]) (zero? j))
       ;;(printf "ZERO!\n")
       depth]
      [(and (number? seen?) (< seen? depth))
       ;;(printf "<!\n")
       MAX-INT]
      [(and (number? seen?) (> seen? depth)) (hash-set! seen current-joltage depth)]
      [else
       ;;(printf "ELSE\n")
       (define next-joltages
         (for/list ([button (in-list buttons)]
                    #:do [(define next (button-press-with-joltage current-joltage button))]
                    #:when next
                    #:when (let ([seen? (hash-ref seen next #f)])
                             (or (not seen?)
                                 (and seen? (> seen? (add1 depth))))))
           next))
       ;; select the best candidate first
       (for/fold ([m MAX-INT])
                 ;;([new-joltage (in-list (sort next-joltages (lambda (a b) (joltage< a b))))])
                 ([new-joltage (in-list next-joltages)])
         (cond
           [new-joltage 
            (define min-depth (loop (add1 depth) new-joltage))
            (if (< min-depth m) min-depth m)]
           [else m]))]))
  
  (loop 0 target-joltage))



;;(time (find-fewest-button-presses (list-ref data 0)))
;;(time (find-fewest-button-presses (list-ref data 7)))

;; (define result
;;   (for/list ([(machine i) (in-indexed data)])
;;     (define result (time (find-fewest-button-presses machine)))
;;     (printf "~a: ~a\n" i result)
;;     result))


;; (let ([h (make-hash)])
;;   (hash-set! h 0 10)
;;   h)

;; [.#.###.#] (0,2,6,7) (2,3) (1,2,3,5,7) (0,1,3,4,5,6,7) (0,1,2,3,5,7) (0,2,4,5,7) (1,2,6,7) (0,2,3,5) (3,7) (2,3,4) {35,44,107,74,41,44,31,81}


;;(dfs (list-ref data 2))
;;(dfs (machine #(#f #t #f) (list '(1) '(0 2) '(0 1 2)) #(2 3 2)))
          
;;(bfs+indicator+joltage (list-ref data 1))

;;(bfs+indicator+joltage (machine #(#f #t #f) (list '(1) '(0 2) '(0 1 2)) #(2 3 2)))
;; (for/sum ([(machine i) (in-indexed data)])
;;   (printf "~a~n" i)
;;   (dfs+indicator+joltage machine))

;; (define result
;;   (for/list ([(machine i) (in-indexed data)])
;;     (define result (dfs machine))
;;     (printf "~a\n" result)
;;     (cons i result)))


(define (iddfs+reqs machine)
  (define target-joltage (machine-requirements machine))
  (define buttons (machine-buttons machine))
  (define MAX-INT (expt 2 32))
  (define seen (make-hash))
  (define dead-end (mutable-set))
  (define count 0)
  
  (define (dfs joltage depth depth-limit)
    (set! count (add1 count))
    (define seen? (hash-ref seen joltage empty))
    (when (zero? (modulo count 100000))
      (printf "~a: ~a    ?~a   [~a] [~a]~n" depth joltage seen? (hash-count seen) (set-count dead-end)))
    (when (empty? seen?)
      (hash-set! seen joltage depth))
    (cond
      [(for/and ([j joltage]) (zero? j)) depth]
      [(set-member? dead-end joltage) #f]
      [(>= depth depth-limit) ;;(printf "limit~n")
                              #f]
      [(and (number? seen?) (> seen? depth))
       ;;(printf "number and seen>\n")
       (hash-set! seen joltage depth)
       (dfs joltage depth depth-limit)]
      [else
       ;;(printf "else\n")
       (define next-joltages
         (for/list ([button (in-list buttons)]
                    #:do [(define next (button-press-with-joltage joltage button))]
                    #:when next
                    #:when (let ([seen? (hash-ref seen next #f)])
                             (or (not seen?)
                                 (and seen? (> seen? (add1 depth)))))
                    ;;#:when (not (set-member? dead-end next))
                    )
           next))
       (cond
         [(empty? next-joltages) #f]
         [else    
          (for/fold ([m #f])
                    ([new-joltage (in-list (sort next-joltages joltage<))])
            (define min-depth (dfs new-joltage (add1 depth) depth-limit))
            ;;(printf "   MINDEPTH: ~a~n" min-depth)
            (cond
              [(not min-depth) ;;(set-add! dead-end new-joltage)
                               m]
              [min-depth (if m
                             (min min-depth m)
                             min-depth)]
              [else (error 'min-depth-sucjs "sucks")]))])]))

  (let loop ([depth-limit 1000])
    (printf "depth: ~a~n" depth-limit)
    (set-clear! dead-end)
    ;;(hash-clear! seen)
    (define result (dfs target-joltage 0 depth-limit))
    (cond
      [(and result (< result MAX-INT)) result]
      [(> depth-limit 400) (printf "depth reached\n") #f]
      [else (loop (add1 depth-limit))])))

(define data (parse-input input))
;;(time (iddfs+reqs (list-ref data 1)))


(define (button->bitmap button len)
  (define result (make-vector len 0))
  (for ([idx button])
    (vector-set! result idx 1))
  result)

(define (bitmaps->matrix bitmaps rows)
  (for*/matrix rows (length bitmaps) 
               ([bitmap bitmaps]
                [v bitmap])
               v))

;; (define (requirement->matrix req)
;;   (for/matrix (sequence-length req) 1 ([r req]) r))

;; (define m (bitmaps->matrix (list #(0 1 1 1) #(1 0 1 0)) 4))

;; (define machine (list-ref data 0))

;; (define bitmaps (map (lambda (button)
;;                        (button->bitmap button (sequence-length (machine-indicator machine))))
;;                      (machine-buttons machine)))

;; (define mats (for/list ([bmp-comb (combinations bitmaps)]
;;            #:unless (empty? bmp-comb))
;;   (printf "~a~n" bmp-comb)
;;   (bitmaps->matrix bmp-comb (sequence-length (machine-indicator machine)))))


;; (define ones (make-matrix (matrix-num-rows (car mats)) 1 1 ))

;; (matrix-lu (car mats))

;; (matrix* (matrix-transpose (car mats)) (col-matrix [1 1 1 1 1]))

;; (matrix* (matrix-shape (car mats))
;;          (matrix-shape (requirement->matrix (machine-requirements machine)))




               

;; (define (machine->matrices machine)
;;   (define buttons (machine-buttons machine))
;;   (define dimensions ))

;; ((list-ref data 1)

;; (define (solve machine)
;;   ;; convert to matrix
;;   (for*/matrix 10 3 ([x 10] [y 3])
;;     (* x y))
;;   )

         

  (define r
    '((0 . 40)
  (1 . -1)
  (2 . -1)
  (3 . 38)
  (4 . 30)
  (5 . 25)
  (6 . 34)
  (7 . -1)
  (8 . -1)
  (9 . 148)
  (10 . -1)
  (11 . -1)
  (12 . -1)
  (13 . -1)
  (14 . 37)
  (15 . 146)
  (16 . 58)
  (17 . -1)
  (18 . -1)
  (19 . 39)
  (20 . -1)
  (21 . -1)
  (22 . 157)
  (23 . -1)
  (24 . 48)
  (25 . 61)
  (26 . -1)
  (27 . -1)
  (28 . 37)
  (29 . 27)
  (30 . -1)
  (31 . -1)
  (32 . 205)
  (33 . -1)
  (34 . -1)
  (35 . 28)
  (36 . -1)
  (37 . -1)
  (38 . 38)
  (39 . -1)
  (40 . 53)
  (41 . -1)
  (42 . -1)
  (43 . -1)
  (44 . -1)
  (45 . -1)
  (46 . -1)
  (47 . -1)
  (48 . 63)
  (49 . -1)
  (50 . 48)
  (51 . -1)
  (52 . 39)
  (53 . 49)
  (54 . -1)
  (55 . 41)
  (56 . -1)
  (57 . 174)
  (58 . -1)
  (59 . 58)
  (60 . -1)
  (61 . 7)
  (62 . -1)
  (63 . 30)
  (64 . -1)
  (65 . 23)
  (66 . -1)
  (67 . 48)
  (68 . -1)
  (69 . -1)
  (70 . -1)
  (71 . 24)
  (72 . 52)
  (73 . -1)
  (74 . -1)
  (75 . -1)
  (76 . -1)
  (77 . 51)
  (78 . -1)
  (79 . -1)
  (80 . -1)
  (81 . -1)
  (82 . 38)
  (83 . 44)
  (84 . 40)
  (85 . -1)
  (86 . 27)
  (87 . -1)
  (88 . -1)
  (89 . 70)
  (90 . -1)
  (91 . -1)
  (92 . -1)
  (93 . 29)
  (94 . -1)
  (95 . -1)
  (96 . 21)
  (97 . -1)
  (98 . 159)
  (99 . -1)
  (100 . -1)
  (101 . -1)
  (102 . -1)
  (103 . -1)
  (104 . -1)
  (105 . 175)
  (106 . 66)
  (107 . -1)
  (108 . 38)
  (109 . -1)
  (110 . 179)
  (111 . 57)
  (112 . -1)
  (113 . -1)
  (114 . 28)
  (115 . -1)
  (116 . -1)
  (117 . -1)
  (118 . -1)
  (119 . 22)
  (120 . 33)
  (121 . -1)
  (122 . -1)
  (123 . -1)
  (124 . -1)
  (125 . 75)
  (126 . -1)
  (127 . -1)
  (128 . 53)
  (129 . -1)
  (130 . -1)
  (131 . -1)
  (132 . 134)
  (133 . 137)
  (134 . -1)
  (135 . -1)
  (136 . -1)
  (137 . -1)
  (138 . -1)
  (139 . -1)
  (140 . 51)
  (141 . -1)
  (142 . -1)
  (143 . 37)
  (144 . -1)
  (145 . -1)
  (146 . -1)
  (147 . -1)
  (148 . -1)
  (149 . -1)
  (150 . -1)))
