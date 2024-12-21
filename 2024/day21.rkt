#lang racket

(require advent-of-code
         threading
         "./lib.rkt")

(define input (fetch-aoc-input (find-session) 2024 21 #:cache #true))


(struct edge (dir key) #:transparent)
(struct key (val edges) #:mutable)

;; keypad
(define keypad0-a '((7 8 9)
                    (4 5 6)
                    (1 2 3)
                    (_ 0 A)))

(define keypad<-a '((_ ^ A)
                    (< v >)))

(define (list->nodes keypad)
  (define keys
    (for/list ([row keypad])
      (define keys 
        (for/list ([button row])
          (if (and (symbol? button) (symbol=? button '_))
              '_
              (key button '()))))
      ;; connect horizontally
      (for ([key1 keys]
            [key2 (cdr keys)]
            #:unless (or (symbol? key1) (symbol? key2)))
        (define edges1 (key-edges key1))
        (define edges2 (key-edges key2))
        (set-key-edges! key1 (cons (edge '> key2) edges1))
        (set-key-edges! key2 (cons (edge '< key1) edges2)))
      keys))
  ;; connect vertically
  (for ([row1 keys]
        [row2 (cdr keys)])
    (for ([key1 row1]
          [key2 row2]
          #:unless (or (symbol? key1) (symbol? key2)))
      (define edges1 (key-edges key1))
      (define edges2 (key-edges key2))
      (set-key-edges! key1 (cons (edge 'v key2) edges1))
      (set-key-edges! key2 (cons (edge '^ key1) edges2))))
  (for/hash ([key (flatten keys)]
             #:unless (symbol? key))
    (values (key-val key) key)))

(define codes
  (for/list ([s (string-split input)])
    (append (for/list ([n (cdr (regexp-match #px"([0-9]?)([0-9]?)([0-9]?)A" s))])
              (string->number n))
            '(A))))

(define keypad1 (list->nodes keypad0-a))
(define keypad2 (list->nodes keypad<-a))
(define keypad3 (list->nodes keypad<-a))
(define keypad4 (list->nodes keypad<-a))


(define (priority-queue-insert states st)
  (let loop ([head '()][states states])
    (cond [(empty? states) (append head (list st))]
          [(eq? (car states) st) (append head (cons st states))]
          [else (loop (append head (list (car states))) (cdr states))])))

(define (priority-queue-member? pq key)
  (for/or ([state pq])
    (eq? (node-key state) key)))


(define (priority-queue-replace-if-better pq st)
  (define key (node-key st))
  (define cost (node-cost st))
  (for/list ([s pq])
    (if (eq? key (node-key s))
        (if (< cost (node-cost s))
            st
            s)
        s)))

(struct node (cost key edge parent))

(define (uniform-cost-search start goal)
  (define explored (mutable-set))
  (let loop ([frontier (list (node 0 start empty empty))])
    (cond
      [(empty? frontier) #f]
      [else
       (define a-node (car frontier))
       (define p (node-key a-node))
       (define cost (node-cost a-node))
       (cond [(eq? p goal) a-node]
             [else
              (set-add! explored (node-key a-node))
              (define adj-edges (key-edges p))
              (define new-frontier
                (for/fold ([new-frontier (cdr frontier)]) ([child-key (map edge-key adj-edges)]
                                                           [child-edge adj-edges])
                  (define child-in-frontier? (priority-queue-member? new-frontier child-key))
                  (define new-state (node (+ cost 1) child-key child-edge a-node))
                  (cond [(not (or child-in-frontier? (set-member? explored child-key)))
                         (priority-queue-insert new-frontier new-state)]
                        [else
                         (priority-queue-replace-if-better new-frontier new-state)])))
              (loop new-frontier)])])))

(define (node->path a-node)
  (rest
   (reverse 
    (let loop ([a-node a-node])
      (define parent (node-parent a-node))
      (define edge (node-edge a-node))
      (define edge-val (if (empty? edge) empty (edge-dir edge)))
      (cond
        ([empty? parent] (list edge-val))
        (else (cons edge-val (loop parent))))))))

;; press some code on keypad1
(define (press-code code keypad initial)
  (define-values (paths _)
    (for/fold ([paths '()]
               [current-key (hash-ref keypad initial)])
              ([c code])
      (define goal (hash-ref keypad c))
      (define path (append (node->path (uniform-cost-search current-key goal)) '(A)))
      (values (cons path paths)
              goal)))
  (flatten (reverse paths)))

;;(press-code (car codes) keypad1 'A)
(length (press-code (press-code (press-code '(0 2 9 A) keypad1 'A)
                                keypad2 'A) keypad2 'A))

(define (layers code keypads)
  (for/fold ([code code])
            ([(keypad i) (in-indexed keypads)])
    (define new-code (press-code code keypad 'A))
    (printf "~a: ~a~n" i (length new-code))
    new-code
    ))

(length (layers '(0 2 9 A) (list keypad1 keypad2 keypad2)))

(press-code '(< A ^ A > ^ ^ A v v v A) keypad2 'A)
(length (layers '(< A ^ A > ^ ^ A v v v A) (list keypad2 keypad2)))

(length (press-code '(v < < A > > ^ A < A > A v A < ^ A A > A < v A A A > ^ A) keypad2 'A))

(press-code '(< A ^ A > ^ ^ A v v v A) keypad2 'A)
;;(node->path (uniform-cost-search (hash-ref keypad1 'A) (hash-ref keypad1 7)))
(string-length "<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A")

;; some local shortest path for one key pad might not result in shortest global path after n keypad chains
;; 
