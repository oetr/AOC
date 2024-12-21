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

(define (get-all-paths start goal)
  (define visited (mutable-set))
  (define best-cost #f)
  (define paths '())
  (let loop ([frontier (list (node 0 start '() '()))])
    (cond
      [(empty? frontier) paths]
      [else
       (for ([a-node frontier])
         (define p (node-key a-node))
         (define cost (node-cost a-node))
         (set-add! visited p))
       (define new-frontier
         (for/fold ([new-frontier '()])
                   ([a-node frontier])
           (define p (node-key a-node))
           (define cost (node-cost a-node))
           (cond [(eq? p goal)
                  (set! paths (cons a-node paths))
                  new-frontier]
                 [else
                  (define adj-edges (key-edges p))
                  (define frontier
                    (for/fold ([new-frontier '()])
                              ([child-key (map edge-key adj-edges)]
                               [child-edge adj-edges]
                               #:unless (set-member? visited child-key))
                      (define new-state (node (+ cost 1) child-key child-edge a-node))
                      (cons new-state new-frontier)))
                  (append frontier new-frontier)])))
       (loop new-frontier)])))

(define (compression-rank path)
  (for/sum ([action1 path]
            [action2 (cdr path)])
    (if (eq? action1 action2)
        1
        0)))
    

;; precompute the paths
(define (precompute-shortest keypad)
  (for*/hash ([(key-value1 key1) (in-hash keypad)]
              [(key-value2 key2) (in-hash keypad)])
    (cond [(equal? key-value1 key-value2)
           (values (cons key-value1 key-value2)
                   '(A))]
          [else
           (define all-paths (get-all-paths key1 key2))
           ;; only leave the best-compressed path
           (define best-path (argmax compression-rank (map node->path all-paths)))
           (values (cons key-value1 key-value2)
                   (append best-path '(A)))])))
                   
                   ;; (for/list ([p ])
                   ;;   (append (node->path p) '(A))))])))

;;(map node->path (get-all-paths (hash-ref keypad1 7) (hash-ref keypad1 'A)))

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
(define (press-code code precomp initial)
  (define-values (paths _)
    (for/fold ([paths '()]
               [current-key initial])
              ([goal code])
      (define shortest-paths (hash-ref precomp (cons current-key goal)))
      (values (cons shortest-paths paths)
              goal)))
  (map flatten (apply cartesian-product (reverse paths))))

(define (press-code2 code precomp initial)
  (define-values (paths _)
    (for/fold ([paths '()]
               [current-key initial])
              ([goal code])
      (define shortest-paths (hash-ref precomp (cons current-key goal)))
      (values (cons shortest-paths paths)
              goal)))
  (flatten (reverse paths)))

(define (press-code-pick-one code precomp initial)
  (define-values (paths _)
    (for/fold ([paths '()]
               [current-key initial])
              ([goal code])
      (define p (car (hash-ref precomp (cons current-key goal))))
      (values (cons p paths)
              goal)))
  (flatten (reverse paths)))

(define precomp1 (precompute-shortest keypad1))
(define precomp2 (precompute-shortest keypad2))

;; (press-code (car codes) keypad1 precomp1 'A)
;; (define (get-min-path-len code precomp1 precomp2)
;;   (define paths (press-code code precomp1 'A))
;;   (define paths1
;;     (let ([paths (apply append
;;                         (for/list ([p paths])
;;                           (press-code p precomp2 'A)))])
;;       (define min-len (apply min (map length paths)))
;;       (filter-not (λ (lst) (> (length lst) min-len)) paths)))

;;   (define paths2
;;     (let ([paths (for/list ([p paths1])
;;                    (press-code-pick-one p precomp2 'A))])
;;       (define min-len (apply min (map length paths)))
;;       min-len))
;;   paths2)
(press-code2 (press-code2 (press-code2 '(0 2 9 A) precomp1 'A)
                          precomp2 'A)
             precomp2 'A)

(define (get-min-path-len code precomp1 precomp2 n)
  (length 
   (for/fold ([code (press-code2 code precomp1 'A)])
             ([_ n])
     (press-code2 code precomp2 'A))))
  
  ;; (define paths1
  ;;   (let ([paths (apply append
  ;;                       (for/list ([p paths])
  ;;                         (press-code p precomp2 'A)))])
  ;;     (define min-len (apply min (map length paths)))
  ;;     (filter-not (λ (lst) (> (length lst) min-len)) paths)))
  ;; (define paths2
  ;;   (let ([paths (for/list ([p paths1])
  ;;                  (press-code-pick-one p precomp2 'A))])
  ;;     (define min-len (apply min (map length paths)))
  ;;     min-len))
  ;; paths2)

;;"208A\n586A\n341A\n463A\n593A\n"
;; part 1
(+ 
 (* 208 (get-min-path-len '(2 0 8 A) precomp1 precomp2 2))
 (* 586 (get-min-path-len '(5 8 6 A) precomp1 precomp2 2))
 (* 341 (get-min-path-len '(3 4 1 A) precomp1 precomp2 2))
 (* 463 (get-min-path-len '(4 6 3 A) precomp1 precomp2 2))
 (* 593 (get-min-path-len '(5 9 3 A) precomp1 precomp2 2)))


(define (get-min-path-len2 code precomp1 precomp2 n)
  (define final
    (for/fold ([paths (press-code code precomp1 'A)])
              ([i (- n 2)])
      (define new-paths (apply append
                               (for/list ([p paths])
                                 (press-code p precomp2 'A))))
      (define min-len (apply min (map length new-paths)))
      (define max-len (apply max (map length new-paths)))
      
      (define filtered-paths (filter-not (λ (lst) (> (length lst) min-len)) new-paths))
      (define filtered-bad-paths (filter (λ (lst) (> (length lst) min-len)) new-paths))
      (printf "~a ~a --- ~a\n" min-len max-len (length (car filtered-paths)))
      (printf "   ~a\n   ~a\n"  (car filtered-paths) (car filtered-bad-paths))
      ;; run-length code, and take the shortest
      filtered-paths))
  (length (car final)))

;;(get-min-path-len2 '(2 0 8 A) precomp1 precomp2 4)
