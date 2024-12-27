#lang racket

(provide
 priority-queue?
 priority-queue-add!
 priority-queue-poll!
 priority-queue-empty?
 make-priority-queue
 in-priority-queue
 priority-queue-contains?
 priority-queue-update!
 )

(struct priority-queue (queue size comp)
  #:mutable
  #:property prop:sequence
  (lambda (pq) (in-priority-queue pq)))
;; #:methods gen:equal+hash
;; [(define equal-proc heap=?)
;;  (define (hash-proc a hash-code)
;;    (hash-code (priority-queue->sorted-vector a)))
;;  (define (hash2-proc a hash-code)
;;    (hash-code (priority-queue->sorted-vector a)))]
 

(define (priority-queue-empty? pq)
  (zero? (priority-queue-size pq)))

(define (make-priority-queue comp . data)
  (define pq (priority-queue (make-vector (sequence-length data)) 0 comp))
  (for ([d data])
    (priority-queue-add! pq d))
  pq)

(define (priority-queue-add! pq element)
  (define size (priority-queue-size pq))
  (when (>= size (vector-length (priority-queue-queue pq)))
    (grow! pq (add1 size)))
  (sift-up! pq size element)
  (set-priority-queue-size! pq (+ size 1)))

(define (grow! pq min-capacity)
  (define vec (priority-queue-queue pq))
  (define old-capacity (vector-length vec))
  (define min-growth (-  min-capacity old-capacity))
  (define preferred-growth (if (< old-capacity 64)
                               (+ old-capacity 2)
                               (arithmetic-shift old-capacity -1)))
  (define new-capacity (+ old-capacity (max min-growth preferred-growth)))
  (set-priority-queue-queue! pq (vector-extend vec new-capacity #f)))

(define (sift-up! pq k element)
  (define queue (priority-queue-queue pq))
  (define new-k
    (let loop ([k k])
      (cond [(<= k 0) k]
            [else
             (define parent (arithmetic-shift (- k 1) -1))
             (define e (vector-ref queue parent))
             (cond [(not ((priority-queue-comp pq) element e)) k]
                   [else
                    (vector-set! queue k e)
                    (loop parent)])])))
  (vector-set! queue new-k element))

(define (sift-down! pq k x n)
  (define half (arithmetic-shift n -1))
  (define queue (priority-queue-queue pq))
  (define new-k
    (let loop ([k k])
      (cond
        [(>= k half) k]
        [else
         (define child (+ 1 (arithmetic-shift k 1)))
         (define right (+ child 1))
         (define-values (new-c new-child)
           (cond [(and (< right n)
                       (not ((priority-queue-comp pq) (vector-ref queue child) (vector-ref queue right))))
                  (values (vector-ref queue right)
                          right)]
                 [else
                  (values
                   (vector-ref queue child)
                   child)]))
         (cond [((priority-queue-comp pq) x new-c) k]
               [else
                (vector-set! queue k new-c)
                (loop new-child)])])))
  (vector-set! queue new-k x))


(define (priority-queue-poll! pq)
  (define size (priority-queue-size pq))
  (cond [(zero? size) #f]
        [else
         (define queue (priority-queue-queue pq))
         (define result (vector-ref queue 0))
         (set-priority-queue-size! pq (- size 1))
         (define n (- size 1))
         (define x (vector-ref queue n))
         (vector-set! queue n #f)
         (when (> n 0)
           (sift-down! pq 0 x n))
         result]))


(define (priority-queue-contains? pq comp?)
  (define queue (priority-queue-queue pq))
  (for/or ([i (in-range (priority-queue-size pq))])
    (comp? (vector-ref queue i))))

;; (for/fold ([result #f]) ([i (for/stream ([i 10]) (printf "stream: i: ~a~n" i) i)]
;;                          #:do [(define ok? (if (= i 3) #t #f))]
;;                          #:when ok?
;;                          #:final ok?)
;;   (printf "   ~a~n" i)
;;   i)

(define (priority-queue-update! pq eq-fn update-fn!)
  (define queue (priority-queue-queue pq))
  (for/fold ([updated #f]) ([i (in-range (priority-queue-size pq))]
                            #:do [(define old (vector-ref queue i))
                                  (define is-eq? (eq-fn old))]
                            #:when is-eq?
                            #:final is-eq?)
    (update-fn! old)
    #t))


(define (in-priority-queue pq)
  ;; not thread-safe
  (define queue (priority-queue-queue pq))
  (define size (priority-queue-size pq))
  (make-do-sequence
   (λ ()
     (initiate-sequence
      #:pos->element (λ (i) (vector-ref queue i))
      #:next-pos (λ (i) (+ i 1))
      #:init-pos 0
      #:continue-with-pos? (λ (i) (< i size))))))

(module+ test
  (require rackunit)
  (time
  (for ([i 100])
    (test-case (~a "random array-" i)
      (define n 10000)
      (define data (for/list ([_ n])
                     (exact-truncate (* (random) 10000000000))))
      (define pq (priority-queue (make-vector 0) 0 >))
      (for ([d data])
        (priority-queue-add! pq d))
      (define out (for/list ([_ n])
                    (priority-queue-poll! pq)))
      (check-equal? out (sort data >))))))



      

;; (priority-queue-add! pq 75)
;; (priority-queue-add! pq 23)
;; (priority-queue-add! pq 71)
;; (priority-queue-add! pq 51)
;; (priority-queue-add! pq 79)
;; pq
;; (priority-queue-poll! pq)
;; pq
;; (priority-queue-poll! pq)
;; pq
;; (priority-queue-poll! pq)
;; pq
;; (priority-queue-poll! pq)
;; pq
;; (priority-queue-poll! pq)



;; ;; (priority-queue-add! pq "S")
;; ;; (priority-queue-add! pq "N")
;; ;; (priority-queue-add! pq "G")
;; ;; (priority-queue-add! pq "H")
;; ;; (priority-queue-add! pq "P")
;; ;; (priority-queue-add! pq "N")
;; ;; (priority-queue-add! pq "E")
;; ;; (priority-queue-add! pq "I")
;; ;; (priority-queue-add! pq "H")
;; ;; (priority-queue-add! pq "G")
;; ;; (priority-queue-add! pq 101)
;; ;; (priority-queue-add! pq 3)
;; ;; (priority-queue-add! pq 200)
;; ;; (priority-queue-add! pq 1)
;; ;; (priority-queue-add! pq 1000)
;; ;; (priority-queue-add! pq 1001)
;; pq



;; ;; (define (in-heap heap)
;; ;;   (make-do-sequence
;; ;;    (lambda ()
;; ;;      (values
;; ;;       priority-queue-peek-max
;; ;;       (lambda (pq) (priority-queue-remove-max! pq) pq)
;; ;;       pq
;; ;;       (negate priority-queue-empty?)
;; ;;       #f
;; ;;       #f))))

;; ;; (define (heap=? a b equal-wrapper?)
;; ;;   (and
;; ;;    (equal-wrapper? (priority-queue-ordering a) (priority-queue-ordering b))
;; ;;    (= (flexvector-length (priority-queue-contents a))
;; ;;       (flexvector-length (priority-queue-contents b)))
;; ;;    (let ([fva (flexvector-copy (priority-queue-contents a))]
;; ;;          [fvb (flexvector-copy (priority-queue-contents b))])
;; ;;      (let loop ()
;; ;;        (cond
;; ;;          ((flexvector-empty? fva) #t)
;; ;;          ((equal-wrapper? (flexvector-ref fva 0) (flexvector-ref fvb 0))
;; ;;           (heap-remove-max! fva (priority-queue-ordering a))
;; ;;           (heap-remove-max! fvb (priority-queue-ordering b))
;; ;;           (loop))
;; ;;          (else #f))))))


