#lang racket

(require advent-of-code
         threading)

(define input (fetch-aoc-input (find-session) 2024 09 #:cache #true))
;;(define input "2333133121414131402\n")

(struct block (id files free initial-files) #:mutable #:transparent)

(define (make-block id files free)
  (block id (make-list files id) free files))

(define data
  (for/list ([n (string->list (string-trim input))])
    (- (char->integer n) (char->integer #\0))))

(define (data->blocks data)
  (let loop ([data data] [id 0] [acc '()])
    (cond
      [(> (length data) 1)
       (define files (car data))
       (define free (cadr data))
       (define new-block (make-block id files free))
       (loop (cddr data) (+ id 1) (cons new-block acc))]
      [else
       (= (length data) 1)
       (define files (car data))
       (define new-block (make-block id files 0))
       (list->vector (reverse (cons new-block acc)))])))

(define (find-next-free-block blocks (start 0))
  (let loop ([i start])
    (if (> (block-free (vector-ref blocks i)) 0)
        i
        (loop (+ i 1)))))

(define (block-move-free*! dst src)
  (define dst-free (block-free dst))
  (when (zero? dst-free)
    (raise 'block-move-free! "destination block is not free\n"))
  (for ([_ dst-free]
        [src-file (block-files src)])
    ;; copy data
    (set-block-files! dst (append (block-files dst) (list src-file)))
    (set-block-files! src (cdr (block-files src)))
    ;; update free
    (set-block-free! src (+ (block-free src) 1))
    (set-block-free! dst (- (block-free dst) 1))))

(define (compact blocks)
  (define free-block-i 0)
  (for ([block-i (in-range (- (vector-length blocks) 1) -1 -1)]
        #:unless (>= free-block-i block-i))
    (define b (vector-ref blocks block-i))
    (for ([_ (block-files b)])
      (set! free-block-i (find-next-free-block blocks free-block-i))
      (block-move-free*! (vector-ref blocks free-block-i) b))))

(define (checksum blocks)
  (for/sum ([(id i) (in-indexed (flatten (map block-files (vector->list blocks))))])
    (* i id)))


(define (part1)
  ;;(pretty-print blocks)
  (define blocks (data->blocks data))
  (compact blocks)
  (checksum blocks))

(time (part1))
;; cpu time: 107 real time: 107 gc time: 1


;; part 2
(define (make-disk index)
  (define disk-len (for/sum ([d index])
                     (+ (vector-ref d 0) (vector-ref d 1))))
  (define disk (make-vector disk-len -1))
  (let loop ([id 0][data index][vec-idx 0])
    (for ([vi (in-range vec-idx (+ vec-idx (vector-ref (car data) 0)))])
      (vector-set! disk vi id))
    (when (> (length data) 1)
      (loop (+ id 1) (cdr data) (+ vec-idx
                                   (vector-ref (car data) 0)
                                   (vector-ref (car data) 1)))))
  disk)

;; brute-force
(define (compact2! disk index)
  (define id-moved? (make-vector (length index) #f))
  (for ([data (reverse index)]
        [id (in-range (- (length index) 1) 0 -1)])
    (define n (vector-ref data 0))
    (define offset (vector-ref data 2))
    ;; search for free space
    (define-values (n-free free-idx)
      (for/fold ([n-free 0][_ 0])
                ([i (vector-length disk)]
                 #:break (or (>= n-free n)
                             (>= i offset)))
        (define v (vector-ref disk i))
        (cond [(= v -1)
               (values (+ n-free 1) i)]
              [else
               (values 0 0)])))
    (when (= n-free n)
      (for ([src-i (in-range offset (+ offset n))]
            [dst-i (in-range (- free-idx n -1) (+ free-idx 1))])
        (vector-set! disk src-i -1)
        (vector-set! disk dst-i id)))))

(define (checksum2 disk)
  (for/sum ([(id i) (in-indexed disk)])
    (if (= id -1)
        0
        (* i id))))

(define (part2)
  (define index
    (let ([data
           (~> input
               string-trim
               string->list
               (map (lambda~> char->integer (- _ (char->integer #\0))) _))])
      (let loop ([data data][acc '()][offset 0])
        (if (> (length data) 1)
            (loop (cddr data) (cons (vector (car data) (cadr data) offset)
                                    acc)
                  (+ offset (car data) (cadr data)))
            (reverse (cons (vector (car data) 0 offset) acc))))))

  (define disk (make-disk index))
  (compact2! disk index)
  (checksum2 disk))

(time (part2))
;; cpu time: 2199 real time: 2199 gc time: 22
