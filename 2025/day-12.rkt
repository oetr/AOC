#lang racket

(require advent-of-code)

(define input (fetch-aoc-input (find-session) 2025 12 #:cache #true))

(define test-input "0:
###
##.
##.

1:
###
##.
.##

2:
.##
###
##.

3:
##.
###
##.

4:
###
#..
###

5:
###
.#.
###

4x4: 0 0 0 0 2 0
12x5: 1 0 1 0 2 2
12x5: 1 0 1 0 3 2")

(struct region (cols rows counts) #:transparent)

;; rotate clockwise, only works for square matrices
(define (rotate-shape shape)
  (define rows (vector-length shape))
  (define cols (vector-length (vector-ref shape 0)))
  (for/vector ([row rows])
    (for/vector ([col cols])
      (@ shape (- cols col 1) row))))

;; flip along the vertical axis; only works for square matrices
(define (flip-shape shape)
  (define rows (vector-length shape))
  (define cols (vector-length (vector-ref shape 0)))
  (for/vector ([row rows])
    (for/vector ([col cols])
      (@ shape row (- cols col 1)))))

(define (all-shape-variants shape)
  (for/fold ([result (list shape)]
             [current-shape shape]
             #:result (remove-duplicates result))
            ([op (flatten
                  (list (make-list 3 rotate-shape)
                        flip-shape
                        (make-list 3 rotate-shape)))])
    (define new-variant (op current-shape))
    (values (cons new-variant result)
            new-variant)))


(define (parse-input input)
  (define split (string-split input "\n\n"))
  (define-values (shapes-str regions-str) (split-at split (sub1 (length split))))
  (define shapes (vector-map
                  all-shape-variants
                  (for/vector ([shape shapes-str])
                    (for/vector ([row (drop (string-split shape "\n") 1)])
                      (list->vector (string->list row))))))
  (define regions (for/list ([r (string-split (car regions-str) "\n")])
                    (define split (string-split r ": "))
                    (define wh (string-split (first split) "x"))
                    (region (string->number (second wh))
                            (string->number (first wh))
                            (list->vector (map string->number (string-split (second split) " "))))))
  (values shapes regions))

(define (@ mat r c)
  (vector-ref (vector-ref mat r) c))

(define (! mat r c val)
  (vector-set! (vector-ref mat r) c val))



;; 1) place requested shape in a position
;;    - fits? go to 2)
;;    - does not fit? - find another position
;;    - all positions have been tried? -- backtrack
;;    - all of all shapes positions have been tried?  - UNSAT
;; 2) more shapes to place? goto1
;;    - else we are done --- SAT!

(define (make-matrix rows cols (fill? #\.))
  (for/vector ([row rows])
    (for/vector ([col cols])
      fill?)))

(define (shape-fits? matrix row col shape)
  (define rows (vector-length shape))
  (define cols (vector-length (vector-ref shape 0)))
  (define m-rows (vector-length matrix))
  (define m-cols (vector-length (vector-ref matrix 0)))
  (for*/and ([r rows]
             [c cols]
             #:when (char=? #\# (@ shape r c)))
    (and (< (+ row r) m-rows)
         (< (+ col c) m-cols)
         (char=? #\. (@ matrix (+ row r) (+ col c))))))


(define (add-shape! matrix row col shape)
  (define rows (vector-length shape))
  (define cols (vector-length (vector-ref shape 0)))
  (define m-rows (vector-length matrix))
  (define m-cols (vector-length (vector-ref matrix 0)))
  (for* ([r rows]
         [c cols]
         #:when (char=? #\# (@ shape r c)))
    (! matrix (+ row r) (+ col c) #\#)))

(define (add-shape matrix row col shape)
  (define rows (vector-length shape))
  (define cols (vector-length (vector-ref shape 0)))
  (define m-rows (vector-length matrix))
  (define m-cols (vector-length (vector-ref matrix 0)))
  (for/vector ([(m-row r) (in-indexed (in-vector matrix))])
    (for/vector ([(val c) (in-indexed (in-vector m-row))])
      (if (and (>= r row) (< r (+ row rows))
               (>= c col) (< c (+ col cols)))
          (let ([v (@ shape (- r row) (- c col))])
            (if (char=? v #\.)
                val
                (if (char=? val #\#)
                    (error 'add-shape "should not be possible!")
                    v)))
          val))))

;;(add-shape (make-matrix 10 10) 3 3 (list-ref (vector-ref shapes 0) 0))
          

(define (remove-shape! matrix row col shape)
  (define rows (vector-length shape))
  (define cols (vector-length (vector-ref shape 0)))
  (define m-rows (vector-length matrix))
  (define m-cols (vector-length (vector-ref matrix 0)))
  (for*/and ([r rows]
             [c cols]
             #:when (char=? #\# (@ shape r c)))
    (! matrix (+ row r) (+ col c) #\.)))


;;(define w (make-matrix 4 4))


;; (shape-fits? w 1 1 (list-ref (vector-ref shapes 0) 0))

;; (! w 1 0 #\#)
;; (! w 2 0 #\#)

;; exclude regions that cannot possibly fit the requested presents
(define (region-ok? reg)
  ;; each present needs 7 tiles
  (define required-tiles (* 7 (for/sum ([count (in-vector (region-counts reg))])
                                count)))
  (define region-tiles (* (region-cols reg) (region-rows reg)))
  (<= required-tiles region-tiles))



(struct work (row col shape) #:transparent)

(define (make-work-stream shapes rows cols shape-id)
  (for*/stream ([shape (vector-ref shapes shape-id)]
                [row (in-range (- rows 2))]
                [col (in-range (- cols 2))])
    (work row col shape)))

;; (define s (make-work-stream shapes 4 4 0))
;; (for ([str s]) (printf "~a~n" str))

;;(stream-ref (for*/stream ([i '(1 2 3)]) (displayln i) (* i i)) 1)

(define (decrement-count a-count)
  (for/fold ([result empty]
             [done? #f]
             #:result (reverse result))
            ([c a-count])
    (cond
      [(or done? (zero? c)) (values (cons c result) done?)]
      [else
        (values (cons (- c 1) result) #t)])))

(define (print-shape shape)
  (for ([row shape])
    (printf "   ")
    (for ([val row])
      (printf "~a " val))
    (printf "\n")))


(define (fit shapes region)
  (define rows (region-rows region))
  (define cols (region-cols region))
  (define counts (region-counts region))
  (define counter 10000000000)
  (printf "REGION: ~a~n" region)

  (define first-shape-id (for/first ([(count i) (in-indexed counts)] #:unless (zero? count)) i))
  (define first-shape-stream (make-work-stream shapes rows cols first-shape-id))

  (struct state (counts world))
  
  (let loop ([backtrack empty]
             [todo first-shape-stream]
             [counts (list counts)]
             [worlds (list (make-matrix rows cols))])
    (define count (car counts))
    (set! counter (- counter  1))

    ;; (when (and (zero? (modulo counter 1000000000))
    ;;            (not (stream-empty? todo)))
    ;;   (define world (car worlds))
    ;;    (define candidate (stream-first todo))
    ;;    (define row (work-row candidate))
    ;;    (define col (work-col candidate))
    ;;    (define shape (work-shape candidate))
    ;;    ;;(printf "TRY ~a, ~a   --- ~a --- ~a~n" row col count candidate)
    ;;    (print-shape shape)
    ;;    (print-shape world))
    (cond
      [(< counter 0) 'EXIT]
      [(zero? (for/sum ([c count]) c))
       (print-shape (car worlds))
       #t]
      [(stream-empty? todo) ;; no fitting shape found -- backtrack now
       ;;(printf "EMPTY\n")
       (if (empty? backtrack)
           #f
           (let ([new-shape-work (car backtrack)])
             ;;(printf "BACKTRACKING\n")
             (loop (cdr backtrack)
                   new-shape-work
                   (cdr counts)
                   (cdr worlds))))]
      [else
       ;;(printf ";; ELSE\n")
       (define world (car worlds))
       (define candidate (stream-first todo))
       (define row (work-row candidate))
       (define col (work-col candidate))
       (define shape (work-shape candidate))
       ;;(printf "TRY ~a, ~a   --- ~a --- ~a~n" row col count candidate)
       (cond
         [(shape-fits? world row col shape)
          ;; (printf "FITS!\n")
          ;; (print-shape shape)
          ;; (print-shape world)
          (define next-count (decrement-count count))
          (define next-shape-id (for/first ([(count i) (in-indexed next-count)] #:unless (zero? count)) i))
          (if next-shape-id
              (loop (cons (stream-rest todo) backtrack)
                    (make-work-stream shapes rows cols next-shape-id)
                    (cons next-count counts)
                    (cons (add-shape world row col shape) worlds))
              #t)]
         [else
          ;;(printf "else\n")
          (loop backtrack
                (stream-rest todo)
                counts
                worlds)])])))

(define-values (shapes regions) (parse-input input))
(define ok-regions (filter region-ok? regions))
(fit shapes (list-ref ok-regions 4))
(for/list ([r ok-regions])
  (fit shapes r))

;; How generous! Not a single tight fit here, so the brute-force approach just works!
