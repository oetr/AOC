#lang racket

(require advent-of-code
         racket/flonum)


(define input (fetch-aoc-input (find-session) 2024 11 #:cache #true))

(define stones (map string->number (string-split (string-trim input))))

;; part 1, brute force
(define (blink stones)
  (define new-stones
    (let loop ([stones stones][new-stones '()])
      (cond
        [(empty? stones) new-stones]
        [else
         (define stone (car stones))
         (define n-digits (if (< stone 10)
                              1
                              (ceiling (log (+ stone 1) 10))))
         (cond
           [(zero? stone) (loop (cdr stones) (cons 1 new-stones))]
           [(and (not (zero? n-digits)) (even? n-digits))
            (define sn (number->string stone))
            (define left-stone (string->number (substring sn 0 (/ (string-length sn) 2))))
            (define right-stone (string->number (substring sn (/ (string-length sn) 2))))
            (loop (cdr stones) (cons right-stone (cons left-stone new-stones)))]
           [else
            (loop (cdr stones) (cons (* stone 2024) new-stones))])])))
  (reverse new-stones))


(module+ test
  (require rackunit)
  (check-equal? (blink '(125 17)) '(253000 1 7))
  (check-equal? (blink '(0 1 10 99 999)) '(1 2024 1 0 9 9 2021976)))

(time (length (for/fold [(stones (list (list-ref stones 0)))]
                        [(_ 25)]
                (blink stones))))

;; part 2: outstanding craftsmanship :D
(define (blink-once stone)
  (define n-digits (string-length (number->string stone)))
  (cond
    [(zero? stone) 1]
    [(and (not (zero? n-digits)) (even? n-digits))
     (define sn (number->string stone))
     (define left-stone (string->number (substring sn 0 (fl->exact-integer (fl/ (->fl (string-length sn)) (->fl 2))))))
     (define right-stone (string->number (substring sn (fl->exact-integer (fl/ (->fl (string-length sn)) (->fl 2))))))
     (list left-stone right-stone)]
    [else (fl->exact-integer (fl* (->fl stone) (->fl 2024)))]))

(define (blink-n stone n)
  ;; stone . depth
  (let loop ([acc '()] [todo (list (cons stone 0))])
    (cond [(empty? todo) acc]
          [else
           (define todo-left (rest todo))
           (define stone-with-depth (car todo))
           (define stone (car stone-with-depth))
           (define depth (cdr stone-with-depth))
           (cond
             [(= depth n) (loop (cons stone acc) todo-left)]
             [else
              (define stone-s (blink-once stone))
              (cond [(list? stone-s)
                     (loop acc (cons (cons (car stone-s) (+ depth 1))
                                     (cons (cons (cadr stone-s) (+ depth 1))
                                           todo-left)))]
                    [else
                     (loop acc (cons (cons stone-s (+ depth 1))
                                     todo-left))])])])))

(define (get-frequencies lof-nums)
  (for/fold ([h (make-immutable-hash)])
            ([n (in-list lof-nums)])
    (hash-set h n (+ (hash-ref h n 0) 1))))

(define (hash-scalar* h1 n)
  (for/hash ([(k v) (in-hash h1)])
    (values k (* v n))))

(define (hash+ h1 h2)
  (define-values (seen? result)
    (for/fold ([seen? (hash)]
               [result (hash)])
              [((k1 v1) (in-hash h1))]
      (define v2 (hash-ref h2 k1 #f))
      (if (hash-ref h2 k1 #f)
          (values (hash-set seen? k1 #t)
                  (hash-set result k1 (+ v2 v1)))
          (values seen? (hash-set result k1 v1)))))
  (for/fold ([result result])
            ([(k2 v2) (in-hash h2)]
             #:unless (hash-ref seen? k2 #f))
    (hash-set result k2 v2)))

;; a function nobody ever asked for, yet here it is!
;; it computes frequencies for each stone id, while it splits up the computation into chunks of incr and
;; does each "incr" "times" times
(define (compute-frequencies stones incr times)
  (for/fold ([result (get-frequencies stones)]
             [h (make-immutable-hash)])
            ([_ (in-range times)])
    (define-values (new-stones new-h)
      (for/fold [(new-stones '())
                 (h h)]
                ([(stone _) (in-hash result)])
        (define entries (hash-ref h stone #f))
        (cond [entries
               (values (cons entries new-stones)
                       h)]
              [else
               (define new-entries (get-frequencies (blink-n stone incr)))
               (values (cons new-entries new-stones)
                       (hash-set h stone new-entries))])))
    (define st
      (for/list ([(_ v) (in-hash result)]
                 [stone-group (reverse new-stones)])
        (hash-scalar* stone-group v)))
    
    (values (for/fold ([r (make-immutable-hash)])
                      ([hh st])
              (hash+ r hh))
            new-h)))

;; 6 seconds, as opposed to 0s when memoizing 
(define-values (frequencies _)
  (time (compute-frequencies stones 15 5)))

(for/sum ([(_ v) (in-hash frequencies)]) v)
