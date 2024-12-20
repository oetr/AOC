#lang racket

(require advent-of-code)

(define input (fetch-aoc-input (find-session) 2024 20 #:cache #true))

(define (input->state input)
  (define start #f)
  (define end #f)
  (values 
   (for/vector ([(line y) (in-indexed (string-split input "\n"))])
     (for/vector ([(char x) (in-indexed line)])
       (match char
         [#\S (set! start (list x y)) #\.]
         [#\E (set! end (list x y)) #\.]
         [_ char])))
   start
   end))

(define-values (grid start end) (input->state input))

(define (! grid x y v)
  (vector-set! (vector-ref grid y)
               x v))

(define (!p grid p v)
  (vector-set! (vector-ref grid (cadr p))
               (car p) v))

(define (&p grid p)
  (vector-ref (vector-ref grid (cadr p)) (car p)))


(define (posn+ p1 p2)
  (map + p1 p2))

(define (posn= p1 p2)
  (andmap = p1 p2))

(define (in-grid? grid p)
  (define x (car p))
  (define y (cadr p))
  (and (> x -1)
       (> y -1)
       (< x (vector-length (vector-ref grid 0)))
       (< y (vector-length grid))))

(define (is-wall? grid p)
  (char=? #\# (vector-ref (vector-ref grid (cadr p)) (car p))))

(define (is-safe? grid p)
  (char=? #\. (vector-ref (vector-ref grid (cadr p)) (car p))))

(define (get-safe-adj grid p)
  (for/fold ([adj '()])
            ([p (map (curry posn+ p) '((0 1) (0 -1) (1 0) (-1 0)))])
    (cond [(in-grid? grid p)
           (define v (&p grid p))
           (if (char=? #\. v)
               (cons p adj)
               adj)]
          [else adj])))

;; part 1
(define (get-path grid start end)
  (define explored (mutable-set))
  (let loop ([path (list start)])
    (define pos (car path))
    (set-add! explored pos)
    (cond [(posn= pos end) (reverse path)]
          [else
           (define next-pos (car (filter-not (λ (p) (set-member? explored p)) (get-safe-adj grid pos))))
           (loop (cons next-pos path))])))

(define (get-all-cheat-positions grid p)
  (for/fold ([adj '()])
            ([cheat-moves '(
                            ((0 1) (0 1))
                            ((0 -1) (0 -1))
                            ((1 0) (1 0))
                            ((-1 0) (-1 0))) ;; (1 1) (1 -1) (-1 1) (-1 -1)
                         ;;(0 1) (0 -1) (1 0) (-1 0)
                     ])
            ;;([p (map (curry posn+ p) '((0 2) (0 -2) (2 0) (-2 0)))])
    (define one (posn+ p (car cheat-moves)))
    (define two (posn+ one (cadr cheat-moves)))
    (cond [(and
            (in-grid? grid one)
            (is-wall? grid one)
            (in-grid? grid two)
            (is-safe? grid two))
           (cons two adj)]
          [else adj])))

;; for each position on the path, try to cheat, see where we end up, adn save the cost
(define path (get-path grid start end))

;; index each subpath based on cost
(define subpath-index
  (let ([cost-so-far-index
         (let loop ([h (hash)]
                    [path (reverse path)]
                    [forward-cost 0])
           (cond ([empty? path] h)
                 (else (loop (hash-set h (car path) (list forward-cost))
                             (cdr path)
                             (+ forward-cost 1)))))])
    (let loop ([h cost-so-far-index]
               [path path]
               [backward-cost 0])
      (cond ([empty? path] h)
            (else (loop (hash-set h (car path) (cons backward-cost (hash-ref h (car path))))
                        (cdr path)
                        (+ backward-cost 1)))))))

;; -1 because the start position adds no cost
(define original-cost (- (length path) 1))

(define path-costs-diff>=100
  (filter (λ (n) (>= n 100))
          (sort (flatten (for/list ([cheat-start path])
                           (define cheat-end-positions (get-all-cheat-positions grid cheat-start))
                           (for/list ([cheat-end cheat-end-positions])
                             (define cost-start (car (hash-ref subpath-index cheat-start)))
                             (define cost-from-end (cadr (hash-ref subpath-index cheat-end)))
                             (- original-cost (+ cost-start 2 cost-from-end)))))
                <)))

;; part 1
(length path-costs-diff>=100)

(struct state (posn cost visited))


(define (get-permissible grid p cost max-cost visited)
  (for/fold ([adj '()])
            ([candidate-p (map (curry posn+ p) '((0 1) (0 -1) (1 0) (-1 0)))]
             #:when (and (in-grid? grid candidate-p)
                         (not (set-member? candidate-p visited))))
                         ;; (or (and (= cost 0) (is-wall? grid candidate-p))
                         ;;     (and (= cost (- max-cost 1)) (is-safe? grid candidate-p))
                         ;;     (and (> cost 0) (< cost (- max-cost 1))))))
    (cons candidate-p adj)))


(define (get-cheats grid p max-cost)
  (define initial-frontier (get-permissible grid p 0 max-cost (set)))
  (define-values (_ solutions frontier)
    (for/fold ([visited (set p)]
               [solutions '()]
               [frontier (list->set initial-frontier)])
              ([cost (in-range 1 (+ max-cost 1))]
               #:break (set-empty? frontier))
      (define-values (new-visited new-solutions good-to-continue)
        (for/fold ([new-visited visited]
                   [new-solutions '()]
                   [good-to-continue (set)])
                  ([p frontier])
          (define visited (set-add new-visited p))
          (cond [(and (is-safe? grid p) (not (set-member? new-visited p)))
                 (values visited
                         (cons (cons cost p) new-solutions)
                         (set-add good-to-continue p))]
                [(not (set-member? new-visited p))
                 (values visited
                         new-solutions
                         (set-add good-to-continue p))]
                [else
                 (values visited
                         new-solutions
                         good-to-continue)])))
      (define new-frontier
        (for/fold ([new-frontier (set)])
                  ([new-p good-to-continue])
          (set-union new-frontier (list->set (get-permissible grid new-p cost max-cost visited)))))
      (values
       new-visited
       (append new-solutions solutions)
       (set-union new-frontier))))
  solutions)


(define max-cost 20)
(define costs2
  (sort (flatten (for/list ([(cheat-start i) (in-indexed path)])
                   (define cheat-end-positions (get-cheats grid cheat-start max-cost))
                   (for/list ([cheat-end cheat-end-positions])
                     (define cheat-cost (car cheat-end))
                     (define cost-start (car (hash-ref subpath-index cheat-start)))
                     (define cost-from-end (cadr (hash-ref subpath-index (cdr cheat-end))))
                     (- original-cost (+ cost-start cheat-cost cost-from-end)))))
        <))

(length (filter (λ (n) (>= n 100)) costs2))
