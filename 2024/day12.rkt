#lang racket

(require advent-of-code
         threading
         data/union-find)

(module+ test
  (require rackunit))

(define input (fetch-aoc-input (find-session) 2024 12 #:cache #true))

;; (define input
;;   "RRRRIICCFF
;; RRRRIICCCF
;; VVRRRCCFFF
;; VVRCCCJFFF
;; VVVVCJJCFE
;; VVIVCCJJEE
;; VVIIICJJEE
;; MIIIIIJJEE
;; MIIISIJEEE
;; MMMISSJEEE")

(define grid
  (~> input
      string-split
      (map (lambda~> string->list
                     (map (lambda~> char->integer) _ )
                     list->vector)
           _)
      list->vector))

(define ROWS (vector-length grid))
(define COLS (vector-length (vector-ref grid 0)))


(define (& grid row col)
  (vector-ref (vector-ref grid row)
              col))

(define (rc->i row col cols)
  (+ (* row cols) col))

(define (i->rc i cols)
  (cons (quotient i cols)
        (remainder i cols)))


(define (in-grid? row col rows cols)
  (and (>= row 0) (>= col 0)
       (< row rows) (< col cols)))

(module+ test
  (check-= 32 (rc->i 2 10 11) 0)
  (check-= 202 (rc->i 2 2 100) 0))

(module+ test
  (check-equal? (cons 0 0) (i->rc 0 10))
  (check-equal? (cons 0 1) (i->rc 1 10))
  (check-equal? (cons 0 9) (i->rc 9 10))
  (check-equal? (cons 1 0) (i->rc 10 10))
  (check-equal? (cons 1 1) (i->rc 11 10))
  (check-equal? (cons 1 2) (i->rc 12 10))
  (check-equal? (cons 10 0) (i->rc 100 10))
  )

(define (ccl2 grid)
  (define rows (vector-length grid))
  (define cols (vector-length (vector-ref grid 0)))
  
  (define nodes
    (for/vector ([i (* rows cols)])
      (uf-new i)))
  
  (define adjacent-m
    (list (cons 0 1) (cons 1 0) (cons -1 0) (cons 0 -1)))

  (for* ([row rows]
         [col cols])
    (define label (& grid row col))
    (define u1 (vector-ref nodes (rc->i row col cols)))
    (for [(coord adjacent-m)]
      (define new-row (+ row (car coord)))
      (define new-col (+ col (cdr coord)))
      (when (and (in-grid? new-row new-col rows cols)
                 (= (& grid new-row new-col)
                    label))
        (define i2 (rc->i new-row new-col cols))
        (uf-union! u1 (vector-ref nodes i2))))
    )
  ;; (for/fold ([result (set)])
  ;;           ([node nodes])
  ;;   (set-add result (uf-find node)))
  nodes
  )

;;(set-count (ccl2 grid))

(struct ndata (label area perimeter fence) #:mutable #:transparent)

(define (needs-fence? grid r1 c1 r2 c2 rows cols)
  (define label (& grid r1 c1))
  (or (not (in-grid? r2 c2 rows cols))
      (not (= (& grid r2 c2)
              label))))

(module+ test
  (check-true (needs-fence? grid 0 0 -1 0 ROWS COLS))
  (check-false (needs-fence? grid 0 0 0 1 ROWS COLS))
  (check-false (needs-fence? grid 0 0 1 0 ROWS COLS))
  (check-true (needs-fence? grid 0 0 0 -1 ROWS COLS))
  
  (check-true (needs-fence? grid 1 0 2 0 ROWS COLS))
  (check-true (needs-fence? grid 1 0 1 -1 ROWS COLS))
  (check-false (needs-fence? grid 1 0 0 0 ROWS COLS))
  (check-false(needs-fence? grid 1 0 1 1 ROWS COLS))
  )



(define (form-side? rc1 rc2 rc3 rc4)
  ;; fence normal 1
  (define r1 (car rc1))
  (define c1 (cdr rc1))
  (define r2 (car rc2))
  (define c2 (cdr rc2))
  ;; fence normal 2
  (define r3 (car rc3))
  (define c3 (cdr rc3))
  (define r4 (car rc4))
  (define c4 (cdr rc4))
  ;; normals should equal
  (and (= (- r1 r2) (- r3 r4))
       (= (- c1 c2) (- c3 c4))
       ;; fence normals are exactly 1 point apart
       (or (and (= r1 r3) (= 1 (abs (- c1 c3))))
           (and (= c1 c3) (= 1 (abs (- r1 r3)))))))

(module+ test
  (check-true (form-side? (cons 0 0) (cons -1 0)
                          (cons 0 1) (cons -1 1)))
  (check-true (form-side? (cons 0 0) (cons 1 0)
                          (cons 0 1) (cons 1 1)))
  (check-true (form-side? (cons 0 0) (cons 0 1)
                          (cons 1 0) (cons 1 1)))
  (check-true (form-side? (cons 0 0) (cons 0 -1)
                          (cons 1 0) (cons 1 -1)))
  
  (check-false (form-side? (cons 0 0) (cons 0 1)
                           (cons 2 0) (cons 2 1)))
  (check-false (form-side? (cons 0 0) (cons 0 1)
                           (cons 1 1) (cons 1 0)))
  )

(define nodes (ccl2 grid))

(define all
  (for/fold ([result (hash)])
            ([(node i) (in-indexed nodes)])
    (define id (uf-find node))
    (define rc (i->rc i COLS))
    (define row (car rc))
    (define col (cdr rc))
    (define data (hash-ref result id (ndata (& grid row col) 0 0 '())))
    (set-ndata-area! data (+ (ndata-area data) 1))

    ;; update perimeter
    (for ([adj (list (cons 0 1) (cons 1 0) (cons -1 0) (cons 0 -1))])
      (define r-adj (+ row (car adj)))
      (define c-adj (+ col (cdr adj)))
      (when (needs-fence? grid row col r-adj c-adj ROWS COLS)      
        (set-ndata-perimeter! data (+ (ndata-perimeter data) 1))
        ;; update sides
        (set-ndata-fence! data (cons (cons (cons row col) (cons r-adj c-adj)) (ndata-fence data)))))

    (hash-set result id data)))

;; part 1
;; (for ([(k v) (in-hash all)])
;;   ;;(* (ndata-area v) (ndata-perimeter v)))
;;   (printf "~a: ~a, ~a    ~a\n" (integer->char (ndata-label v)) (ndata-area v) (ndata-perimeter v) (ndata-sides v)))

;; (define (fence->sides coords)

(for/sum ([(k v) (in-hash all)])
  (define fences (list->vector (ndata-fence v)))
  (define groups (for/vector ([i (in-range (vector-length fences))])
                   (uf-new i)))
  (for* ([(p1-p2 i) (in-indexed fences)]
         [j (in-range (+ i 1) (vector-length fences))])
    
    (define p3-p4 (vector-ref fences j))
    (when (form-side? (car p1-p2) (cdr p1-p2)
                      (car p3-p4) (cdr p3-p4))
      (uf-union! (vector-ref groups i) (vector-ref groups j))))
  (define sides (set-count (for/set ([group groups])
                             (uf-find group))))
  
  ;; (printf "~a: ~a, ~a --- ~a\n" (integer->char (ndata-label v)) (ndata-area v) (ndata-perimeter v)
  ;;         sides)
  (* (ndata-area v) sides))
