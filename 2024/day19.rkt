#lang racket

(require advent-of-code)

(define input (fetch-aoc-input (find-session) 2024 19 #:cache #true))

;; (define input
;;   "r, wr, b, g, bwu, rb, gb, br

;; brwrr
;; bggr
;; gbbr
;; rrbgbr
;; ubwu
;; bwurrg
;; brgr
;; bbrgwb")

(define-values (patterns designs)
  (let ([data (string-split input "\n\n")])
    (define patterns (string-split (car data) "\n"))
    (define designs (string-split (cadr data) "\n"))
    (values (string-split (car patterns) ", ") designs)))


(define (make-dfs1)
  ;; manual memoization
  (define memo (make-hash))
  (define (dfs1 design patterns)
    (or (string=? design "")
        (for/or ([pattern patterns]
                 #:when (string-prefix? design pattern))
          (define design-rest (substring design (string-length pattern)))
          (define lookup (hash-ref memo design-rest 'none))
          (when (symbol? lookup)
            (set! lookup (dfs1 design-rest patterns))
            (hash-set! memo design-rest lookup))
          lookup)))
  dfs1)

(define dfs1 (make-dfs1))
(time
 (for/sum ([design designs])
   (match (dfs1 design patterns)
     [#t 1]
     [#f 0])))

;; cpu time: 1 real time: 1 gc time: 0

(define (make-dfs2)
  (define memo (make-hash))
  (define (dfs2 design patterns)
    (cond [(string=? design "") 1]
          [else
           (for/sum ([pattern patterns]
                     #:when (string-prefix? design pattern))
             (define design-rest (substring design (string-length pattern)))
             (define lookup (hash-ref memo design-rest 'none))
             (when (symbol? lookup)
               (set! lookup (dfs2 design-rest patterns))
               (hash-set! memo design-rest lookup))
             lookup)]))
  dfs2)

(define dfs2 (make-dfs2))

(time
 (for/sum ([design designs])
   (dfs2 design patterns)))

;; cpu time: 140 real time: 140 gc time: 1

