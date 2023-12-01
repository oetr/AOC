#lang racket

(define data (file->lines "day3.txt"))

(with-input-from-file "day3.txt"
  (lambda ()
    (define contents (read-line))
    (define half (/ (string-length contents) 2))
    (define compartment-1 (substring contents 0 half))
    (define compartment-2 (substring contents half))
    (let loop (c compartment)
      (
      (string-contains? 
    

(for/list ([contents data])
    (define half (/ (string-length contents) 2))
    (define compartment-1 (substring contents 0 half))
    (define compartment-2 (substring contents half))

(define (duplicates s1 s2)
  (let loop ()
  (for ([c1 s1])
    (string-contains? s2 c1))

  
  ;; find duplicates
  
  
  )
  
