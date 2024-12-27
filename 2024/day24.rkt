#lang racket

(require advent-of-code
         "./dot.rkt")

(define input (fetch-aoc-input (find-session) 2024 24 #:cache #true))

(struct wire (value label) #:transparent #:mutable)
(struct gate (in1 op in2 out) #:mutable #:transparent)

(define-values (wires gates)
  (let ([data (string-split input "\n\n")])
    (define wires (for/hash ([initial (string-split (car data) "\n")])
                    (define name-val (cdr (regexp-match #px"(.+): ([0-9]+)" initial)))
                    (cons (car name-val) (string->number (cadr name-val)))
                    (values (car name-val)
                            (string->number (cadr name-val)))))
    (define gates
      (for/fold ([gates '()])
                ([line (string-split (cadr data) "\n")])
        (match (cdr (regexp-match #px"(.+) (.+) (.+) -> (.+)" line))
          [(list in1 op-label in2 out)
           (define op
             (match op-label
               ["AND" bitwise-and]
               ["OR" bitwise-ior]
               ["XOR" bitwise-xor]))
           (define g (gate in1 op in2 out))
           (cons g gates)])))
    (values wires gates)))

(define (update-waiting-on waiting-on in gate)
  (hash-update waiting-on
               in
               (λ (old) (set-add old gate))
               (lambda () (set))))


(define (resolve wires waiting-on new-label)
  (let loop ([labels-to-check (list new-label)]
             [wires wires]
             [waiting-on waiting-on])
    (cond [(empty? labels-to-check) (values wires waiting-on)]
          [else
           (define label (car labels-to-check))
           (define label-dependencies (set->list (hash-ref waiting-on label '())))
           (define-values (resolved-labels new-wires)
             (for/fold ([labels '()]
                        [new-wires '()])
                       ([dependency label-dependencies]
                        #:do [(define in1 (hash-ref wires (gate-in1 dependency) #f))
                              (define in2 (hash-ref wires (gate-in2 dependency) #f))]
                        #:when (and in1 in2))
               (define value ((gate-op dependency) in1 in2))
               (define label (gate-out dependency))
               (define out-exists? (hash-ref wires label #f))
               (cond [out-exists?
                      (values (cons label labels) new-wires)]
                     [else
                      (define new-wire (wire value label))
                      (values (cons label labels) (cons new-wire new-wires))])))
           (cond [(empty? resolved-labels)
                  ;; update waiting on
                  (loop (cdr labels-to-check) wires waiting-on)]
                 [else
                  (loop (append resolved-labels (cdr labels-to-check))
                        (for/fold ([wires wires])
                                  ([wire new-wires])
                          (hash-set wires (wire-label wire) (wire-value wire)))
                        (hash-remove waiting-on label))])])))

(define (evaluate gates wires)
  (let loop ([wires wires]
             [waiting-on (hash)]
             [gates gates])
    (cond [(empty? gates) wires]
          [else
           (define gate (car gates))
           (define in1 (hash-ref wires (gate-in1 gate) #f))
           (define in2 (hash-ref wires (gate-in2 gate) #f))
           
           (match (list in1 in2)
             [(list #f #f)
              (loop wires
                    (update-waiting-on (update-waiting-on waiting-on (gate-in1 gate) gate)
                                       (gate-in2 gate) gate)
                    (cdr gates))]
             [(list #f _)
              (loop wires
                    (update-waiting-on waiting-on (gate-in1 gate) gate)
                    (cdr gates))]
             [(list _ #f)
              (loop wires
                    (update-waiting-on waiting-on (gate-in2 gate) gate)
                    (cdr gates))]
             [(list v1 v2)
              (define-values (new-wires new-waiting-on)
                (resolve
                 (hash-set wires (gate-out gate) ((gate-op gate) v1 v2))
                 waiting-on
                 (gate-out gate)))
              (loop new-wires new-waiting-on (cdr gates))])])))


;; part 1
(time
 (let ([z-wires (for/list ([(label value) (evaluate gates wires)]
                           #:when (string-prefix? label "z"))
                  (wire value label))])
   (for/fold ([n 0])
             ([(bit i) (in-indexed (sort z-wires
                                         (λ (a b)
                                           (string<? (wire-label a) (wire-label b)))))])
     (bitwise-ior n (arithmetic-shift (wire-value bit) i)))))
;; cpu time: 0 real time: 0 gc time: 0

;; part 2
(define (->dot wires gates)
  (define id 0)
  (foldr string-append ""
         (flatten (list "digraph {"
                        "rankdir=\"TB\";"
                        (for/list ([gate gates])
                          (define op (gate-op gate))
                          (define result
                            (list
                             (~a id " [label=\"" (cond [(eq? bitwise-and op) "AND"]
                                                       [(eq? bitwise-ior op) "OR"]
                                                       [(eq? bitwise-xor op) "XOR"]
                                                       [else (error '->dot "unknown op!")])
                                 "\" ];" )
                             (~a (gate-in1 gate) " -> " id ";")
                             (~a (gate-in2 gate) " -> " id ";")
                             (~a id " -> " (gate-out gate) ";")
                             ))
                          (set! id (add1 id))
                          result)
                        "}"))))

;; (draw-dot (->dot wires gates)
;;           #:size 10
;;           #:path "./out.pdf"
;;           #:type "pdf"
;;           #:layout "dot")

;; index wires to gates; inputs -> gate (forward), and gate<-output (backward)
(define-values (input->gates output->gate gates2)
  (let ([data (string-split input "\n\n")])
    (define (add-gate h k g)
      (hash-update h k (λ (a) (set-add a g)) set))
    (for/fold ([input->gate (hash)]
               [output->gate (hash)]
               [gates (set)])
              ([line (string-split (cadr data) "\n")])
      (define gate-args (cdr (regexp-match #px"(.+) (.+) (.+) -> (.+)" line)))
      (define g (apply gate gate-args))
      (values (add-gate (add-gate input->gate (gate-in1 g) g)
                        (gate-in2 g) g)
              (hash-set output->gate (gate-out g) g)
              (set-add gates g)))))

(define n-inputs
  (for/sum ([(wire _) (in-hash input->gates)]
            #:when (string-prefix? wire "x"))
    1))

(define (inputs-eq? gate in1 in2)
  (define gin1 (gate-in1 gate))
  (define gin2 (gate-in2 gate))
  (or (and (string=? in1 gin1)
           (string=? in2 gin2))
      (and (string=? in1 gin2)
           (string=? in2 gin1))))


(define bad-wires
  (for/fold ([result (set)]) ([g gates2])
    (define op (gate-op g))
    (define bad-wires
      (match op
        ["AND"
         ;; output should go to OR
         (define out-wire (gate-out g))
         (define out (hash-ref input->gates out-wire #f))
         (when (or (string-prefix? out-wire "z")
                   (not (set? out))
                   (not (= 1 (set-count out)))
                   (not (string=? "OR" (gate-op (car (set->list out))))))
           (unless (and (set? out) (inputs-eq? g "y00" "x00"))
             out-wire))
         ]
        ["OR"
         ;; output should go to XOR and AND
         (define out-wire (gate-out g))
         (define out (hash-ref input->gates out-wire #f))
         (define bad?
           (cond [(not (set? out))
                  #t]
                 [(not (= 2 (set-count out)))
                  #t]
                 [else
                  (define sorted (sort (set->list out) (λ (a b) (string<? (gate-op a) (gate-op b)))))
                  (define o (not (and (string=? "AND" (gate-op (car sorted)))
                                      (string=? "XOR" (gate-op (cadr sorted))))))
                  o]))
         (when bad? out-wire)
         ]
        ["XOR"
         (define out-wire (gate-out g))
         (define out (hash-ref input->gates out-wire #f))
         ;;(printf "~a~n" out)
         (define bad-wire
           (cond
             [(not out) #f]
             [(not (= 2 (set-count out))) out-wire]
             [else
              (define sorted (sort (set->list out) (λ (a b) (string<? (gate-op a) (gate-op b)))))
              (define o (and (string=? "AND" (gate-op (car sorted)))
                             (string=? "XOR" (gate-op (cadr sorted)))))
              (cond [(not o) out-wire]
                    [else
                     (define out-out (hash-ref input->gates (gate-out (cadr sorted)) #f))
                     (when out-out
                       (gate-out (cadr sorted)))])]
             ))
         bad-wire
         ]
        [_ '()]))
    (if (string? bad-wires)
        (set-add result bad-wires)
        result)))

;; part 2
(apply (curry ~a #:separator ",") (sort (set->list (set-remove bad-wires "z45")) string<?))

;; bwd,cpm,gpr,krs,nks,z10,z21,z33    - wrong submission
;; cpm,ghp,gpr,krs,nks,z10,z21,z33
