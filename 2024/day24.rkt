#lang racket

(require advent-of-code
         racket/struct)

(define input (fetch-aoc-input (find-session) 2024 24 #:cache #true))

;; (struct wire (value children label) #:mutable)
;; (struct gate (inputs forward-fn op label) #:mutable)

(struct wire (value label) #:transparent #:mutable)
(struct gate (in1 op in2 out) #:mutable)
;; #:methods gen:custom-write
;; [(define write-proc
;;    (make-constructor-style-printer
;;     (lambda (obj)
;;       (cond [(not (string=? "" (gate-op obj))) (~a 'gate: " " (gate-op obj))]
;;             [(not (string=? "" (gate-label obj))) (~a 'gate: " " (gate-label obj))]
;;             [else 'gate]))
;;     (lambda (obj) (list (list 'value (gate-value obj))
;;                         ))))])

;; (define (make-gate data ( '()) #:op (op "") #:label (label ""))
;;   (gate data void (apply set parents) op label))

;; (define-syntax (define-value stx)
;;   (syntax-parse stx
;;     [(_ name:id data (~optional (~seq #:op op:str)))
;;      #'(define name (make-gate data (~? (~@ #:op op)) #:label (symbol->string 'name)))]
;;     [(_ name:id data parents (~optional (~seq #:op op:str)))
;;      #'(define name (make-gate data parents (~? (~@ #:op op)) #:label (symbol->string 'name)))]))


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
                    ;; (define out (hash-ref wires out-labe (wire 'unknown (mutable-set) out-label)))
                    ;; (define in1 (hash-ref wires in1-label (wire 'unknown (mutable-set) in1-label)))
                    ;; (define in2 (hash-ref wires in2-label (wire 'unknown (mutable-set) in2-label)))
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
    (cond ([empty? labels-to-check] (values wires waiting-on))
          [else
           (define label (car labels-to-check))
           (define dependencies (set->list (hash-ref waiting-on new-label)))
           (define resolved (for/list ([]))
           ;; check if the gate can compute now
           (define g (car gates-to-resolve))
           
           
           
           ])))

(define (evaluate gates wires)
  (let loop ([wires wires]
             [waiting-on (hash)]
             [gates gates])
    (cond [(empty? gates) (values wires waiting-on)]
          [else
           (define gate (car gates))
           (define in1 (hash-ref wires (gate-in1 gate) #f))
           (define in2 (hash-ref wires (gate-in2 gate) #f))
           
           (match (list in1 in2)
             [(list #f #f)
              (loop wires
                    (update-waiting-on (update-waiting-on waiting-on in1 gate)
                                       in2 gate)
                    (cdr gate))]
             [(list #f _)
              (loop wires
                    (update-waiting-on waiting-on in1 gate)
                    (cdr gate))]
             [(list _ #f)
              (loop wires
                    (update-waiting-on waiting-on in2 gate)
                    (cdr gate))]
             [(list v1 v2)
              (define new-wires (hash-set wires (gate-out gate) ((gate-op gate) v1 v2)))
              
              (loop 
                    waiting-on
                    (cdr gate))])
              
              
             
              (loop wires (hash-update waiting-on (gate-in1 gate) (λ (old) (set-add old (gate-out gate))))
                    (cdr gate))]
                  ]
           ])
  )

           
    ;;        (set-add! (wire-children in1) g)
    ;;        (set-add! (wire-children in2) g)
    ;;        (hash-set* wires )
           
           
    ;;     (values
    ;;      (hash-set* input->gate
    ;;                 (gate-in1 new-gate)
    ;;                 new-gate
    ;;                 (gate-in2 new-gate) new-gate)
    ;;      (hash-set output->gate
    ;;                (gate-out new-gate)
    ;;                new-gate))))
    ;; (values
    ;;  wires
    ;;  input->gate
    ;;  output->gate)))

;; topo sort the gates
;; (define (topo-sort wires input->gate output->gate)
;;   (struct order (order gate))
;;   (define (aux todo)
;;     (cond [(empty? todo) '()]
;;           [else
;;            (cons todo (aux (set->list (for/set ([in todo])
;;                                         (printf "~a~n" in)
;;                                         (hash-ref output->(gate-out (hash-ref input->gate in))
;;                                       )))]))
;;   (aux (hash-keys wires)))

;;(topo-sort wires input->gate output->gate)

    
  ;;   (define in1 (gate-in1 gate))
  ;;   (define in2 (gate-in2 gate))
  ;;   (define out (gate-out gate))
  ;;   (define is-initial? (hash-ref wires in1))
  ;;   ;;(define is-initial2? (hash-ref wires in2))
  ;;   (cond [is-initial? 1]
  ;;         [
    
    
    
    
  ;; )
