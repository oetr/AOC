#lang racket

(require syntax/parse/define)

(provide (all-defined-out))

;; define/memoize and memoize are taken from https://github.com/kstrafe/memo
;; and modified here. Their original licence is LGPL.


(define-syntax-parser define/memoize
  ([_ (name:id formals:id ...+)
      (~optional (~seq #:hash hsh:expr) #:defaults [(hsh #'make-hash)])
      body:expr ...+]
   #'(define name (memoize-first (formals ...) #:hash hsh body ...))))

;; only memoizes the first parameter
(define-syntax-parser memoize-first
  ([_ (param:id ...)
      (~optional (~seq #:hash hsh:expr) #:defaults [(hsh #'make-hash)])
      body:expr ...+]
   #'(let ([cache (hsh)]
           [unique-token (gensym 'memoize-)])
       (case-lambda
         [() cache]
         [(param ...)
          (let ([data (list param ...)])
            (let ([result (hash-ref cache (car data) unique-token)])
              (if (and (symbol? result) (symbol=? unique-token result))
                  (let ([value ((lambda () body ...))])
                    (hash-set! cache (car data) value)
                    value)
                  result)))]))))
