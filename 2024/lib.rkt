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
  ([_ (param:id not-memoized-params ...)
      (~optional (~seq #:hash hsh:expr) #:defaults [(hsh #'make-hash)])
      body:expr ...+]
   #'(let ([cache (hsh)]
           [unique-token (gensym 'memoize-)])
       (case-lambda
         [() cache]
         [(param not-memoized-params ...)
            (let ([result (hash-ref cache param unique-token)])
              (if (and (symbol? result) (symbol=? unique-token result))
                  (let ([value ((lambda () body ...))])
                    (hash-set! cache param value)
                    value)
                  result))]))))
