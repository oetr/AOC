#lang racket

(require racket/draw)

(provide (all-defined-out))


(define (draw-dot str
                  #:dpi (dpi 100)
                  #:size (size #f)
                  #:path (path #f)
                  #:type (type "png")
                  #:layout (layout #f))
  (parameterize ([current-custodian (make-custodian)])
    (let ([p-stdin (open-input-string str)]
          [out (open-output-bytes)]
          [err (open-output-bytes)])
      (define proc-data
        (process/ports out p-stdin err
                       (string-join
                        (list "dot"
                              (if layout (~a "-K" layout) "")
                              (~a "-T" type)
                              (~a "-Gdpi=" dpi)
                              (if size (~a "-Gsize=" size) "")))))
      (define control (list-ref proc-data 4))
      (control 'wait)
      (define errors (get-output-string err))
      (unless (string=? "" errors)
        (error 'draw-dot "errors: ~s~n" errors))
      (define data (get-output-bytes out))
      (if path
          (with-output-to-file path
            #:exists 'truncate/replace
            (lambda () (display data)))
          (make-object bitmap% (open-input-bytes data))))))

