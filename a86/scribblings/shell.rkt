#lang racket
(provide shell shell-result)
(require scribble/base scribble/core scribble/html-properties)

;; calls proc and produces stdout appendde w/ stderr as a string
(define (with-output-to-string/err proc)
  (define os "")
  (define es "")
  (define r #f)
  (set! os (call-with-output-string
            (lambda (o)
              (set! es
                    (call-with-output-string
                     (λ (e)
                       (parameterize ([current-output-port o]
                                      [current-error-port e])
                         (set! r (proc)))))))))
  ; (unless r (error (string-append os es)))
  (string-append os es))

(define (shell-result c)
  (with-output-to-string/err (λ () (system #:set-pwd? #t c))))

(define (shell . cs)
  (match cs
    ['() ""]
    [(cons c cs)
     (string-append "> " c "\n"
                    (with-output-to-string/err (λ () (system #:set-pwd? #t c)))
                    (apply shell cs))]))

