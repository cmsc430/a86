#lang racket
(provide (all-defined-out))
(require (for-syntax racket/runtime-path racket/base racket/file pkg/lib)
         (for-meta 2 racket/base pkg/lib))
(require scribble/manual racket/runtime-path pkg/lib)
(require (for-label (except-in racket compile) a86))



(begin-for-syntax
  (define-runtime-path a86 (build-path (pkg-directory "a86") "a86")))

(define-runtime-path a86 (build-path (pkg-directory "a86") "a86"))

(define-syntax (filebox-include stx)
  (syntax-case stx (a86)
    [(_ form lang fn)
     (parameterize ()
       (let ((s (file->string (build-path (syntax-case #'lang (a86)
                                            [a86 a86])
                                          (syntax->datum #'fn)))))
         #`(filebox (link (string-append "code/" fn) (tt fn)) (form #,(datum->syntax #'form s)))))]))

(define (save-file f s)
  (with-output-to-file f (λ () (display s)) #:exists 'replace))
