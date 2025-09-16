#lang racket
(provide pre-installer)
(define (pre-installer _)
  (unless (eq? 'x86_64 (system-type 'arch))
    (error 'a86-installer
           "This library requires an x86_64 installation of Racket; yours is a ~a (~a)."
           (system-type 'arch)
           (system-type 'os))))

