#lang info
(define version "1.0")
(define collection "a86")
(define deps (list "base" "rackunit" "redex-lib" "redex-gui-lib"))
(define scribblings '(("scribblings/a86.scrbl")))
(define test-omit-paths '("scribblings/"))
(define pre-install-collection "check-x86.rkt")
