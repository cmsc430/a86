#lang info
(define version "1.0")
(define collection 'multi)
(define deps (list "base" "rackunit" "redex-lib" "redex-gui-lib"))

(define build-deps '("racket-doc"
                     "sandbox-lib"
                     "scribble-lib"))
