#lang racket
(require rackunit "../ast.rkt" "../interp.rkt")

(check-equal?
 (asm-interp (prog (Global 'entry)
                   (Label 'entry)
                   (Mov 'rax 42)
                   (Ret)))
 42)
