#lang racket
(require rackunit "../ast.rkt" "../interp.rkt")

;; Check that using named sections are OK
(check-equal?
 (asm-interp
  (prog (Text 'foo)
        (Global 'entry)
        (Label 'entry)
        (Mov 'rax 42)
        (Ret)))
 42)

;; Check that using named sections are OK
(check-equal?
 (asm-interp
  (prog (Text 'foo)
        (Global 'entry)
        (Label 'entry)
        (Lea 'rax 'd)
        (Mov 'rax (Offset 'rax 0))
        (Ret)
        (Data 'bar)
        (Label 'd)
        (Dq 42)))
 42)

;; Check that align works
(check-equal?
 (bitwise-and #b11111
              (asm-interp
               (prog (Global 'entry)
                     (Label 'entry)
                     (Align 32)
                     (Label 'l)
                     (Lea 'rax 'l)
                     (Ret))))
 0)
