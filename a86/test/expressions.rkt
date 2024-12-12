#lang racket
(require rackunit "../ast.rkt" "../interp.rkt")

(define (ev e)
  (asm-interp (prog (Global 'entry) (Label 'entry) (Mov 'rax e) (Ret))))

(check-equal? (ev '(<< 1 4)) (arithmetic-shift 1 4))
(check-equal? (ev '(<< 1 (+ 2 2))) (arithmetic-shift 1 (+ 2 2)))
(check-equal? (ev '(! 0)) 1)
(check-equal? (ev '(~ 0)) -1)

(when (nasm-version-2.15+?)
  (check-equal? (ev '(< 1 2)) 1)
  (check-equal? (ev '(< 2 1)) 0)
  (check-equal? (ev '(? 1 2 3)) 2)
  (check-equal? (ev '(? 0 2 3)) 3)
  (check-equal? (ev '(? 8 2 3)) 2))
