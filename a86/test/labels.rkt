#lang racket
(require rackunit "../ast.rkt" "../interp.rkt")

;; Check that using label names that conflict with instruction names is fine
(check-equal?
 (asm-interp
  (prog (Global 'entry)
        (Label 'entry)
        (Mov 'rax 42)
        (Label 'mov)
        (Sub 'rax 1)
        (Cmp 'rax 0)
        (Jne 'mov)
        (Ret)))
 0)
        
;; Check that you can use labels names that coincide with registers with $
(check-equal?
 (asm-interp
  (prog (Global 'entry)
        (Label 'entry)
        (Mov 'rax 42)
        (Label 'rax)
        (Sub 'rax 1)
        (Cmp 'rax 0)
        (Jne ($ 'rax))
        (Ret)))
 0)

;; Check that $ can be used anywhere a label is expected
(check-equal?
 (asm-interp
  (prog (Global ($ 'entry))
        (Label ($ 'entry))
        (Mov 'rax 42)
        (Label ($ 'loop))
        (Sub 'rax 1)
        (Cmp 'rax 0)
        (Jne ($ 'loop))
        (Ret)))
 0)

;; Check that other reserved keywords is OK too
(check-equal?
 (asm-interp
  (prog (Global 'entry)
        (Label 'entry)
        (Mov 'rax 42)
        (Label 'pragma)
        (Sub 'rax 1)
        (Cmp 'rax 0)
        (Jne 'pragma)
        (Ret)))
 0)

(check-equal?
 (asm-interp
  (prog (Global 'entry)
        (Label 'entry)
        (Mov 'rax 42)
        (Label 'section)
        (Sub 'rax 1)
        (Cmp 'rax 0)
        (Jne 'section)
        (Ret)))
 0)
