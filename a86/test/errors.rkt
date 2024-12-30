#lang racket
(require rackunit "../ast.rkt")
(check-exn exn:fail?
           (thunk (Mov (Offset 'rax 0) 100)))

;; Checking literal widths
(check-exn exn:fail? (thunk (Mov 'rax (expt 2 64))))
(check-exn exn:fail? (thunk (Mov 'rax (- (add1 (expt 2 63))))))
(check-not-exn       (thunk (Mov 'rax (sub1 (expt 2 64)))))
(check-not-exn       (thunk (Mov 'rax (- (expt 2 63)))))
(check-exn exn:fail? (thunk (Cmp 'rax (expt 2 32))))
(check-exn exn:fail? (thunk (Cmp 'rax (- (add1 (expt 2 31))))))
(check-not-exn       (thunk (Cmp 'rax (sub1 (expt 2 32)))))
(check-not-exn       (thunk (Cmp 'rax (- (expt 2 31)))))
(check-exn exn:fail? (thunk (And 'rax (expt 2 32))))
(check-exn exn:fail? (thunk (And 'rax (- (add1 (expt 2 31))))))
(check-not-exn       (thunk (And 'rax (sub1 (expt 2 32)))))
(check-not-exn       (thunk (And 'rax (- (expt 2 31)))))
(check-exn exn:fail? (thunk (Or 'rax (expt 2 32))))
(check-exn exn:fail? (thunk (Or 'rax (- (add1 (expt 2 31))))))
(check-not-exn       (thunk (Or 'rax (sub1 (expt 2 32)))))
(check-not-exn       (thunk (Or 'rax (- (expt 2 31)))))
(check-exn exn:fail? (thunk (Xor 'rax (expt 2 32))))
(check-exn exn:fail? (thunk (Xor 'rax (- (add1 (expt 2 31))))))
(check-not-exn       (thunk (Xor 'rax (sub1 (expt 2 32)))))
(check-not-exn       (thunk (Xor 'rax (- (expt 2 31)))))
(check-exn exn:fail? (thunk (Push (expt 2 32))))
(check-exn exn:fail? (thunk (Push (- (add1 (expt 2 31))))))
(check-not-exn       (thunk (Push (sub1 (expt 2 32)))))
(check-not-exn       (thunk (Push (- (expt 2 31)))))
(check-exn exn:fail? (thunk (Add 'rax (expt 2 32))))
(check-exn exn:fail? (thunk (Add 'rax (- (add1 (expt 2 31))))))
(check-not-exn       (thunk (Add 'rax (sub1 (expt 2 32)))))
(check-not-exn       (thunk (Add 'rax (- (expt 2 31)))))
(check-exn exn:fail? (thunk (Sub 'rax (expt 2 32))))
(check-exn exn:fail? (thunk (Sub 'rax (- (add1 (expt 2 31))))))
(check-not-exn       (thunk (Sub 'rax (sub1 (expt 2 32)))))
(check-not-exn       (thunk (Sub 'rax (- (expt 2 31)))))

;; Checking literal widths with sub-64-bit registers
(check-exn exn:fail? (thunk (Mov 'eax (expt 2 32))))
(check-not-exn       (thunk (Mov 'eax (sub1 (expt 2 32)))))
(check-exn exn:fail? (thunk (Mov 'al (expt 2 8))))
(check-not-exn       (thunk (Mov 'al (sub1 (expt 2 8)))))

;; Check prog
(check-exn exn:fail? (thunk (prog (Ret))))
(check-exn exn:fail? (thunk (prog (Label 'start) (Ret))))
(check-exn exn:fail? (thunk (prog (Global 'foo) (Label 'start) (Label 'foo) (Ret))))
(check-not-exn       (thunk (prog (Global 'start) (Label 'start) (Ret))))
(check-not-exn       (thunk (prog (Label 'start) (Ret) (Global 'start))))
(check-exn exn:fail? (thunk (prog (Global 'x) (Label 'x) (Jmp (Offset 'y 8)))))
(check-not-exn       (thunk (prog (Global 'x) (Label 'x) (Extern 'y) (Extern 'y) (Ret))))
(check-exn exn:fail? (thunk (prog (Global 'x) (Label 'x) (Extern 'x))))
(check-exn exn:fail? (thunk (prog (Global 'x) (Label 'x) (Label 'y) (Extern 'y))))

;; Check comment escape hatch is closed
(check-exn exn:fail? (thunk (% "comment\nmov rax 42")))
(check-exn exn:fail? (thunk (%% "comment\nmov rax 42")))
(check-exn exn:fail? (thunk (%%% "comment\nmov rax 42")))
(check-not-exn       (thunk (% "commentmov rax 42")))
(check-not-exn       (thunk (%% "commentmov rax 42")))
(check-not-exn       (thunk (%%% "commentmov rax 42")))

;; Check nasm label conventions
(check-exn exn:fail? (thunk (Jmp 'foo-bar)))

;; Check arguments
(check-exn exn:fail? (thunk (Lea (Offset 'rax 0) 'foo)))

;; Check register size agreement
(check-exn exn:fail? (thunk (Mov 'rax 'eax)))
(check-exn exn:fail? (thunk (Mov 'eax 'rax)))
(check-exn exn:fail? (thunk (Cmove 'eax 'rax)))
(check-exn exn:fail? (thunk (Cmove 'rax 'eax)))
(check-exn exn:fail? (thunk (And 'rax 'eax)))
(check-exn exn:fail? (thunk (And 'eax 'rax)))
(check-exn exn:fail? (thunk (Add 'rax 'eax)))
(check-exn exn:fail? (thunk (Add 'eax 'rax)))
