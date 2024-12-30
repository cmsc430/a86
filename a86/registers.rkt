#lang racket

(provide registers register? register-size)

(define-syntax-rule
  (def-registers (group r ...) ...)
  (begin
    (begin (provide r) ...
           (define r 'r) ...
           (define group
             (list r ...)))
    ...))

(def-registers
  (64-bit-registers     rax rbx rcx rdx rsi  rdi  rbp rsp r8  r9  r10  r11  r12  r13  r14  r15)
  (32-bit-registers     eax ebx ecx edx esi  edi  ebp esp r8d r9d r10d r11d r12d r13d r14d r15d)
  (16-bit-registers      ax  bx  cx  dx  si   di  bp  sp  r8w r9w r10w r11w r12w r13w r14w r15w)
  (8-bit-high-registers  ah  bh  ch  dh)
  (8-bit-low-registers   al  bl  cl  dl  sil  dil bpl spl r8b r9b r10b r11b r12b r13b r14b r15b))

(define registers
  (append 64-bit-registers 32-bit-registers 16-bit-registers 8-bit-high-registers 8-bit-low-registers))

(define (make-register-converter group)
  (define (f x) (or (cdr x) (error "no conversion available")))
  (lambda (r)
    (cond
      [(assq r (map cons 64-bit-registers group)) => f]
      [(assq r (map cons 32-bit-registers group)) => f]
      [(assq r (map cons 16-bit-registers group)) => f]
      [(assq r (map cons 8-bit-low-registers group)) => f]
      [(assq r (map cons 8-bit-high-registers (take group 4))) => f])))

(provide reg-8-bit-low reg-8-bit-high reg-16-bit reg-32-bit reg-64-bit)
(define reg-8-bit-low (make-register-converter 8-bit-low-registers))
(define reg-8-bit-high (make-register-converter (append 8-bit-high-registers (make-list 12 #f))))
(define reg-16-bit (make-register-converter 16-bit-registers))
(define reg-32-bit (make-register-converter 32-bit-registers))
(define reg-64-bit (make-register-converter 64-bit-registers))

(define (register? x)
  (and (memq x registers)
       #t))

(define (register-size r)
  (cond [(memq r 64-bit-registers) 64]
        [(memq r 32-bit-registers) 32]
        [(memq r 16-bit-registers) 16]
        [(memq r  8-bit-high-registers) 8]
        [(memq r  8-bit-low-registers)  8]))
