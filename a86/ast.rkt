#lang racket
(require (only-in "registers.rkt" register? register-size))
(provide register?)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Guards

;; These are used to guard the instruction constructors to reject bad inputs
;; with decent error messages.

(define check:align
  (λ (a x n)
    (unless (power-of-2? x)
      (error n "expects power of 2; given ~v" x))
    (values a x)))

(define check:label-symbol
  (λ (a x n)
    (match x
      [(? symbol?)
       (unless (nasm-label? x)
         (error n "label names must conform to nasm restrictions"))
       (values a ($ x))]
      [($ _)      
       (values a x)]
      [_
       (error n "expects valid label name; given ~v" x)])))

(define check:label-symbol+integer
  (λ (a x c n)
    (let-values ([(a x) (check:label-symbol a x n)])
    (unless (integer? c)
      (error n "expects integer constant; given ~v" c))
    (values a x c))))

(define check:target
  (λ (a x n)
    (match x
      [(? offset?) (values a (exp-normalize x))]
      [(? register?) (values a x)]
      [(? nasm-label? x) (values a ($ x))]
      [($ _) (values a x)]
      [_
       (error n "expects label, register, or offset; given ~v" x)])))

(define check:cmov
  (λ (a a1 a2 n)
    (unless (register? a1)
      (error n "expects register; given ~v" a1))
    (unless (or (register? a2) (offset? a2))
      (error n "expects register or offset; given ~v" a2))
    (when (and (register? a1) (register? a2) (not (= (register-size a1) (register-size a2))))
      (error n "cannot move between registers of unequal size; given ~v (~v-bit), ~v (~v-bit)"
             a1 (register-size a1)
             a2 (register-size a2)))
    (values a a1 (exp-normalize a2))))

(define check:arith
  (λ (a a1 a2 n)
    (unless (register? a1)
      (error n "expects register; given ~v" a1))
    (unless (or (exact-integer? a2) (register? a2) (offset? a2))
      (error n "expects exact integer, register, or offset; given ~v" a2))
    (when (and (exact-integer? a2) (> (integer-size a2) 32))
      (error n "literal must not exceed 32-bits; given ~v (~v bits); go through a register instead" a2 (integer-size a2)))
    (when (and (register? a1) (register? a2) (not (= (register-size a1) (register-size a2))))
      (error n "cannot move between registers of unequal size; given ~v (~v-bit), ~v (~v-bit)"
             a1 (register-size a1)
             a2 (register-size a2)))
    (values a a1 a2)))

(define check:register
  (λ (a a1 n)
    (unless (register? a1)
      (error n "expects register; given ~v" a1))
    (values a a1)))

(define check:src-dest
  (λ (a a1 a2 n)
    (unless (or (register? a1) (offset? a1))
      (error n "expects register or offset; given ~v" a1))
    (unless (or (register? a2) (offset? a2) (exact-integer? a2) (Const? a2))
      (error n "expects register, offset, exact integer, or defined constant; given ~v" a2))
    (when (and (offset? a1) (offset? a2))
      (error n "cannot use two memory locations; given ~v, ~v" a1 a2))
    (when (and (exact-integer? a2) (> (integer-size a2) 32))
      (error n "literal must not exceed 32-bits; given ~v (~v bits); go through a register instead" a2 (integer-size a2)))
    (when (and (offset? a1) (exact-integer? a2))
      (error n "cannot use a memory locations and literal; given ~v, ~v; go through a register instead" a1 a2))
    (when (and (register? a1) (register? a2) (not (= (register-size a1) (register-size a2))))
      (error n "cannot move between registers of unequal size; given ~v (~v-bit), ~v (~v-bit)"
             a1 (register-size a1)
             a2 (register-size a2)))
    (values a a1 a2)))

(define check:mov
  (λ (a a1 a2 n)
    (unless (or (register? a1) (offset? a1))
      (error n "expects register or offset; given ~v" a1))
    (unless (or (register? a2) (offset? a2) (Const? a2) (exp? a2))
      (error n "expects register, offset, or expression; given ~v" a2))
    (when (and (offset? a1) (offset? a2))
      (error n "cannot use two memory locations; given ~v, ~v" a1 a2))
    (when (and (register? a1) (exact-integer? a2) (> (integer-size a2) (register-size a1)))
      (error n "literal must not exceed ~v-bits; given ~v (~v bits)" (register-size a1) a2 (integer-size a2)))
    (when (and (offset? a1) (exact-integer? a2))
      (error n "cannot use a memory locations and literal; given ~v, ~v; go through a register instead" a1 a2))
    (when (and (register? a1) (register? a2) (not (= (register-size a1) (register-size a2))))
      (error n "cannot move between registers of unequal size; given ~v (~v-bit), ~v (~v-bit)"
             a1 (register-size a1)
             a2 (register-size a2)))
    (values a (arg-normalize a1) (arg-normalize a2))))

(define check:shift
  (λ (a a1 a2 n)
    (unless (register? a1)
      (error n "expects register; given ~v" a1))
    (unless (or (and (exact-integer? a2) (<= 0 a2 63))
                (eq? 'cl a2))
      (error n "expects exact integer in [0,63]; given ~v" a2))
    (values a a1 a2)))

(define check:push
  (λ (a a1 n)
    (unless (or (exact-integer? a1) (register? a1))
      (error n "expects exact integer or register; given ~v" a1))
    (when (and (exact-integer? a1) (> (integer-size a1) 32))
      (error n "literal must not exceed 32-bits; given ~v (~v bits); go through a register instead" a1 (integer-size a1)))
    (values a a1)))

(define check:lea
  (λ (a dst x n)
    (unless (register? dst)
      (error n "expects register; given ~v" dst))
    (unless (or (exp? x) (Mem? x))
      (error n "expects memory expression; given ~v" x))
    (values a (arg-normalize dst) (arg-normalize x))))

(define check:none
  (λ (a n) (values a)))

(define (power-of-2? n)
  (and (exact-integer? n)
       (> n 0)
       (= (bitwise-and n (- n 1)) 0)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Comments

(provide (struct-out %)
         (struct-out %%)
         (struct-out %%%)
         Comment?)

(struct Comment (str)
  #:transparent
  #:guard
  (λ (s n)
    (unless (string? s)
      (error n "expects string; given ~v" s))
    (when (string-contains? s "\n")
      (error n "newlines disallowed in comments; given ~v" s))
    s))

(struct %   Comment () #:transparent)
(struct %%  Comment () #:transparent)
(struct %%% Comment () #:transparent)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Expressions

(define (arg-normalize a)
  (if (exp? a)
      (exp-normalize a)
      a))

(provide exp?)

(define (exp? x)
  (match x
    [(? register?) #t]
    [(Plus (? exp?) (? exp?)) #t] ; for backwards compatability
    [(list '? (? exp?) (? exp?) (? exp?)) #t]
    ['$ #t]
    ['$$ #t]
    [(list (? exp-unop?) (? exp?)) #t]
    [(list (? exp-binop?) (? exp?) (? exp?)) #t]
    [($ _) #t]
    [(? nasm-label?) #t]
    [(? 64-bit-integer?) #t]
    [_ #f]))

(provide @)

;; @ is like quasiquote with an implicit unquote at the leaves of the expression
;; constructors and bound identifiers
(define-syntax @
  (λ (stx) ; intentionally non-hygienic
    (syntax-case* stx (? $ $$) (λ (i1 i2) (eq? (syntax->datum i1) (syntax->datum i2)))
      [(_ $) #''$]
      [(_ $$) #''$$]
      [(_ (b e1))
       (memq (syntax->datum #'b) exp-unops)
       #'(list 'b (@ e1))]
      [(_ (b e1 e2))
       (memq (syntax->datum #'b) exp-binops)
       #'(list 'b (@ e1) (@ e2))]
      [(_ (? e1 e2 e3))
       #'(list '? (@ e1) (@ e2) (@ e3))]
      [(_ id)
       (and (identifier? #'id) (not (identifier-binding #'id)))
       #''id]
      [(_ e)
       #'(let ((x e))
           (if (exp? x) x (error "not an assembly expression" x)))])))

(provide exp-unop?)
(define (exp-unop? x)
  (memq x exp-unops))

(provide exp-binop?)
(define (exp-binop? x)
  (memq x exp-binops))

(define exp-unops
  '(- + ~ ! SEG))
(define-for-syntax exp-unops
  '(- + ~ ! SEG))
(define exp-binops
  '(<<< << < <= <=> >= > >> >>> = == != || \| & && ^^ ^ + - * / // % %%))
(define-for-syntax exp-binops
  '(<<< << < <= <=> >= > >> >>> = == != || \| & && ^^ ^ + - * / // % %%))

;; Exp -> Exp
(define (exp-normalize x)
  (match x
    [(? register?) x]
    [(? nasm-label?) ($ x)]
    [(? Mem?) x]
    ;[(Offset e1) (Offset (exp-normalize e1))]
    [(Plus e1 e2)
     (list '+
           (exp-normalize e1)
           (exp-normalize e2))]
    [(list '? e1 e2 e3)
     (list '? (exp-normalize e1) (exp-normalize e2) (exp-normalize e3))]
    [(list (? exp-unop? o) e1)
     (list o (exp-normalize e1))]
    [(list (? exp-binop? o) e1 e2)
     (list o (exp-normalize e1) (exp-normalize e2))]
    [_ x]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Labels

;; See https://github.com/cmsc430/a86/issues/2 for discussion

(provide label?)
(define (label? x)
  (and (symbol? x)
       (nasm-label? x)
       (not (register? x))))

(provide (struct-out $))

(struct $ (label)
  #:transparent
  #:guard
  (λ (x n)
    (unless (symbol? x)
      (error n "expects symbol; given ~v" x))
    (unless (nasm-label? x)
      (error n "label names must conform to nasm restrictions"))
    (values x))

  #;#;#;#:methods gen:equal+hash
  [(define equal-proc     
     (λ (i1 i2 equal?)
       (equal? (->symbol i1)
               (->symbol i2))))
   (define hash-proc  (λ (i hash) (hash (->symbol i))))
   (define hash2-proc (λ (i hash) (hash (->symbol i))))]
  #:property prop:custom-print-quotable 'never
  #;#;#;#:methods gen:custom-write
  [(define (write-proc label port mode)     
     (let ([recur (case mode
                    [(#t) write]
                    [(#f) display]
                    [else (lambda (p port) (print p port mode))])])
       (let ((s (vector-ref (struct->vector label) 1)))
         (if (register? s)
             (begin (if (number? mode)
                        (write-string "($ " port)
                        (write-string "#(struct:$ " port))
                    (recur s port)                        
                    (if (number? mode)
                        (write-string ")" port)
                        (write-string ")" port)))
             (recur s port)))))])
  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Effective Addresses

(provide Mem Mem?)

;; type Mem =
;; | (Mem [Maybe Label] [Maybe Integer] [Maybe Register] [Maybe Register] [Maybe Scale])
;; where at least one of label, base, or index must be given,
;; index cannot be 'rsp

;; type Scale = 1 | 2 | 4 | 8

(define (make-Mem . args)
  (match args
    [(list (? exact-integer? o) (? register? r))
     (%mem #f o r #f #f)]
    [(list (? register? r))
     (%mem #f #f r #f #f)]
    [(list (? register? r1) (? register? r2))
     (%mem #f #f r1 r2 #f)]
    [(list (or (? label? l) ($ l)))
     (%mem ($ l) #f #f #f #f)]
    [(list (? register? r) (? exact-integer? o))
     (%mem #f o r #f #f)]
    [(list (or (? label? l) ($ l)) (? exact-integer? o))
     (%mem ($ l) o #f #f #f)]

    [(list (or (? label? l) ($ l))
           (? exact-integer? o)
           (? register? r1)
           (? register? r2)
           (? integer? s))
     (%mem ($ l) o r1 r2 s)]
    [_
     (error 'Mem "bad args: ~a" args)]))

(define (scale? x)
  (memq x '(1 2 4 8)))

(struct %mem (label off base index scale)
  #:reflection-name 'Mem
  #:transparent
  #:guard
  (λ (label off base index scale name)
     (when (and label (not ($? label)))
       (error name "label must be a label or #f, given ~v" label))
    (when (and off (not (exact-integer? off)))
       (error name "offset must be an exact integer or #f, given ~v" off))
     (when (and base (not (register? base)))
       (error name "base must be a register or #f, given ~v" base))
     (when (and index (not (register? index)))
       (error name "index must be a register (other than rsp) or #f, given ~v" index))
     (when (and scale (not (scale? scale)))
       (error name "scale must be 1,2,4,8 or #f, given ~v" scale))
    (when (not (or label base index))
       (error name "must have at least one of label, base, or index"))
     (when (eq? index 'rsp)
       (error name "index cannot be rsp"))
     (values label off base index scale)))

(define Mem? %mem?)

(define-match-expander Mem
  (λ (stx)
    (syntax-case stx ()
      [(_ l o b i s)  #'(%mem l o b i s)]))
  (λ (stx)
    (syntax-case stx ()
      [m (identifier? #'m) #'make-Mem]
      [(_ . m) #'(make-Mem . m)])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Offsets

(provide Offset Offset?)

(define check:offset
  (λ (m n)
    (unless (or (exp? m) (register? m))
      (error n "expects a memory expression or register; given ~v" m))
     (values (exp-normalize m))))

(struct %offset (m)
  #:reflection-name 'Offset
  #:transparent
  #:guard check:offset)

(define Offset? %offset?)

(define-match-expander Offset
  (λ (stx)
    (syntax-case stx ()
      [(_ p)  #'(%offset p)]
      [(_ p1 p2) #'(%offset (Plus p1 p2))]))
  (λ (stx)
    (syntax-case stx ()
      [m (identifier? #'m)
         #'(case-lambda
             [(m) (%offset m)]
             [(m1 m2) (%offset (Plus m1 m2))])]
      [(_ m) #'(%offset m)]
      [(_ m1 m2) #'(%offset (Plus m1 m2))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instructions

(require racket/struct)
(define current-annotation (make-parameter #f))
(provide instruction-annotation current-annotation
         instruction-name instruction-args)

;; Instruction -> String
(define (instruction-name i)
  (string-downcase (symbol->string (object-name i))))

;; Instruction -> [Listof Arg]
(define (instruction-args i)
  (rest (rest (vector->list (struct->vector i)))))

(struct instruction (annotation))

(define-syntax (instruct stx)
  (syntax-case stx ()
    [(instruct Name (x ...) guard)
     (with-syntax ([Name? (datum->syntax stx (string->symbol (string-append (symbol->string (syntax->datum #'Name)) "?")))])
     #'(begin (provide Name Name?)
              (define-match-expander Name
                (lambda (stx)
                  (syntax-case stx ()
                    [(_ elts (... ...))
                     #'(%Name _ elts (... ...))]))
                (lambda (stx)
                  (syntax-case stx ()
                    [m (identifier? #'m) #'(λ (x ...) (%Name (current-annotation) x ...))]
                    [(m x ...) #'(%Name (current-annotation) x ...)])))
              (struct %Name instruction (x ...)
                #:reflection-name 'Name
                #:transparent
                #:guard guard
                #:methods gen:equal+hash
                [(define equal-proc (λ (i1 i2 equal?)
                                      (equal? (struct->vector i1)
                                              (struct->vector i2))))
                 (define hash-proc  (λ (i hash) (hash (struct->vector i))))
                 (define hash2-proc (λ (i hash) (hash (struct->vector i))))]

                #:property prop:custom-print-quotable 'never
                #:methods gen:custom-write
                [(define write-proc
		   (instr-print 'Name)
                   #;(make-constructor-style-printer
                    (lambda (obj) 'Name)
                    (lambda (obj)
                      (rest (rest (vector->list (struct->vector obj)))))))])
              (define Name? %Name?)))]))

(define (instr-print type)
  (lambda (instr port mode)
    (if (number? mode)
        (write-string "(" port)
        (write-string "#(struct:" port))
    (write-string (symbol->string type) port)
    (let ([recur (case mode
                   [(#t) write]
                   [(#f) display]
                   [else (lambda (p port) (print p port mode))])])
        (for-each (lambda (e)
                    (write-string " " port)
                    (match e
                      [($ (? register?)) (recur e port)]
                      [($ l) (recur l port)]
                      [_ (recur e port)]))
                  (rest (rest (vector->list (struct->vector instr))))))
    (if (number? mode)
        (write-string ")" port)
        (write-string ")" port))))

;(instruct Text   ()        check:none)
;(instruct Data   ()        check:none)

(instruct Align  (x)       check:align)
(instruct Global (x)       check:label-symbol)
(instruct Label  (x)       check:label-symbol)
(instruct Call   (x)       check:target)
(instruct Ret    ()        check:none)
(instruct Mov    (dst src) check:mov)
(instruct Add    (dst src) check:arith)
(instruct Sub    (dst src) check:arith)
(instruct Mul    (src)     check:register)
(instruct Cmp    (a1 a2)   check:src-dest)
(instruct Jmp    (x)       check:target)
(instruct Ja     (x)       check:target)
(instruct Jae    (x)       check:target)
(instruct Jb     (x)       check:target)
(instruct Jbe    (x)       check:target)
(instruct Jc     (x)       check:target)
(instruct Je     (x)       check:target)
(instruct Jz     (x)       check:target)
(instruct Jg     (x)       check:target)
(instruct Jge    (x)       check:target)
(instruct Jl     (x)       check:target)
(instruct Jle    (x)       check:target)
(instruct Jna    (x)       check:target)
(instruct Jnae   (x)       check:target)
(instruct Jnb    (x)       check:target)
(instruct Jnbe   (x)       check:target)
(instruct Jnc    (x)       check:target)
(instruct Jne    (x)       check:target)
(instruct Jng    (x)       check:target)
(instruct Jnge   (x)       check:target)
(instruct Jnl    (x)       check:target)
(instruct Jnle   (x)       check:target)
(instruct Jno    (x)       check:target)
(instruct Jnp    (x)       check:target)
(instruct Jns    (x)       check:target)
(instruct Jnz    (x)       check:target)
(instruct Jo     (x)       check:target)
(instruct Jp     (x)       check:target)
(instruct Jpe    (x)       check:target)
(instruct Jpo    (x)       check:target)
(instruct Js     (x)       check:target)
(instruct Cmovz  (dst src) check:cmov)
(instruct Cmovnz (dst src) check:cmov)
(instruct Cmove  (dst src) check:cmov)
(instruct Cmovne (dst src) check:cmov)
(instruct Cmovl  (dst src) check:cmov)
(instruct Cmovle (dst src) check:cmov)
(instruct Cmovg  (dst src) check:cmov)
(instruct Cmovge (dst src) check:cmov)
(instruct Cmovo  (dst src) check:cmov)
(instruct Cmovno (dst src) check:cmov)
(instruct Cmovc  (dst src) check:cmov)
(instruct Cmovnc (dst src) check:cmov)
(instruct And    (dst src) check:src-dest)
(instruct Test   (dst src) check:src-dest)
(instruct Or     (dst src) check:src-dest)
(instruct Xor    (dst src) check:src-dest)
(instruct Sal    (dst i)   check:shift)
(instruct Sar    (dst i)   check:shift)
(instruct Shl    (dst i)   check:shift)
(instruct Shr    (dst i)   check:shift)
(instruct Push   (a1)      check:push)
(instruct Pop    (a1)      check:register)
(instruct Pushf  ()        check:none)
(instruct Popf   ()        check:none)
(instruct Lea    (dst x)   check:lea)
(instruct Not    (x)       check:register)
(instruct Div    (den)     check:register)

;(instruct Offset (m)       check:offset)        ;; May need to make this not an instruction
(instruct Extern (x)       check:label-symbol)

(instruct Equ    (x v)     check:label-symbol+integer)
(instruct Const  (x)       check:label-symbol)

;; IMPROVE: do more checking
(instruct Db (x) (lambda (a x n) (values a (exp-normalize x))))
(instruct Dw (x) (lambda (a x n) (values a (exp-normalize x))))
(instruct Dd (x) (lambda (a x n) (values a (exp-normalize x))))
(instruct Dq (x) (lambda (a x n) (values a (exp-normalize x))))

(provide (struct-out Plus))
(struct Plus (e1 e2) #:transparent)

#;(provide exp?)
#;
(define (exp? x)
  (or (Offset? x)
      (and (Plus? x)
           (exp? (Plus-e1 x))
           (exp? (Plus-e2 x)))
      (register? x)
      (nasm-label? x)
      ($? x)
      (integer? x)))

(provide offset? 64-bit-integer? 32-bit-integer? 16-bit-integer? 8-bit-integer?)

(define (offset? x)
  (or (Offset? x)
      (Mem? x)))

(define (integer-size x)
  (if (negative? x)
      (add1 (integer-length (sub1 (- x))))
      (integer-length x)))

(define (n-bit-integer n)
  (λ (x)
    (and (exact-integer? x)
         (<= (- (expt 2 (sub1 n))) x (sub1 (expt 2 n))))))

(define 64-bit-integer? (n-bit-integer 64))
(define 32-bit-integer? (n-bit-integer 32))
(define 16-bit-integer? (n-bit-integer 16))
(define 8-bit-integer? (n-bit-integer 8))

(provide (rename-out [a86:instruction? instruction?]))
(define (a86:instruction? x)
  (or (instruction? x)
      (Comment? x)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Sections

(define-syntax-rule
  (section-type T default)
  (begin
    (provide T)
    (struct %T instruction (name)
      #:reflection-name 'T
      #:transparent
      #:property prop:custom-print-quotable 'never
      #:methods gen:custom-write
      [(define write-proc
         (sect-print 'T))]
      #:methods gen:equal+hash
      [(define equal-proc (λ (i1 i2 equal?)
                            (equal? (struct->vector i1)
                                    (struct->vector i2))))
       (define hash-proc  (λ (i hash) (hash (struct->vector i))))
       (define hash2-proc (λ (i hash) (hash (struct->vector i))))])
    (define-match-expander T
      (lambda (stx)
        (syntax-case stx ()
          [(_) #'(%T _ default)]
          [(_ p) #'(%T _ p)]))
      (lambda (stx)
        (syntax-case stx ()
          [m (identifier? #'m) #'(λ ([x default]) (%T (current-annotation) x))]
          [(m) #'(%T (current-annotation) default)]
          [(m n) #'(%T (current-annotation) n)])))))

(define (sect-print type)
  (lambda (instr port mode)
    (if (number? mode)
        (write-string "(" port)
        (write-string "#(struct:" port))
    (write-string (symbol->string type) port)
    (let ([recur (case mode
                   [(#t) write]
                   [(#f) display]
                   [else (lambda (p port) (print p port mode))])])
        (for-each (lambda (e)
                    (match e
                      ['.text (void)]
                      ['.data (void)]
                      [_
                       (write-string " " port)
                       (recur e port)]))
                  (rest (rest (vector->list (struct->vector instr))))))
    (if (number? mode)
        (write-string ")" port)
        (write-string ")" port))))

(section-type Text '.text)
(section-type Data '.data)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Instruction sequencing and program error checking

(provide/contract
 [seq   (-> (or/c a86:instruction? (listof a86:instruction?)) ...
            (listof a86:instruction?))]
 [prog (-> (or/c a86:instruction? (listof a86:instruction?)) ...
           (listof a86:instruction?))])

;; (U Instruction Asm) ... -> Asm
;; Convenient for sequencing instructions or groups of instructions
(define (seq . xs)
  (foldr (λ (x is)
           (if (list? x)
               (append x is)
               (cons x is)))
         '()
         xs))

;; (U Instruction Asm) ... -> Asm
;; Construct a "program", does some global well-formedness checking to help
;; prevent confusing error messages as the nasm level
(define (prog . xs)
  (let ((p (apply seq xs)))
    (check-unique-label-decls p)
    (check-label-targets-declared p)
    (check-has-initial-label p)
    (check-initial-label-global p)
    ;; anything else?
    p))

;; Asm -> Void
(define (check-unique-label-decls xs)
  (let ((r (check-duplicates (all-label-decls xs))))
    (when r
      (error 'prog "duplicate label declaration found: ~v" r))))

;; Asm -> (Listof Symbol)
;; Compute all declared label names
(define (all-label-decls asm)
  (append (label-decls asm)
          (remove-duplicates (extern-decls asm))))

;; Asm -> (Listof Symbol)
(define (label-decls asm)
  (match asm
    ['() '()]
    [(cons (Label ($ s)) asm)
     (cons s (label-decls asm))]
    [(cons _ asm)
     (label-decls asm)]))

;; Asm -> (Listof Symbol)
(define (extern-decls asm)
  (match asm
    ['() '()]
    [(cons (Extern ($ s)) asm)
     (cons s (extern-decls asm))]
    [(cons _ asm)
     (extern-decls asm)]))

;; Any -> Boolean
(define (nasm-label? s)
  (and (symbol? s)
       (regexp-match #rx"^[a-zA-Z._?][a-zA-Z0-9_$#@~.?]*$" (symbol->string s))))

;; Asm -> (Listof Symbol)
;; Compute all uses of label names
(define (label-uses i)
  (match i
    [(? register?) '()]
    [(Label _) '()]  ; declaration, not use
    [(Extern _) '()] ; declaration, not use
    [(Offset m)
     (label-uses m)]
    [(? exp?)
     (exp-label-uses i)]
    [(instruction _)
     (append-map label-uses (instruction-args i))]
    [(cons x y)
     (append (label-uses x) (label-uses y))]
    [_ '()]))

;; Exp -> (Listof Symbol)
(define (exp-label-uses e)
  (match e
    [($ x) (list x)]
    [(Plus e1 e2)
     (append (label-uses e1) (label-uses e2))]
    [(list '? e1 e2 e3)
     (append (label-uses e1) (label-uses e2) (label-uses e3))]
    [(list (? exp-unop?) e1)
     (label-uses e1)]
    [(list (? exp-binop?) e1 e2)
     (append (label-uses e1) (label-uses e2))]
    [_ '()]))

;; Asm -> Void
(define (check-label-targets-declared asm)
  (let ((ds (apply set (all-label-decls asm)))
        (us (apply set (label-uses asm))))

    (let ((undeclared (set-subtract us ds)))
      (unless (set-empty? undeclared)
        (error 'prog "undeclared labels found: ~v" (set->list undeclared))))))

;; Asm -> Void
(define (check-has-initial-label asm)
  (unless (findf Label? asm)
    (error 'prog "no initial label found")))

;; Asm -> Void
(define (check-initial-label-global asm)
  (match (findf Label? asm)
    [(Label init)
     (unless (member init (map (lambda (i) (match i [(Global l) l]))
                               (filter Global? asm)))
       (error 'prog "initial label undeclared as global: ~v" init))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Symbol to Label

;; Symbol -> Label
;; Produce a symbol that is a valid Nasm label
;; Guarantees that (eq? s1 s2) <=> (eq? (symbol->label s1) (symbol->label s1))
(provide symbol->label)
(define (symbol->label s)
  (string->symbol
   (string-append
    "label_"
    (list->string
     (map (λ (c)
            (if (or (char<=? #\a c #\z)
                    (char<=? #\A c #\Z)
                    (char<=? #\0 c #\9)
                    (memq c '(#\_ #\$ #\# #\@ #\~ #\. #\?)))
                c
                #\_))
         (string->list (symbol->string s))))
    "_"
    (number->string (eq-hash-code s) 16))))
