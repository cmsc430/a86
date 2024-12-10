#lang racket

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
    (unless (or (register? a2) (offset? a2) (exact-integer? a2) (Const? a2) (nasm-label? a2) ($? a2))
      (error n "expects register, offset, exact integer, or label; given ~v" a2))
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
    (values a (exp-normalize a1) (exp-normalize a2))))

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
    (unless (exp? x)
      (error n "expects memory expression; given ~v" x))
    (values a (exp-normalize dst) (exp-normalize x))))

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
;; Labels

;; See https://github.com/cmsc430/a86/issues/2 for discussion

(provide (struct-out $))

;; Exp -> Exp
(define (exp-normalize x)
  (match x
    [($ _) x]
    [(? register?) x]
    [(? nasm-label?) ($ x)]
    [(? integer? i) i]
    [(Offset e1) (Offset (exp-normalize e1))]
    [(Plus e1 e2)
     (Plus (exp-normalize e1)
           (exp-normalize e2))]))

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
;; Offsets

(provide Offset Offset?)

(define check:offset
  (λ (m n)
    (unless (exp? m)
      (error n "expects a memory expression; given ~v" m))
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

(provide exp?)
(define (exp? x)
  (or (Offset? x)
      (and (Plus? x)
           (exp? (Plus-e1 x))
           (exp? (Plus-e2 x)))
      (register? x)
      (nasm-label? x)
      ($? x)
      (integer? x)))

(provide offset? register? label? 64-bit-integer? 32-bit-integer? register-size)

(define offset? Offset?)

(define-syntax-rule
  (def-registers (group r ...) ...)
  (begin
    (begin ; (provide r) ... ; avoid for now
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

(define (integer-size x)
  (integer-length (abs x)))

(define (64-bit-integer? x)
  (and (exact-integer? x)
       (<= (integer-size x) 64)))

(define (32-bit-integer? x)
  (and (exact-integer? x)
       (<= (integer-size x) 32)))

(define (label? x)
  (and (symbol? x)
       (nasm-label? x)
       (not (register? x))))

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
    [(Label _) '()]  ; declaration, not use
    [(Extern _) '()] ; declaration, not use
    [(Offset m)
     (label-uses m)]
    [(instruction _)
     (append-map label-uses (instruction-args i))]
    [($ x) (list x)]
    [(Plus e1 e2)
     (append (label-uses e1) (label-uses e2))]
    [(cons x y)
     (append (label-uses x) (label-uses y))]
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
