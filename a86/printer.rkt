#lang racket
(require "ast.rkt")
(provide/contract
 [asm-string  (-> (listof instruction?) string?)] ; deprecated
 [asm-display (-> (listof instruction?) any)])

(define current-shared?
  (make-parameter #f))

(module* private #f
  (provide current-shared?))

;; Asm -> String
(define (asm-string a)
  (with-output-to-string (lambda () (asm-display a))))

(define label-occurred? (make-parameter #f))
(define-syntax (sqbracket stx)
  (syntax-case stx ()
    [(_ e)
     #'(parameterize ([label-occurred? #f])
         (let ([v e])
           (string-append "["
                          v
                          (if (label-occurred?)
                              " + rip"
                              "")
                          "]")))]))

(define tab (make-string 8 #\space))

(define (comment->string c)
  (match c
    [(% s)   (string-append (make-string 32 #\space) "# " s)]
    [(%% s)  (string-append tab "## " s)]
    [(%%% s) (string-append "### " s)]))

(define current-extern-labels (make-parameter '()))

;; Label -> String
;; prefix with _ for Mac
(define label-symbol->string
  (match (system-type 'os)
    ['macosx
     (λ (s)
       (label-occurred? #t)
       (string-append "_" (symbol->string s)))]
    [_
     (λ (s)
       (label-occurred? #t)
       (symbol->string s))]))

       ;(if (and (current-shared?) (memq s (current-extern-labels)))
           ; hack for ELF64 shared libraries in service of
           ; calling external functions in asm-interp
           ;(string-append "$" (symbol->string s) " wrt ..plt")
           ;(symbol->string s)))]))

(define extern-label-decl-symbol->string
  (match (system-type 'os)
    ['macosx
     (λ (s)
       (label-occurred? #t)
       (string-append "_" (symbol->string s)))]
    [_
     (λ (s)
       (label-occurred? #t)
       (symbol->string s))]))

;; Instruction -> String
(define (common-instruction->string i)
  (let ((as (instruction-args i)))
    (string-append tab
                   (instruction-name i)
                   (apply string-append
                          (if (empty? as) "" " ")
                          (add-between (map arg->string as)
                                       ", ")))))
;; Instruction -> String
(define (fancy-instr->string i)
  (let ((s (simple-instr->string i)))
    (if (instruction-annotation i)
        (if (< (string-length s) 40)
            (format "~a~a; ~.s" s (make-string (- 40 (string-length s)) #\space) (instruction-annotation i))
            (format "~a ; ~.s" s (instruction-annotation i)))
        s)))

;; Mem -> String
(define (mem->string m)
  (define (x->string x)
    (match x
      [(? integer?) (number->string x)]
      [(? symbol?) (symbol->string x)]
      [($ x) (label-symbol->string x)]))
  (match m
    [(Mem l b i o s)
     (string-append
      (apply string-append (add-between (map x->string (filter identity (list l o b i))) " + "))
      (match s
        [#f ""]
        [1  ""]
        [i (string-append " * " (number->string i))]))]))

;; Exp ∪ Reg ∪ Offset -> String
(define (arg->string e)
  (match e
    [(? register?) (symbol->string e)]
    #;[(? Mem?) (string-append "[" (mem->string e) "]")]
    #;[(Offset (? register? r)) (string-append "[" (symbol->string r) "]")]
    #;[(Offset ($ (? label? r))) (string-append "[" (label-symbol->string r) " + rip]")]
    #;[(Offset e) (string-append "[" (exp->string e) "]")]
    [(? Mem?)                  (sqbracket (mem->string e))]
    [(Offset (? register? r))  (sqbracket (symbol->string r))]
    [(Offset ($ (? label? r))) (sqbracket (label-symbol->string r))]
    [(Offset e)                (sqbracket (exp->string e))]
    [_ (exp->string e)]))

;; Exp -> String
(define (exp->string e)
  (match e
    [(? register?) (symbol->string e)]
    [(? integer?) (number->string e)]
    [($ x) (label-symbol->string x)]
    ;; [($ x) (string-append (label-symbol->string x) " + rip")]
    [(list '? e1 e2 e3)
     (string-append "(" (exp->string e1) " ? " (exp->string e2) " : " (exp->string e3) ")")]
    [(list (? exp-unop? o) e1)
     (string-append "(" (symbol->string o) " " (exp->string e1) ")")]
    [(list (? exp-binop? o) e1 e2)
     (string-append "(" (exp->string e1) " " (symbol->string o) " " (exp->string e2) ")")]))

(define (text-section n)
  (match (system-type 'os)
    ['macosx (format ".section __TEXT,~a \n\t .p2align 4" n)]
    [_       (format ".section ~a progbits alloc exec nowrite align=16" n)]))

(define (data-section n)
  (match (system-type 'os)
    ['macosx (format ".section __DATA,~a \n\t .p2align 3" n)]
    [_       (format ".section ~a progbits alloc noexec write align=8" n)]))

;; Instruction -> String
(define (simple-instr->string i)
  (match i
    [(Text)         (string-append tab ".text")]
    [(Text n)       (string-append tab (text-section n))]
    [(Data)         (string-append tab ".data\n\t .p2align 3")] ; 8-byte aligned data
    [(Data n)       (string-append tab (data-section n))]
    [(Extern ($ l)) (string-append tab ".extern " (extern-label-decl-symbol->string l))]
    [(Global ($ l)) (string-append tab ".global " (label-symbol->string l))]
    ;;[(Label ($ l))  (string-append "_" (symbol->string l) ":")]
    [(Label ($ l))  (string-append (label-symbol->string l) ":")]
    [(Lea d (? Mem? m))
     (string-append tab "lea "
                    (arg->string d) ", "
                    (sqbracket (mem->string m)))]
    [(Lea d e)
     (string-append tab "lea "
                    (arg->string d) ", "
                    (sqbracket (arg->string e)))]
    [(Equ x c)
     (string-append tab
                    (symbol->string x)
                    " equ "
                    (number->string c))]
    [(Dq (? Mem? m))
     (string-append tab ".quad " (mem->string m))]
    [(Dq  m)
     (string-append tab ".quad " (number->string m))]
    [(Dd (? Mem? m))
     (string-append tab ".long " (mem->string m))]
    [(Dd  m)
     (string-append tab ".long " (number->string m))]
    [(Db (? bytes? bs))
     (apply string-append tab ".byte " (add-between (map number->string (bytes->list bs)) ", "))]
    [_ (common-instruction->string i)]))

(define (line-comment i s)
  (let ((i-str (simple-instr->string i)))
    (let ((pad (make-string (max 1 (- 32 (string-length i-str))) #\space)))
      (string-append i-str pad "# " s))))

;; [Listof Instr] -> Void
(define (instrs-display a)
  (match a
    ['() (void)]
    [(cons (? Comment? c) a)
     (begin (write-string (comment->string c))
            (write-char #\newline)
            (instrs-display a))]
    [(cons i (cons (% s) a))
     (begin (write-string (line-comment i s)) ; a line comment trumps an annotation
            (write-char #\newline)
            (instrs-display a))]
    [(cons i a)
     (begin (write-string (fancy-instr->string i))
            (write-char #\newline)
            (instrs-display a))]))

;; Asm -> [Listof Symbol]
(define (extern-labels a)
  (match a
    ['() '()]
    [(cons (Extern ($ l)) a)
     (cons l (extern-labels a))]
    [(cons _ a)
     (extern-labels a)]))

;; Asm -> Void
(define (asm-display a)
  (define (go)
    ;; entry point will be first label
    (match (findf Label? a)
      [(Label g)
       (begin
         (write-string (string-append
                        ; tab "global " (label-symbol->string g) "\n"
                        tab ".intel_syntax noprefix\n"
                        tab ".text\n"))
         (instrs-display a))]
      [_
       (instrs-display a)
       #;
       (error "program does not have an initial label")]))
  (if (current-shared?)
      (parameterize ([current-extern-labels (extern-labels a)])
        (go))
      (go)))
