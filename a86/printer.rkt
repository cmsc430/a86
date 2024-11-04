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

(define tab (make-string 8 #\space))

(define (comment->string c)
  (match c
    [(% s)   (string-append (make-string 32 #\space) "; " s)]
    [(%% s)  (string-append tab ";; " s)]
    [(%%% s) (string-append ";;; " s)]))

(define current-extern-labels (make-parameter '()))

;; Label -> String
;; prefix with _ for Mac
(define label-symbol->string
  (match (system-type 'os)
    ['macosx
     (λ (s) (string-append "$_" (symbol->string s)))]
    [_
     (λ (s)
       (if (and (current-shared?) (memq s (current-extern-labels)))
           ; hack for ELF64 shared libraries in service of
           ; calling external functions in asm-interp
           (string-append "$" (symbol->string s) " wrt ..plt")
           (string-append "$" (symbol->string s))))]))

(define extern-label-decl-symbol->string
  (match (system-type 'os)
    ['macosx
     (λ (s) (string-append "$_" (symbol->string s)))]
    [_
     (λ (s)
       (string-append "$" (symbol->string s)))]))
  
;; Instruction -> String
(define (common-instruction->string i)
  (let ((as (instruction-args i)))
    (string-append tab
                   (instruction-name i)
                   (apply string-append
                          (if (empty? as) "" " ")
                          (add-between (map exp->string as)
                                       ", ")))))
;; Instruction -> String
(define (fancy-instr->string i)
  (let ((s (simple-instr->string i)))
    (if (instruction-annotation i)
        (if (< (string-length s) 40)
            (format "~a~a; ~.s" s (make-string (- 40 (string-length s)) #\space) (instruction-annotation i))
            (format "~a ; ~.s" s (instruction-annotation i)))
        s)))

;; Exp -> String
(define (exp->string e)
  (match e
    [(? register?) (symbol->string e)]
    [(? integer?) (number->string e)]
    [($ x) (label-symbol->string x)]
    [(Offset e1 e2)
     (string-append "[" (exp->string e1) " + " (exp->string e2) "]")]
    [(Plus e1 e2)
     (string-append "(" (exp->string e1) " + " (exp->string e2) ")")]))

;; Instruction -> String
(define (simple-instr->string i)
  (match i
    [(Text)         (string-append tab "section .text")]
    [(Data)         (string-append tab "section .data align=8")] ; 8-byte aligned data
    [(Extern ($ l)) (string-append tab "extern " (extern-label-decl-symbol->string l))]
    [(Label ($ l))  (string-append (label-symbol->string l) ":")]
    [(Lea d e)
     (string-append tab "lea "
                    (exp->string d) ", [rel "
                    (exp->string e) "]")]
    [(Equ x c)
     (string-append tab
                    (symbol->string x)
                    " equ "
                    (number->string c))]
    [(Db (? bytes? bs))
     (apply string-append tab "db " (add-between (map number->string (bytes->list bs)) ", "))]
    [_ (common-instruction->string i)]))

(define (line-comment i s)
  (let ((i-str (simple-instr->string i)))
    (let ((pad (make-string (max 1 (- 32 (string-length i-str))) #\space)))
      (string-append i-str pad "; " s))))

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
                        tab "default rel\n"
                        tab "section .text\n"))
         (instrs-display a))]
      [_
       (instrs-display a)
       #;
       (error "program does not have an initial label")]))
  (if (current-shared?)
      (parameterize ([current-extern-labels (extern-labels a)])
        (go))
      (go)))
