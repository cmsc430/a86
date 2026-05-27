#lang racket
(require "ast.rkt")
(provide/contract
 [asm-string  (-> (listof instruction?) string?)] ; deprecated
 [asm-display (-> (listof instruction?) any)])

(define current-os
  (make-parameter (system-type 'os)))

(define current-shared?
  (make-parameter #f))

(module* private #f
  (provide current-shared?)
  (provide current-os))

;; Asm -> String
(define (asm-string a)
  (with-output-to-string (lambda () (asm-display a))))

(define tab (make-string 8 #\space))

(define (comment->string c)
  (match c
    [(% s)   (string-append (make-string 32 #\space) "# " s)]
    [(%% s)  (string-append tab "## " s)]
    [(%%% s) (string-append "### " s)]))

(define current-extern-labels (make-parameter '()))

;; Label -> String
;; prefix with _ for Mac
(define (label-symbol->string s)
  (match (current-os)
    ['macosx
     (string-append "\"_" (symbol->string s) "\"")]
    [_
     (string-append "\"" (symbol->string s) "\"")]))

       ;(if (and (current-shared?) (memq s (current-extern-labels)))
           ; hack for ELF64 shared libraries in service of
           ; calling external functions in asm-interp
           ;(string-append "$" (symbol->string s) " wrt ..plt")
           ;(symbol->string s)))]))

(define extern-label-decl-symbol->string label-symbol->string)

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
(define (mem->string m #:omit-brackets? [omit-brackets? #f])
  (string-join (flatten
                (list (if omit-brackets? "" "[")
                      (match m
                        ;; Relative label address; inject [rip] base if using
                        ;; brackets, leave out otherwise.
                        [(Mem #f ($ label) #f (and off (or #f (? integer?))))
                         (list (if omit-brackets? "" "rip + ")
                               (label-symbol->string label)
                               (if off
                                   (list  " + "
                                          (number->string off))
                                   (list)))]
                        ;; Invalid relative configuration.
                        [(Mem b (and i (? $?)) s o)
                         (unless (false? b)
                           (raise-argument-error 'mem->string "#f" b))
                         (unless (false? s)
                           (raise-argument-error 'mem->string "#f" s))
                         (unless (or (false? o) (integer? o))
                           (raise-argument-error 'mem->string "(or/c #f integer?)" o))
                         ;; Shouldn't get here, but just in case...
                         (raise-user-error 'mem->string
                                           "invalid relative Mem configuration: ~a"
                                           `(Mem #t ,b ,i ,s ,o))]

                        ;; Only absolute addresses below this.

                        ;; Base register or offset only.
                        [(Mem (and b (not #f)) #f #f #f)
                         (list
                          (cond
                            [(symbol? b) (symbol->string b)]
                            [(integer? b) (number->string b)]
                            [else
                             (raise-argument-error 'mem->string
                                                   "(or/c symbol? integer?)"
                                                   b)]))]
                        ;; Base + index registers w/ optional offset.
                        [(Mem (and b (not #f)) (and i (not #f)) #f o)
                         (list (symbol->string b)
                               " + "
                               (symbol->string i)
                               (if o
                                   (list " + " (number->string o))
                                   (list)))]
                        ;; Base + index registers w/ scale w/ optional offset.
                        [(Mem (and b (not #f)) (and i (not #f)) s o)
                         (list (symbol->string b)
                               " + ("
                               (symbol->string i)
                               " * "
                               (number->string s)
                               ")"
                               (if o
                                   (list " + " (number->string o))
                                   (list)))]
                        ;; Base w/ offset.
                        [(Mem (and b (not #f)) #f #f (and o (not #f)))
                         (list (symbol->string b)
                               " + "
                               (number->string o))]
                        ;; Index w/ scale w/ optional offset.
                        [(Mem #f (and i (not #f)) (and s (not #f)) o)
                         (list "("
                               (symbol->string i)
                               " * "
                               (number->string s)
                               ")"
                               (if o
                                   (list " + " (number->string o))
                                   (list)))]
                        ;; Invalid configuration.
                        [(Mem b i s o)
                         (unless (or (false? b) (register? b) (integer? b))
                           (raise-argument-error 'mem->string
                                                 "(or/c #f register? integer?)"
                                                 b))
                         (unless (or (false? i) (register? i))
                           (raise-argument-error 'mem->string
                                                 "(or/c #f register?)"
                                                 i))
                         (unless (or (false? s) (integer? s))
                           (raise-argument-error 'mem->string
                                                 "(or/c #f integer?)"
                                                 s))
                         (unless (or (false? o) (integer? o))
                           (raise-argument-error 'mem->string
                                                 "(or/c #f integer?)"
                                                 o))
                         (raise-user-error 'mem->string
                                           "invalid absolute Mem configuration: ~a"
                                           `(Mem #f ,b ,i ,s ,o))])
                      (list (if omit-brackets? "" "]"))))
               ""))

;; Exp ∪ Reg ∪ Offset -> String
(define (arg->string e)
  (match e
    [(? register?) (symbol->string e)]
    [(? Mem?) (mem->string e)]
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
  (match (current-os)
    ['macosx (format ".section __TEXT,~a" n)]
    [_       (format ".section ~a,\"ax\",@progbits\n\t.p2align 4" n)]))

(define (data-section n)
  (match (current-os)
    ['macosx (format ".section __DATA,~a\n\t.p2align 3" n)]
    [_       (format ".section ~a,\"aw\",@progbits\n\t.p2align 3" n)]))

;; Instruction -> String
(define (simple-instr->string i)
  (match i
    [(Text)         (string-append tab ".text")]
    [(Text n)       (string-append tab (text-section n))]
    [(Data)         (string-append tab ".data\n\t.p2align 3")] ; 8-byte aligned data
    [(Data n)       (string-append tab (data-section n))]
    [(Extern ($ l)) (string-append tab ".extern " (extern-label-decl-symbol->string l))]
    [(Global ($ l)) (string-append tab ".global " (label-symbol->string l))]
    [(Label ($ l))  (string-append (label-symbol->string l) ":")]
    [(Align n)
     (match (current-os)
       ['macosx (string-append ".p2align "
                               (number->string
                                (let loop ([i 0] [n n])
                                  (if (= n 1) i (loop (add1 i)
                                                      (arithmetic-shift n -1))))))]
       [_       (string-append ".align " (number->string n))])]
    [(Lea d (? Mem? m))
     (string-append tab "lea "
                    (arg->string d) ", "
                    (mem->string m))]
    [(Lea _ _)
     (error 'simple-instr->string "unsupported instruction variant: ~e" i)]
    [(Equ x c)
     (string-append tab
                    (symbol->string x)
                    " equ "
                    (number->string c))]
    [(Dq (? Mem? m))
     (string-append tab ".quad " (mem->string m #:omit-brackets? #t))]
    [(Dq (? number? m))
     (string-append tab ".quad " (number->string m))]
    [(Dq _)
     (error 'simple-instr->string "unknown instruction: ~e" i)]
    [(Dd (? Mem? m))
     (string-append tab ".long " (mem->string m #:omit-brackets? #t))]
    [(Dd (? number? m))
     (string-append tab ".long " (number->string m))]
    [(Dd _)
     (error 'simple-instr->string "unknown instruction: ~e" i)]
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
