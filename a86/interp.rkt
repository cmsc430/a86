#lang racket
(provide/contract
 [current-objs  (parameter/c (listof path-string?))]
 [asm-interp    ;(-> (listof instruction?) any/c)
  (->* () #:rest (or/c (listof instruction?)
                       (listof (listof instruction?)))
       any/c)]
 [asm-interp/io (-> (listof instruction?) string? any/c)])

(define-logger a86)

(require "printer.rkt" "ast.rkt" "callback.rkt" "check-assembler.rkt"
         (rename-in ffi/unsafe [-> _->])
         ffi/unsafe/define
         racket/runtime-path)
(require (submod "printer.rkt" private))

;; Check clang availability when required to fail fast.
(check-clang-available)

;; Bail out if we're not on an x86_64 Racket.
(unless (eq? 'x86_64 (system-type 'arch))
  (error 'a86
         "This library requires x86_64 Racket, but yours is ~a (~a)."
         (system-type 'arch)
         (system-type 'os)))

(define *debug*?
  (let ((r (getenv "PLTSTDERR")))
    (and r
         (string=? r "info@a86"))))

;; Assembly code is linked with object files in this parameter
(define current-objs
  (make-parameter '()))

;; Asm ... -> Value
;; Interpret (by assemblying, linking, and loading) x86-64 code
;; Assume: entry point is "entry"
(define (asm-interp . is)
  (match (asm-interp/io is #f)
    [(cons r out) r]))

(define fopen
  (get-ffi-obj "fopen" (ffi-lib #f) (_fun _path _string/utf-8 _-> _pointer)))

(define fflush
  (get-ffi-obj "fflush" (ffi-lib #f) (_fun _pointer _-> _void)))

(define fclose
  (get-ffi-obj "fclose" (ffi-lib #f) (_fun _pointer _-> _void)))

;; WARNING: The heap is re-used, so make sure you're done with it
;; before calling asm-interp again
(define *heap*
  ; IMPROVE ME: hard-coded heap size
  (malloc _int64 20000 'raw))


;; Integer64 -> String
(define (int64->binary-string n)
  (format "#b~a"
          (~r n #:base 2 #:min-width 64 #:pad-string "0")))

;; Integer64 -> String
(define (int64->octal-string n)
  (format "#o~a"
          (~r n #:base 8 #:min-width 22 #:pad-string "0")))

;; Integer64
(define (int64->hex-string n)
  (format "#x~a"
          (~r n #:base 16 #:min-width 16 #:pad-string "0")))

(define (show-state . regs)
  (format "\n~a"
          (map (lambda (r v)
                 (format "(~a ~a)" r (int64->hex-string v)))
               '(rax rbx rcx rdx rbp rsp rsi rdi
                     r8 r9 r10 r11 r12 r13 r14 r15 instr flags)
               regs)))


(define-runtime-path here ".")
(define liba86-jit
  (ffi-lib
   (build-path here
               "llvm-jit"
               "lib"
               (format "liba86_jit~a" (system-type 'so-suffix)))
   #:custodian (current-custodian)))

(define-cpointer-type _a86_jit_t)

(define-cstruct _a86_jit_result
  ([ok _int]
   [value _int64]
   [error_message _pointer]))

(define-ffi-definer define-a86 liba86-jit)

(define-a86 a86_jit_create
  (_fun _-> _a86_jit_t))

(define-a86 a86_jit_destroy
  (_fun _a86_jit_t _-> _void))

(define-a86 a86_jit_run
  (_fun _a86_jit_t _string _string _-> _a86_jit_result))

(define-a86 a86_jit_define_symbol
  (_fun _a86_jit_t _string _pointer _-> _int))

(define-a86 a86_jit_clear_symbols
  (_fun _a86_jit_t _-> _int))

(define-a86 a86_jit_set_global
  (_fun _a86_jit_t _string _pointer _-> _int))

(define-a86 a86_jit_clear_globals
  (_fun _a86_jit_t _-> _int))

(define (jit-clear-symbols! jit)
  (unless (= 1 (a86_jit_clear_symbols jit))
    (error 'a86-jit "failed to clear JIT symbols")))

(define-a86 a86_jit_add_object_file
  (_fun _a86_jit_t _path _-> _int))

(define-a86 a86_jit_clear_object_files
  (_fun _a86_jit_t _-> _int))

(define (jit-define-symbol! jit name ptr)
  (unless (= 1 (a86_jit_define_symbol jit name ptr))
    (error 'a86-jit "failed to define JIT symbol ~a" name)))

(define (jit-clear-globals! jit)
  (unless (= 1 (a86_jit_clear_globals jit))
    (error 'a86-jit "failed to clear JIT globals")))

(define (jit-set-global! jit name ptr)
  (unless (= 1 (a86_jit_set_global jit name ptr))
    (error 'a86-jit "failed to set JIT global ~a" name)))

(define (jit-clear-object-files! jit)
  (unless (= 1 (a86_jit_clear_object_files jit))
    (error 'a86-jit "failed to clear JIT object files")))

(define (jit-add-object-file! jit path)
  (unless (= 1 (a86_jit_add_object_file jit path))
    (error 'a86-jit "failed to add JIT object file ~a" path)))

(define (decode-error-message p)
  (if (ptr-equal? p #f)
      "unknown error"
      (cast p _pointer _string/utf-8)))

(define (check-result who r)
  (if (= 1 (a86_jit_result-ok r))
      (a86_jit_result-value r)
      (error who (decode-error-message (a86_jit_result-error_message r)))))

(define the-jit #f)

(define (reset-jit!)
  (when the-jit
    (a86_jit_destroy the-jit))
  (set! the-jit
        (or (a86_jit_create)
            (error 'a86-jit "failed to create JIT instance"))))

(reset-jit!)

(define (run-jit! asm-str init-label)
  (with-handlers ([symbol?
                   (λ (s)
                     (reset-jit!)
                     s)]
                  [exn:fail?
                   (λ (e)
                     (reset-jit!)
                     (raise e))])
    (guard-foreign-escape
     (check-result 'a86-jit
                   (a86_jit_run the-jit asm-str (symbol->string init-label))))))

(define (program->asm-string a)
  (with-output-to-string
    (λ ()
      (parameterize ([current-shared? #t])
        (asm-display (if *debug*? (debug-transform a) a))))))

;; Returns two values: transformed program and unmangled entry name
(define (prepare-program a)
  (define init-label
    (match (findf Label? a)
      [(Label ($ l)) l]
      [_ #f]))
  (define global?
    (and init-label
         (ormap (match-lambda
                  [(Global g) (eq? g init-label)]
                  [_ #f])
                a)))
  (cond
    [(and init-label global?)
     (values (apply prog a) init-label)]
    [else
     (define i (symbol->label (gensym 'init)))
     (values (apply prog (Global i) (Label i) a) i)]))

(define (null-ptr? p)
  (ptr-equal? p #f))

(define (box-pointer p)
  (define cell (malloc _pointer))
  (ptr-set! cell _pointer p)
  cell)

;; Asm ... String -> (cons Value String)
;; Like asm-interp, but uses given string for input and returns
;; result with string output
(define (asm-interp/io a input)
  (log-a86-info (~v a))

  (define tin  (make-temporary-file "a86in~a"))
  (define tout (make-temporary-file "a86out~a"))
  (define in-port #f)
  (define out-port #f)

  (dynamic-wind
   void
   (λ ()
     (call-with-output-file tin
       #:exists 'truncate/replace
       (λ (op) (display input op)))

     (set! in-port (fopen tin "r"))
     (set! out-port (fopen tout "w"))

     (when (null-ptr? in-port)
       (error 'asm-interp/io "failed to open input file"))

     (when (null-ptr? out-port)
       (error 'asm-interp/io "failed to open output file"))

     (define-values (a* init-label)
       (prepare-program a))

     (define asm-str
       (program->asm-string a*))

     (jit-clear-symbols! the-jit)
     (jit-clear-globals! the-jit)

     (jit-set-global! the-jit "heap"  *heap*)
     (jit-set-global! the-jit "from"  *heap*)
     (jit-set-global! the-jit "to"    (ptr-add *heap* 10000 _int64))
     ;(jit-set-global! the-jit "types" (box-pointer types-ptr))
     (jit-set-global! the-jit "in"    in-port)
     (jit-set-global! the-jit "out"   out-port)

     ;; error hook
     (define error-handler-ptr
       (function-ptr (λ () (raise 'err)) (_fun _-> _void)))
     (jit-set-global! the-jit "error_handler" error-handler-ptr)

     ;; debug hook
     #;
     (when *debug*?
       (a86_jit_define_symbol the-jit
                              log-label
                              (function-ptr
                               (λ ()
                                 (log-a86-info
                                  (apply show-state
                                         (build-list 18
                                                     (λ (i) (ptr-ref debug-log _int64 (add1 i)))))))
                               (_fun _-> _void))))


     (jit-clear-object-files! the-jit)
     (for ([obj (current-objs)])
       (jit-add-object-file! the-jit obj))

     (define result
       (run-jit! asm-str init-label))

     (fflush out-port)
     (cons result (call-with-input-file tout port->string)))

   ;; clean-up
   (λ ()
     (when out-port (fclose out-port) (set! out-port #f))
     (when in-port  (fclose in-port)  (set! in-port #f))
     (when (file-exists? tin)  (delete-file tin))
     (when (file-exists? tout) (delete-file tout)))))

#;
(define (asm-interp/io a input)

  (log-a86-info (~v a))

  (define t.s   (make-temporary-file "clang-~a.s"))
  (define t.o   (path-replace-extension t.s #".o"))
  (define t.so  (path-replace-extension t.s #".so"))
  (define t.in  (path-replace-extension t.s #".in"))
  (define t.out (path-replace-extension t.s #".out"))

  ;; If the initial label is declared global, jump to that, otherwise
  ;; generate an initial label at first instruction and jump there

  (define init-label
    (match (findf Label? a)
      [(Label ($ l)) l]
      [_ #f]))

  (define global?
    (and init-label
         (ormap (match-lambda
                  [(Global g) (eq? g init-label)]
                  [_ #f])
                a)))

  (define a*
    (cond
      [(and init-label global?) (apply prog a)]
      [else (let ((i (symbol->label (gensym 'init))))
              (set! init-label i)
              (apply prog
                     (Global i)
                     (Label i)
                     a))]))

  (with-output-to-file t.s
    #:exists 'truncate
    (λ ()
      (parameterize ((current-shared? #t))
        (asm-display (if *debug*?
                         (debug-transform a*)
                         a*)))))

  (clang t.s t.o)
  (ld t.o t.so)

  (define libt.so (ffi-lib t.so))


  (define entry
    (get-ffi-obj init-label libt.so (_fun _pointer _-> _int64)))

  ;; install our own `error_handler` procedure to prevent `exit` calls
  ;; from interpreted code bringing down the parent process.  All of
  ;; these hooks into the runtime need a better API and documentation,
  ;; but this is a rough hack to make Extort work for now.
  (when (ffi-obj-ref "error_handler" libt.so (thunk #f))
    (set-ffi-obj! "error_handler" libt.so _pointer
                  (function-ptr (λ () (raise 'err)) (_fun _-> _void))))

  (when *debug*?
    (define log (ffi-obj-ref log-label libt.so (thunk #f)))
    (when log
      (set-ffi-obj! log-label libt.so _pointer
                    (function-ptr
                     (λ () (log-a86-info
                            (apply show-state
                                   (build-list 18 (lambda (i) (ptr-ref log _int64 (add1 i)))))))
                     (_fun _-> _void)))))

  (define has-heap? #f)

  (when (ffi-obj-ref "heap" libt.so (thunk #f))
    (set! has-heap? #t)

    ;; This is a GC-enabled run-time so set from, to, and types space
    (when (ffi-obj-ref "from" libt.so (thunk #f))
      ;; FIXME: leaks types memory
      (set-ffi-obj! "from" libt.so _pointer *heap*)
      (set-ffi-obj! "to" libt.so _pointer (ptr-add *heap* 10000 _int64))
      (set-ffi-obj! "types" libt.so _pointer (malloc _int32 10000))))

  (delete-file t.s)
  (delete-file t.o)
  (delete-file t.so)
  (if input
      (let ()
        (unless (and (ffi-obj-ref "in" libt.so (thunk #f))
                     (ffi-obj-ref "out" libt.so (thunk #f)))
          (error "asm-interp/io: running in IO mode without IO linkage"))

        (with-output-to-file t.in #:exists 'truncate
          (thunk (display input)))

        (define current-in
          (make-c-parameter "in" libt.so _pointer))
        (define current-out
          (make-c-parameter "out" libt.so _pointer))

        (current-in  (fopen t.in "r"))
        (current-out (fopen t.out "w"))

        (define result
          (with-handlers ((symbol? identity))
            (guard-foreign-escape
             (entry *heap*))))

        (fflush (current-out))
        (fclose (current-in))
        (fclose (current-out))

        (define output (file->string t.out))
        (delete-file t.in)
        (delete-file t.out)
        (cons result output))

      (with-handlers ((symbol? identity))
        (guard-foreign-escape
         (entry *heap*)))))


(define (string-splice xs)
  (apply string-append
         (add-between (map (lambda (s) (string-append "\"" s "\"")) xs)
                      " ")))

;;; Utilities for calling clang and linker with informative error messages

(struct exn:clang exn:fail:user ())
(define assembly-error-msg
  (string-append
   "assembly error: make sure to use `prog` to construct an assembly program\n"
   "if you did and still get this error; please share with course staff."))

(define (clang:error msg)
  (raise (exn:clang (format "~a\n\n~a" assembly-error-msg msg)
                    (current-continuation-marks))))

;; run clang on t.s to create t.o
(define (clang t.s t.o)
  (define err-port (open-output-string))
  (define fmt (if (eq? (system-type 'os) 'macosx) 'macho64 'elf64))
  (define prefix
    (if (eq? (system-type 'os) 'macosx)
        "arch -x86_64"
        ""))

  (unless (parameterize ((current-error-port err-port))
            (system (format "~a clang -c ~a -o ~a" prefix t.s t.o)))
    (clang:error (get-output-string err-port))))

(struct exn:ld exn:fail:user ())
(define (ld:error msg)
  (raise (exn:ld (format "link error: ~a" msg)
                 (current-continuation-marks))))

(define (ld:undef-symbol s)
  (ld:error
   (string-append
    (format "symbol ~a not defined in linked objects: ~a\n" s (current-objs))
    "use `current-objs` to link in object containing symbol definition.")))

;; link together t.o with current-objs to create shared t.so
(define (ld t.o t.so)
  (define err-port (open-output-string))
  (define objs (string-splice (current-objs)))
  (define -z-defs-maybe
    (if (eq? (system-type 'os) 'macosx)
        ""
        "-z defs "))
  (unless (parameterize ((current-error-port err-port))
            (system (format "gcc ~a-v -shared ~a ~a -o ~a"
                            -z-defs-maybe
                            t.o objs t.so)))
    (define err-msg
      (get-output-string err-port))
    (match (or (regexp-match #rx"Undefined.*\"(.*)\"" err-msg)            ; mac
               (regexp-match #rx"undefined reference to `(.*)'" err-msg)) ; linux
      [(list _ symbol) (ld:undef-symbol symbol)]
      [_ (ld:error (format "unknown link error.\n\n~a" err-msg))])))



;; Debugging facilities

(define log-label (symbol->label (gensym 'log)))

(define (Log i)
  (seq (save-registers)
       (Pushf)
       (Mov 'rax i)
       (Mov (Mem log-label (* 8 17)) 'rax)
       (Mov 'rax (Mem 'rsp 0))
       (Mov (Mem log-label (* 8 18)) 'rax)
       (Call (Mem log-label))
       (Popf)
       (restore-registers)))

(define (instrument is)
  (for/fold ([ls '()]
             #:result (reverse ls))
            ([idx (in-naturals)]
             [ins (in-list is)])
    (if (serious-instruction? ins)
        (seq ins (reverse (Log idx)) ls)
        (seq ins ls))))

(define (serious-instruction? ins)
  (match ins
    [(Label _) #f]
    [(Global _) #f]
    [(? Comment?) #f]
    [_ #t]))

(define (debug-transform is)
  (seq (instrument is)
          ;; End of user program
          (Data)
          (Global log-label)
          (Label log-label)
          (Dq 0) ; callback placeholder
          (static-alloc-registers)
          (Dq 0) ; index of instruction
          (Dq 0) ; flags
          ))

(define registers
  '(rax rbx rcx rdx rbp rsp rsi rdi
        r8 r9 r10 r11 r12 r13 r14 r15))

(define (static-alloc-registers)
  (apply seq
         (map (λ (r) (seq (Dq 0) (% (~a r))))
              registers)))

(define (save-registers)
  (apply seq
         (map (λ (r i) (seq (Mov (Mem log-label (* 8 i)) r)))
              registers
              (build-list (length registers) add1))))

(define (restore-registers)
  (apply seq
         (map (λ (r i) (seq (Mov r (Mem log-label (* 8 i)))))
              registers
              (build-list (length registers) add1))))
