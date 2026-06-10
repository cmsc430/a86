#lang racket

(provide
 (contract-out
  [struct extern
    ([name symbol?]
     [value extern-value?]
     [ctype ctype?])]
  [current-jit (parameter/c jit?)]
  [current-externs (parameter/c extern-list/c)]
  [current-objects (parameter/c (listof path-string?))]
  [reset-jit! (-> void?)]
  [asm-load
   (->* ((listof instruction?))
        (#:externs (listof extern?)
         #:objects (listof path-string?)
         #:jit (or/c #f jit?))
        asm-program?)]
  [asm-call
   (->* (asm-program? symbol?)
        ()
        #:rest (listof machine-word?)
        integer?)]
  [asm-unload
   (-> asm-program? void?)]
  [call-with-asm-loaded
   (->* ((listof instruction?) (-> asm-program? any/c))
        (#:externs (listof extern?)
         #:objects (listof path-string?)
         #:jit (or/c #f jit?))
        any/c)]
  [asm-interp
   (->* () #:rest (or/c (listof instruction?) (listof (listof instruction?))) any/c)]

  [asm-interp/io
   (->* () #:rest (or/c (*list/c instruction? string?) (*list/c (listof instruction?) string?)) any/c)]))


(require (except-in ffi/unsafe ->)
         "ast.rkt"
         "jit.rkt"
         "printer.rkt"
         (submod "printer.rkt" private))

(define extern-list/c
  (flat-named-contract
   'extern-list/c
   (λ (xs)
     (and (list? xs)
          (andmap extern? xs)
          (not (check-duplicates xs #:key extern-name))))))

(define (extern-value? x)
  (or (procedure? x) (cpointer? x)))

(define (jit? x)
  #t) ; FIXME

(define (machine-word? x)
  (or (exact-integer? x) (cpointer? x)))

(define A86_EXTERN_FUNCTION 0)
(define A86_EXTERN_GLOBAL   1)

(define (jit-trace-enabled?)
  (define v (getenv "A86_JIT_TRACE"))
  (and v (not (member v '("" "0" "false" "FALSE" "False")))))

(define (jit-trace fmt . args)
  (when (jit-trace-enabled?)
    (parameterize ([current-output-port (current-error-port)])
      (apply printf (string-append "a86-jit: " fmt "\n") args)
      (flush-output))))

(define (trace-extern-procedure name proc)
  (if (not (jit-trace-enabled?))
      proc
      (lambda args
        (jit-trace "extern ~a args=~s" name args)
        (with-handlers ([exn:fail?
                         (lambda (e)
                           (jit-trace "extern ~a raised: ~a"
                                      name
                                      (exn-message e))
                           (raise e))])
          (define vs
            (call-with-values
             (lambda () (apply proc args))
             list))
          (jit-trace "extern ~a result=~s" name vs)
          (apply values vs)))))

;; ------------------------------------------------------------
;; current JIT environment

(define current-jit
  (make-parameter (make-jit)))

(define current-externs
  (make-parameter '()))

(define jit-no-unload?
  (let ([v (getenv "A86_JIT_NO_UNLOAD")])
    (and v (not (member (string-downcase v) '("0" "false" "no" ""))))))

(define current-objects
  (make-parameter '()))

(define (reset-jit!)
  (define old (current-jit))
  (when old
    (jit-close old))
  (current-jit (make-jit)))

;; ------------------------------------------------------------
;; higher-level extern representations

(struct extern (name value ctype) #:transparent)

(struct cached-callback (name ctype wrapper fptr) #:transparent)

;; ------------------------------------------------------------
;; loaded program wrapper
;;
;; `ptr` is the raw native program handle.
;; `keepalive` holds any callback pointers so they are not GC'd
;; while the program is live.
;; `jit` is the owning JIT when the program was loaded into an isolated
;; one-shot JIT. `owned?` controls whether unload should close that JIT.

(struct asm-program (ptr keepalive jit owned?) #:transparent #:mutable)

;; ------------------------------------------------------------
;; helpers

(define (program->asm-string p)
  (with-output-to-string
    (λ ()
      (parameterize ([current-shared? #t])
        (asm-display p)))))

(define (resolve-object-path p)
  (define path
    (cond
      [(path? p) p]
      [(string? p) (string->path p)]))
  (path->string
   (simplify-path
    (path->complete-path path (current-directory)))))

;; Hold callback trampolines for the life of the process so long-running test
;; runs do not depend on per-load callback allocation or GC timing.
(define callback-cache (make-hasheq))

(define (cached-function-ptr name value ctype)
  (define entries (hash-ref callback-cache value '()))
  (define hit
    (for/or ([entry entries])
      (and (eq? name (cached-callback-name entry))
           (equal? ctype (cached-callback-ctype entry))
           entry)))
  (cond
    [hit
     (cached-callback-fptr hit)]
    [else
     (define wrapper (trace-extern-procedure name value))
     (define fptr (function-ptr wrapper ctype))
     (hash-set! callback-cache
                value
                (cons (cached-callback name ctype wrapper fptr) entries))
     fptr]))

(define (prepare-externs externs)
  (define keepalive '())
  (define bindings
    (for/vector ([x externs])
      (match-define (extern name value ctype) x)
      (cond
        [(procedure? value)
         (define fptr (cached-function-ptr name value ctype))
         (set! keepalive (cons fptr keepalive))
         (make-jit-extern-binding
          (symbol->string name)
          A86_EXTERN_FUNCTION
          fptr)]

        [(cpointer? value)
         (make-jit-extern-binding
          (symbol->string name)
          A86_EXTERN_GLOBAL
          value)])))

  (values bindings keepalive))

(define (prepare-object-files objs)
  (for/vector ([p objs])
    (resolve-object-path p)))

(define (arg->u64 x)
  (cond
    [(exact-integer? x)
     ;; jit.rkt passes _uint64 arguments, so normalize negatives mod 2^64
     (modulo x (arithmetic-shift 1 64))]
    [(cpointer? x)
     (cast x _pointer _uintptr)]))

;; ------------------------------------------------------------
;; public API

(define (asm-load prog
                  #:externs [externs (current-externs)]
                  #:objects [objs (current-objects)]
                  #:jit [jit #f])
  (define owned? (not jit))
  (define use-jit (or jit (make-jit)))
  (define asm-str (program->asm-string prog))
  (define-values (ext-vec keepalive) (prepare-externs externs))
  (define obj-vec (prepare-object-files objs))
  (jit-trace "load externs=~s objects=~s"
             (map extern-name externs)
             objs)
  (define p (jit-load use-jit asm-str obj-vec ext-vec))
  (asm-program p keepalive use-jit owned?))

(define (asm-call p label . args)
  (define raw (asm-program-ptr p))
  (unless raw
    (error 'asm-call "program has already been unloaded"))
  (define argv
    (list->vector (map arg->u64 args)))
  (with-handlers ([exn:fail?
                   (λ (e)
                     (jit-trace "call label=~s args=~s failed: ~a"
                                label
                                args
                                (exn-message e))
                     (raise e))])
    (jit-call (asm-program-ptr p) label argv)))

(define (asm-unload p)
  (define raw (asm-program-ptr p))
  (when raw
    (define maybe-jit (asm-program-jit p))
    (define owned? (asm-program-owned? p))
    (if jit-no-unload?
        (jit-trace "skip unload due to A86_JIT_NO_UNLOAD")
        (begin
          (if (and owned? maybe-jit)
              ;; For the default one-program JIT path, avoid explicitly
              ;; removing ORC resources before tearing down the whole JIT.
              (jit-close maybe-jit)
              (jit-unload raw))
          (set-asm-program-ptr! p #f)
          (set-asm-program-jit! p #f)))))

(define (call-with-asm-loaded prog f
                              #:externs [externs (current-externs)]
                              #:objects [objs (current-objects)]
                              #:jit [jit #f])
  (define p (asm-load prog
                      #:externs externs
                      #:objects objs
                      #:jit jit))
  (dynamic-wind
    void
    (λ () (f p))
    (λ () (asm-unload p))))

;; DOES NOT UNLOAD
(define (asm-interp . asm)
  (define-values (init-label code) (asm-fixup asm))
  (define p
    (asm-load code))
  (asm-call p init-label)
  #;
  (call-with-asm-loaded code
                        (λ (p) (asm-call p init-label))))

(define (asm-interp/io . asm+in)
  (match asm+in
    [(list asm ... in-str)
     (define in (open-input-string in-str))
     (define out (open-output-string))
     (parameterize ([current-input-port in]
                    [current-output-port out])
       (define r (apply asm-interp asm))
       (begin0 (cons r (get-output-string out))
         (close-output-port out)
         (close-input-port in)))]))


;; (listof (or/c instruction? (listof instruction?))) -> (listof instruction?)
(define (asm-fixup asm)
  (define a (apply seq asm))
  (define init-label
    (match (findf Label? a)
      [(Label ($ l)) l]
      [_ #f]))
  (define global?
    (and init-label
         (ormap (match-lambda
                  [(Global ($ g)) (eq? g init-label)]
                  [_ #f])
                a)))
  (cond
    [(and init-label global?) (values init-label (apply prog a))]
    [else (let ((i (symbol->label (gensym 'init))))
            (values i
                    (apply prog
                           (Global i)
                           (Label i)
                           a)))]))
