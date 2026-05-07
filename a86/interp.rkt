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
         #:jit jit?)
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
         #:jit jit?)
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

;; ------------------------------------------------------------
;; current JIT environment

(define current-jit
  (make-parameter (make-jit)))

(define current-externs
  (make-parameter '()))

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

;; ------------------------------------------------------------
;; loaded program wrapper
;;
;; `ptr` is the raw native program handle.
;; `keepalive` holds any callback pointers so they are not GC'd
;; while the program is live.

(struct asm-program (ptr keepalive) #:transparent #:mutable)

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

(define (prepare-externs externs)
  (define keepalive '())
  (define bindings
    (for/vector ([x externs])
      (match-define (extern name value ctype) x)
      (cond
        [(procedure? value)
         (define fptr (function-ptr value ctype))
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
                  #:jit [jit (current-jit)])
  (define _reset (reset-jit!)) ;; FIXME: heavy handed way to avoid problems with langs test suite failures
  (define asm-str (program->asm-string prog))
  (define-values (ext-vec keepalive) (prepare-externs externs))
  (define obj-vec (prepare-object-files objs))
  (define p (jit-load jit asm-str obj-vec ext-vec))
  (asm-program p keepalive))

(define (asm-call p label . args)
  (define raw (asm-program-ptr p))
  (unless raw
    (error 'asm-call "program has already been unloaded"))
  (define argv
    (list->vector (map arg->u64 args)))
  (jit-call (asm-program-ptr p) label argv))

(define (asm-unload p)
  (define raw (asm-program-ptr p))
  (when raw
    (jit-unload raw)
    (set-asm-program-ptr! p #f)))

(define (call-with-asm-loaded prog f
                              #:externs [externs (current-externs)]
                              #:objects [objs (current-objects)]
                              #:jit [jit (current-jit)])
  (define p (asm-load prog
                      #:externs externs
                      #:objects objs
                      #:jit jit))
  (dynamic-wind
    void
    (λ () (f p))
    (λ () (asm-unload p))))


(define (asm-interp . asm)
  (define-values (init-label code) (asm-fixup asm))
  (define p (asm-load code))
  (asm-call p init-label))

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
