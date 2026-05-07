#lang racket

(provide make-jit
         jit-close
         jit-load
         jit-call
         jit-unload
         make-jit-extern-binding)

(require ffi/unsafe
         ffi/unsafe/define
         racket/runtime-path
         "callback.rkt")

;; ----------------------------------------
;; Native interface

(define-runtime-path here ".")

(define liba86
  (ffi-lib
   (build-path here
               "native"
               "lib"
               (format "liba86_jit~a" (system-type 'so-suffix)))))

(define-ffi-definer define-a86 liba86)

;; opaque types
(define-cpointer-type _a86_jit_t)
(define-cpointer-type _a86_program_t)

;; result struct
(define-cstruct _a86_call_result
  ([ok _int]
   [value _int64]
   [error_message _pointer]))

;; extern binding
(define _a86_extern_kind_t _int)

(define-cstruct _a86_extern_binding
  ([name _string]
   [kind _a86_extern_kind_t]
   [value _pointer]))

;; Raw call-time layout used to pin C strings explicitly while loading.
(define-cstruct _a86_extern_binding_raw
  ([name _pointer]
   [kind _a86_extern_kind_t]
   [value _pointer]))

;; native functions
(define-a86 a86_jit_create
  (_fun -> _a86_jit_t))

(define-a86 a86_jit_destroy
  (_fun _a86_jit_t -> _void))

(define-a86 a86_jit_last_error
  (_fun _a86_jit_t -> _pointer))

(define-a86 a86_jit_load
  (_fun _a86_jit_t
        _string
        _pointer _int        ; object files
        _pointer _int        ; externs
        -> _a86_program_t))

(define-a86 a86_program_call
  (_fun _a86_program_t
        _string
        _pointer _int
        -> _a86_call_result))

(define-a86 a86_program_unload
  (_fun _a86_program_t -> _void))

;; ----------------------------------------
;; helpers

(define (decode-error-pointer p)
  (if (ptr-equal? p #f)
      "unknown error"
      (cast p _pointer _string/utf-8)))

(define (jit-error jit who fallback)
  (define p (a86_jit_last_error jit))
  (error who
         (if (ptr-equal? p #f)
             fallback
             (decode-error-pointer p))))

(define (vector->c-array vec type)
  (define n (vector-length vec))
  (if (zero? n)
      #f
      (let ([ptr (malloc type n)])
        (for ([i (in-range n)])
          (ptr-set! ptr type i (vector-ref vec i)))
        ptr)))

(define (copy-cstring s)
  (define bs (string->bytes/utf-8 s))
  (define n (add1 (bytes-length bs)))
  (define ptr (malloc _byte n))
  (for ([i (in-range (bytes-length bs))])
    (ptr-set! ptr _byte i (bytes-ref bs i)))
  (ptr-set! ptr _byte (sub1 n) 0)
  ptr)

(define (extern-vector->c-array vec)
  (define n (vector-length vec))
  (if (zero? n)
      (values #f '())
      (let ([ptr (malloc _a86_extern_binding_raw n)])
        (define keepalive '())
        (for ([i (in-range n)])
          (define binding (vector-ref vec i))
          (define name-ptr (copy-cstring (a86_extern_binding-name binding)))
          (set! keepalive (cons name-ptr keepalive))
          (ptr-set! ptr
                    _a86_extern_binding_raw
                    i
                    (make-a86_extern_binding_raw
                     name-ptr
                     (a86_extern_binding-kind binding)
                     (a86_extern_binding-value binding))))
        (values ptr keepalive))))

;; ----------------------------------------
;; exported constructors/helpers

(define (make-jit-extern-binding name kind value)
  (make-a86_extern_binding name kind value))

;; ----------------------------------------
;; exported operations

(define (make-jit)
  (or (a86_jit_create)
      (error 'make-jit "failed to create JIT")))

(define (jit-close jit)
  (a86_jit_destroy jit))

(define (jit-load jit code obj-vec ext-vec)
  (define obj-ptr (vector->c-array obj-vec _string))
  (define-values (ext-ptr ext-keepalive) (extern-vector->c-array ext-vec))
  (define p
    (a86_jit_load jit
                  code
                  obj-ptr (vector-length obj-vec)
                  ext-ptr (vector-length ext-vec)))
  (void ext-keepalive)
  (unless p
    (jit-error jit 'jit-load "failed to load program"))
  p)

(define (jit-call p label args)
  (define argc (vector-length args))
  (define argv (vector->c-array args _uint64))
  (define r
    (guard-foreign-escape
     (a86_program_call p (symbol->string label) argv argc)))
  (if (= 1 (a86_call_result-ok r))
      (a86_call_result-value r)
      (error 'jit-call
             (decode-error-pointer (a86_call_result-error_message r)))))

(define (jit-unload p)
  (a86_program_unload p))
