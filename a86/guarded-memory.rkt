#lang racket

(provide/contract
 [memory-allocated? (-> boolean?)]
 [guarded-malloc (->* (exact-positive-integer?) (ctype?) cpointer?)]
 [guarded-free (-> cpointer? (or/c #f void?))]
 [pointer->natural (-> cpointer? natural?)]
 [natural->pointer (-> natural? cpointer?)]
 [pointer-or-natural->natural (-> symbol? (or/c cpointer? natural?) natural?)]
 [pointer-or-natural->pointer (-> symbol? (or/c cpointer? natural?) cpointer?)]
 [pointer-ref (->* ((or/c cpointer? natural?)) (ctype?) any)]
 [pointer-in-allocated-memory? (-> (or/c cpointer? natural?) boolean?)])

(require (rename-in ffi/unsafe [-> _->]))

(struct region (base-pointer size type)
  #:extra-constructor-name make-region
  #:transparent)

(define (region-range region)
  (unless (region? region)
    (raise-argument-error 'region-range "region?" region))
  (define base (pointer->natural (region-base-pointer region)))
  (values base (+ base (region-size region))))

(define (in-region-range? region pointer-or-natural)
  (unless (region? region)
    (raise-argument-error 'in-region-range? "region?" region))
  (define-values (lo hi) (region-range region))
  (define i (pointer-or-natural->natural 'in-region-range? pointer-or-natural))
  (and (>= i lo)
       (<  i hi)))

(define regions (make-parameter '()))

(define (memory-allocated?)
  (not (empty? (regions))))

(define (guarded-malloc size [type _int64])
  (define p (malloc _int64 size 'raw 'failok))
  (define r (make-region p size type))
  (regions (cons r (regions)))
  p)

(define (guarded-free pointer)
  (for/or ([region (in-list (regions))])
    (and (ptr-equal? pointer (region-base-pointer region))
         (begin (free pointer)
                (regions (filter (λ (other-region) (eq? region other-region)) (regions)))
                (void)))))

(define (pointer-in-allocated-memory? pointer)
  (define i (pointer-or-natural->natural 'pointer-in-allocated-memory? pointer))
  (for/or ([region (in-list (regions))])
    (in-region-range? region i)))

(define (pointer->natural p)
  (unless (cpointer? p)
    (raise-argument-error 'pointer->natural "cpointer?" p))
  (cast p _pointer _int64))

;; TODO: Should probably check the bounds on the integer first.
(define (natural->pointer i)
  (unless (natural? i)
    (raise-argument-error 'natural->pointer "natural?" i))
  (cast i _int64 _pointer))

(define (pointer-ref pointer-or-natural [type _int64])
  (define p (pointer-or-natural->pointer 'pointer-ref pointer-or-natural))
  (unless (ctype? type)
    (raise-argument-error 'pointer-ref "ctype?" type))
  (ptr-ref p type))

(define (pointer-or-natural->natural who pointer-or-natural)
  (cond
    [(cpointer? pointer-or-natural) (pointer->natural pointer-or-natural)]
    [(natural? pointer-or-natural) pointer-or-natural]
    [else (raise-argument-error who '(or/c cpointer? natural?) pointer-or-natural)]))

(define (pointer-or-natural->pointer who pointer-or-natural)
  (cond
    [(cpointer? pointer-or-natural) pointer-or-natural]
    [(natural? pointer-or-natural) (natural->pointer pointer-or-natural)]
    [else (raise-argument-error who '(or/c cpointer? natural?) pointer-or-natural)]))
