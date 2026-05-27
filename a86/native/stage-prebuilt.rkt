#lang racket

(require racket/runtime-path
         setup/cross-system)

(define-runtime-path native-dir ".")

(define lib-name
  (format "liba86_jit~a" (cross-system-type 'so-suffix)))

(define src
  (build-path native-dir "lib" lib-name))

(define dst-dir
  (build-path native-dir
              "prebuilt"
              (symbol->string (cross-system-type 'os))
              (symbol->string (cross-system-type 'arch))))

(define dst
  (build-path dst-dir lib-name))

(define (main)
  (unless (file-exists? src)
    (error 'stage-prebuilt
           (format "expected built library at ~a; run `make -C a86/native all` first"
                   src)))

  (make-directory* dst-dir)
  (copy-file src dst #t)

  (printf "a86: staged prebuilt library to ~a\n" dst))

(module+ main
  (main))
