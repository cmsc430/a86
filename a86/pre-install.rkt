#lang racket
(require setup/cross-system)

(provide pre-installer)
(define (pre-installer collects-dir collection-dir)
  (define coll-dir
    (simplify-path (path->complete-path collection-dir)))
  (check-x86)
  (install-jit! (build-path coll-dir "native")))

(define (check-x86)
  (unless (eq? 'x86_64 (system-type 'arch))
    (error 'a86-installer
           "This library requires an x86_64 installation of Racket; yours is a ~a (~a)."
           (system-type 'arch)
           (system-type 'os))))

(define (install-jit! jit-dir)
  (or (install-prebuilt-jit jit-dir)
      (build-jit jit-dir)))

(define (install-prebuilt-jit jit-dir)
  (define src (prebuilt-lib-path jit-dir))
  (define dst (lib-path jit-dir))
  (and (file-exists? src)
       (begin
         (make-directory* (path-only dst))
         (copy-file src dst #t)
         (printf "a86: installed prebuilt native JIT library ~a\n" src)
         (flush-output)
         #t)))

(define (build-jit jit-dir)

  (define makefile-path
    (build-path jit-dir "Makefile"))

  (define built-lib-path
    (lib-path jit-dir))

  (unless (directory-exists? jit-dir)
    (error 'build-jit
           (format "expected native directory at ~a" jit-dir)))

  (unless (file-exists? makefile-path)
    (error 'build-jit
           (format "expected Makefile at ~a" makefile-path)))

  (printf "a86: building native JIT library in ~a for ~a\n"
          jit-dir
          (cross-system-type 'os))
  (flush-output)

  (parameterize ([current-directory jit-dir])
    (define ok?
      (or (find-executable-path "gmake")
          (find-executable-path "make")))
    (unless ok?
      (error 'build-jit
             "could not find 'make' or 'gmake' in PATH"))

    (define make-exe ok?)

    (define status
      (system*/exit-code make-exe "all"))

    (unless (zero? status)
      (error 'build-jit
             (format "native JIT build failed with exit code ~a" status))))

  (unless (file-exists? built-lib-path)
    (error 'build-jit
           (format "build completed but did not produce ~a" built-lib-path)))

  (printf "a86: built ~a\n" built-lib-path)
  (flush-output))

(define (lib-name)
  (string->path
   (string-append "liba86_jit" (target-so-suffix))))

(define (lib-path jit-dir)
  (build-path jit-dir "lib" (lib-name)))

(define (prebuilt-lib-path jit-dir)
  (build-path jit-dir
              "prebuilt"
              (symbol->string (cross-system-type 'os))
              (symbol->string (cross-system-type 'arch))
              (lib-name)))

(define (target-so-suffix)
  (case (cross-system-type 'os)
    [(macosx) ".dylib"]
    [(unix)   ".so"]
    [else
     (error 'build-jit
            (format "unsupported target OS: ~a" (cross-system-type 'os)))]))
