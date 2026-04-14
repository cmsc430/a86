#lang racket
(require setup/cross-system)

(provide pre-installer)
(define (pre-installer collects-dir collection-dir)
  (define coll-dir
    (simplify-path (path->complete-path collection-dir)))
  (check-x86)
  (build-jit (build-path coll-dir "native")))

(define (check-x86)
  (unless (eq? 'x86_64 (system-type 'arch))
    (error 'a86-installer
           "This library requires an x86_64 installation of Racket; yours is a ~a (~a)."
           (system-type 'arch)
           (system-type 'os))))

(define (build-jit jit-dir)

  (define makefile-path
    (build-path jit-dir "Makefile"))

  (define lib-name
    (string->path
     (string-append "liba86_jit" (target-so-suffix))))

  (define lib-path
    (build-path jit-dir "lib" lib-name))

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

  (unless (file-exists? lib-path)
    (error 'build-jit
           (format "build completed but did not produce ~a" lib-path)))

  (printf "a86: built ~a\n" lib-path)
  (flush-output))

(define (target-so-suffix)
  (case (cross-system-type 'os)
    [(macosx) ".dylib"]
    [(unix)   ".so"]
    [else
     (error 'build-jit
            (format "unsupported target OS: ~a" (cross-system-type 'os)))]))
