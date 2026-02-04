#lang racket

(provide check-clang-available
         clang-version
         clang-version-14+?)

(require racket/gui/dynamic)

(define clang-missing-msg
  #<<HERE
clang not found: either you have not installed clang
or it cannot be found in your PATH: ~a.~a
HERE
  )

(define finder-launch-msg
  #<<HERE


It appears you launched DrRacket using Finder, which
does not properly set up the PATH environment variable.
Try launching DrRacket from the command line using the
'drracket' command.
HERE
  )

(define (macos?)
  (eq? 'macosx (system-type 'os)))

;; This will lie if you happen to run drracket from /, but that's OK.
(define (launched-with-finder?)
  (equal? (string->path "/") (find-system-path 'orig-dir)))

(define (drracket?)
  (gui-available?))

;; -> [Maybe String]
(define (clang-version-string)
  (parameterize ([current-output-port (open-output-string)]
                 [current-error-port (open-output-string)])
    (and (system (format "~aclang -v 2>&1"
                         (if (macos?)
                             "arch -x86_64 "
                             "")))
         (get-output-string (current-output-port)))))

(define (clang-version-14+?)
  (match (clang-version)
    [(list major _ _)
     (>= major 14)]
    [_ #f]))

;; -> [Maybe (list Natural Natural Natural)]
(define (clang-version)
  (match (clang-version-string)
    [#f #f]
    [(regexp #rx"clang version ([0-9]+)\\.([0-9]+)\\.([0-9]+)"
             (list _
                   (app string->number major)
                   (app string->number minor)
                   (app string->number patch)))
     (list major minor patch)]))

;; -> Void
(define (check-clang-available)
  (define v (clang-version))
  (unless v
    (error (format clang-missing-msg
                   (getenv "PATH")
                   (if (and (drracket?) (macos?) (launched-with-finder?)) finder-launch-msg ""))))
  (unless (clang-version-14+?)
    (eprintf "clang 16.0.0 or later is recommended; some features may not work as expected.\n")))
