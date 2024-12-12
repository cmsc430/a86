#lang racket
(provide check-nasm-available nasm-version nasm-version-2.15+?)
(require racket/gui/dynamic)

(define nasm-msg
  #<<HERE
nasm not found: either you have not installed nasm
or it cannot be found in your PATH: ~a.~a
HERE
  )

(define macosx-msg
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
(define (nasm-version-string)
  (parameterize ([current-output-port (open-output-string)]
                 [current-error-port (open-output-string)])
    (and (system "nasm -v")
         (get-output-string (current-output-port)))))

(define (nasm-version-2.15+?)
  (match (nasm-version)
    [(list maj min) (and (>= maj 2) (>= min 15))]
    [_ #f]))

;; -> [Maybe (list Natural Natural)]
(define (nasm-version)
  (match (nasm-version-string)
    [#f #f]
    [(regexp #rx"([0-9]+)\\.([0-9]+)"
             (list _ (app string->number maj) (app string->number min)))
     (list maj min)]))

;; -> Void
;; Errors if nasm is not available, warns if available but below 2.15
(define (check-nasm-available)
  (define v (nasm-version))
  (unless v
    (error (format nasm-msg
                   (getenv "PATH")
                   (if (and (drracket?) (macos?) (launched-with-finder?))  macosx-msg ""))))
  (unless (nasm-version-2.15+?)
    (eprintf "nasm 2.15 or later is recommended; some faatures may not work as expected.")))
