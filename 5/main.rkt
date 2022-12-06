#lang racket/base

(module reader racket
  (provide read-syntax)
  (require day5/reader))

(module+ main
  (require racket/cmdline)
  (require racket/sandbox)
  (require racket/port)

  ;; https://stackoverflow.com/questions/59855704/how-to-tell-make-module-evaluator-to-use-a-custom-reader-like-lang-does
  (define (eval-from-cli filename)
    (let* ((contents (open-input-file filename))
           (stx (read contents))
           (evaluator (make-module-evaluator (string-append "#lang day5\n" (port->string contents)))))
      (displayln "contents: ~a" contents)))

  (command-line
    #:program ""
    #:args (filename-or-expr)
    (eval-from-cli filename-or-expr)))
