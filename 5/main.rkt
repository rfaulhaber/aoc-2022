#lang racket/base

(module reader racket
  (provide read-syntax)
  (require day5/reader))

(module+ main
  (require racket/cmdline)
  (require day5/reader)
  (require day5/expander)

  (define (eval-from-cli filename)
    (let* ((contents (open-input-file filename)))
      (eval-syntax (read-syntax #f contents))))

  (command-line
    #:program ""
    #:args (filename-or-expr)
    (eval-from-cli filename-or-expr)))
