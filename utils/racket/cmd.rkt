#lang racket/base

(require racket/cmdline)

(provide make-command-line)

(define (make-command-line name part)
  (command-line
   #:program name
   #:once-each [("-p" "--part") pt "Which part to solve." (part (string->number pt))]
   #:args (filename)
   filename))
