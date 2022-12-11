#lang racket/base

(provide split-by displayfmt displayfmtln)

(require racket/list)

(define (split-by lst n)
  (if (not (empty? lst))
      (cons (take lst n) (split-by (drop lst n) n))
      '() ))

(define-syntax displayfmt
  (syntax-rules ()
    [(displayfmt msg) (display msg)]
    [(displayfmt msg ...) (display (format msg ...))]))

(define-syntax displayfmtln
  (syntax-rules ()
    [(displayfmtln msg) (displayln msg)]
    [(displayfmtln msg ...) (displayln (format msg ...))]))
