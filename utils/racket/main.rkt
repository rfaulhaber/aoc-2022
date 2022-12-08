#lang racket/base

(provide split-by)

(require racket/list)

(define (split-by lst n)
   (if (not (empty? lst))
       (cons (take lst n) (split-by (drop lst n) n))
       '() ))
