#lang racket

(define (load-input path)
  (file->lines path))

(define input (load-input "./input.txt"))

(module+ test
  (require rackunit)

  (define sample (load-input "./sample.txt")))
