#lang racket/base

(require (for-syntax racket/base syntax/parse))

(provide (rename-out [day5-module-begin #%module-begin])
         #%app
         #%datum
         #%top
         day5-state
         day5-move)

(define-syntax (day5-module-begin stx)
  (syntax-case stx ()
    ((_ parsed-args ...)
     #'(#%module-begin
        parsed-args ...))))

(define program-state '())

(define (day5-state lst)
  (set! program-state lst))

(define (day5-move lst)
  (println "~a" lst))
