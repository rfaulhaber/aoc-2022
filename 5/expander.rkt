#lang racket/base

(require (for-syntax racket/base syntax/parse))

(provide (rename-out [day5-module-begin #%module-begin])
         #%app
         #%datum
         state
         move)

(define-syntax (day5-module-begin stx)
  (syntax-case stx ()
    ((_ parsed-args ...)
     #'(#%module-begin
        parsed-args ...))))

(define program-state '())

(define (state lst)
  (println "in expander")
  (set! program-state lst))

(define (move lst)
  (println "move!"))
