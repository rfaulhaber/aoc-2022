#lang racket/base

(require racket/class)

(provide stack%)

(define stack%
  (class object%
    (super-new)
    (field (inner-list '()))
    (define/public (push el)
      (set! inner-list (cons el inner-list)))
    (define/public (pop)
      (if (null? inner-list)
          '()
          (let ((top (car inner-list)))
            (set! inner-list (cdr inner-list))
            top)))
    (define/public (stack->list)
      inner-list)
    (define/public (empty?)
      (null? inner-list))))

(module+ test
  (require rackunit)

  (let ((stack (new stack%)))
    (check-equal? (send stack empty?) #t)
    (send stack push 'foo)
    (check-equal? (get-field inner-list stack) '(foo)))

  (let ((stack (new stack%)))
    (send stack push 'foo)
    (send stack push 'bar)
    (check-equal? (send stack pop) 'bar)
    (check-equal? (send stack pop) 'foo)
    (check-equal? (send stack pop) '()))

  (let ((stack (new stack%)))
    (send stack push 'foo)
    (send stack push 'bar)
    (send stack push 'baz)
    (check-equal? (send stack stack->list) '(baz bar foo))))
