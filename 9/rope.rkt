#lang racket/base

(require racket/contract)
(require "./position.rkt")

(provide (contract-out
          (struct rope ((head position?) (tail position?)))
          (apply-motion-to-rope (-> rope? motion-direction? rope?))))

(struct rope (head tail) #:transparent)

(define (apply-motion-to-rope r direction)
  (let ((new-head (apply-motion (rope-head r) direction)))
    (if (is-adjacent? new-head (rope-tail r))
        (struct-copy rope r (head new-head))
        (rope new-head (rope-head r)))))

(module+ test
  (require rackunit)

  (check-equal? (apply-motion-to-rope (rope (position 0 1) (position 0 0)) 'R)
                (rope (position 0 2) (position 0 1)))

  (check-equal? (apply-motion-to-rope (rope (position 0 0) (position 0 0)) 'R)
                (rope (position 0 1) (position 0 0)))

  (check-equal? (apply-motion-to-rope (rope (position 0 4) (position 0 3)) 'U)
                (rope (position 1 4) (position 0 3)))

  (check-equal? (apply-motion-to-rope (rope (position 1 4) (position 0 3)) 'U)
                (rope (position 2 4) (position 1 4))))
