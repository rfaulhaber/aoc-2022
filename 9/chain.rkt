#lang racket/base

(require racket/contract)
(require racket/list)
(require "./position.rkt")

(provide
 (contract-out
  (apply-motion-to-chain (-> chain/c motion-direction? chain/c))
  (make-chain (->* (number?) (number? number?) chain/c))))

(define chain/c (listof position?))

(define (apply-motion-to-chain c direction)
  (cond
    [(null? c) '()]
    [(= (length c) 2) (apply-motion-to-head-tail (first c) (second c) direction)]
    [else
     (let ([new-head (apply-motion (car c) direction)]
           [first-tail (car (cdr c))])
       (if (is-adjacent? new-head first-tail)
           (append (list new-head) (cdr c))
           (append (list new-head) (apply-motion-to-chain (cdr c) direction))))]))

(define (apply-motion-to-head-tail head tail direction)
  (let ([new-head (apply-motion head direction)])
    (if (is-adjacent? new-head tail)
        (list new-head tail)
        (list new-head (apply-motion tail direction)))))

(define (make-chain len (initial-x 0) (initial-y 0))
  (for/list ([i len])
    (position initial-x initial-y)))

(module+ test
  (require rackunit)

  (check-equal? (make-chain 3) (list (position 0 0) (position 0 0) (position 0 0)))

  (check-equal? (apply-motion-to-chain (make-chain 4) 'R)
                (list
                 (position 0 1)
                 (position 0 0)
                 (position 0 0)
                 (position 0 0)))

  (check-equal? (apply-motion-to-chain
                 (list
                  (position 0 2)
                  (position 0 1)
                  (position 0 0)
                  (position 0 0)) 'R)
                (list
                 (position 0 3)
                 (position 0 2)
                 (position 0 1)
                 (position 0 0)))

  (check-equal? (apply-motion-to-chain
                 (list
                  (position 2 4)
                  (position 1 4)
                  (position 1 3)
                  (position 1 2)
                  (position 1 1)
                  (position 0 0)) 'U)
                (list
                  (position 3 4)
                  (position 2 4)
                  (position 1 3)
                  (position 1 2)
                  (position 1 1)
                  (position 0 0))))
