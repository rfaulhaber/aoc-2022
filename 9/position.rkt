#lang racket/base

(require racket/contract)

(provide (contract-out
          (struct position ((x number?) (y number?)))
          (apply-motion (-> position? motion-direction? position?))
          (is-adjacent? (-> position? position? boolean?))
          (dist (-> position? position? number?))
          (motion-direction? (-> symbol? boolean?))))

(define sqrt-2 (sqrt 2))

(struct position (x y) #:transparent)

(define (apply-motion pos direction)
  (case direction
    [(U) (struct-copy position pos (x (+ (position-x pos) 1)))]
    [(D) (struct-copy position pos (x (- (position-x pos) 1)))]
    [(R) (struct-copy position pos (y (+ (position-y pos) 1)))]
    [(L) (struct-copy position pos (y (- (position-y pos) 1)))]))

(define (motion-direction? direction)
  (member direction '(U D R L)))

(define (is-adjacent? pos other)
  (<= (dist pos other) sqrt-2)) ;; close enough

(define (dist pos other)
  (abs (sqrt (+ (expt (- (position-x other) (position-x pos)) 2) (expt (- (position-y other) (position-y pos)) 2)))))

(module+ test
  (require rackunit)

  (check-equal? (apply-motion (position 0 0) 'U) (position 1 0))

  (check-equal? (dist (position 0 0) (position 0 1)) 1)
  (check-equal? (dist (position 0 0) (position 0 5)) 5)
  (check-equal? (dist (position 1 0) (position 0 1)) sqrt-2)

  (check-equal? (is-adjacent? (position 1 0) (position 0 1)) #t)
  (check-equal? (is-adjacent? (position 0 0) (position 0 1)) #t)
  (check-equal? (is-adjacent? (position 0 0) (position 2 2)) #f))
