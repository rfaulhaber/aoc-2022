#lang errortrace racket/base

(require racket/file)
(require racket/list)
(require racket/string)

(require "./position.rkt")
(require "./rope.rkt")
(require "./chain.rkt")

(define (parse-file filename)
  (map (lambda (line)
         (let ([split (string-split line " ")])
           (cons (string->symbol (first split)) (string->number (second split)))))
       (file->lines filename)))

(define (solve-part-1 filename)
  (let ([contents (parse-file filename)]
        [r (rope (position 0 0) (position 0 0))])
    (length
     (remove-duplicates
      (flatten
       (for/list ([instruction contents])
         (let ([tails '()])
           (for ([i (cdr instruction)])
             (set! r (apply-motion-to-rope r (car instruction)))
             (set! tails (append tails (list (rope-tail r)))))
           tails)))))))

(define (solve-part-2 filename)
  (let ([contents (parse-file filename)]
        [c (make-chain 10)])
    (length
     (remove-duplicates
      (flatten
       (for/list ([instruction contents])
         (let ([tails '()])
           (for ([i (cdr instruction)])
             (set! c (apply-motion-to-chain c (car instruction)))
             (set! tails (append tails (list (last c)))))
           tails)))))))

(module+ test
  (require rackunit)

  (define sample "./sample.txt")
  (define sample2 "./sample2.txt")

  (check-equal? (solve-part-1 sample) 13)
  (check-equal? (solve-part-2 sample) 0)
  (check-equal? (solve-part-2 sample2) 36)
  )

(module+ main
  (require "../utils/racket/cmd.rkt")

  (define part (make-parameter null))
  (define filename (make-command-line "day9" part))

  (if (null? (part))
      (begin
        (println "Expected --part to be provided.")
        (exit 1))
      (case (part)
        [(1) (solve-part-1 filename)]
        [(2) (solve-part-2 filename)]
        [else (println (format "invalid part: ~a" (part)))
              (exit 1)])))
