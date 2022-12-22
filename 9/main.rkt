#lang errortrace racket/base

(require racket/file)
(require racket/string)
(require racket/list)

(require "./position.rkt")
(require "./rope.rkt")

(define (parse-file filename)
  (map (lambda (line)
         (let ([split (string-split line " ")])
           (cons (string->symbol (first split)) (string->number (second split)))))
       (file->lines filename)))

(define (solve-part-1 filename)
  (println (format "hello ~a" (parse-file filename))))

(define (solve-part-2 filename)
  (println (format "hello ~a" filename)))

(module+ test
  (require rackunit)

  (define sample "./sample.txt")

  (check-equal? (solve-part-1 sample) 13)
  ;; (check-equal? (solve-part-2 sample))
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
