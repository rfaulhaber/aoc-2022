#lang racket/base

(require racket/file)
(require "./filesystem.rkt")

(define (solve-part1 filename)
  (let* ((filesystem (make-filesystem (file->string filename)))
         (file-sizes (get-file-sizes filesystem))
         (dirs-over (filter (lambda (el)
                              (<= (cdr el) 100000)) file-sizes)))

    (foldl + 0 (map cdr dirs-over))))

(define (solve-part2 filename)
  (let* ((filesystem (make-filesystem (file->string filename)))
         (file-sizes (get-file-sizes filesystem))
         (free-space (- 70000000 (cdr (assoc "/" file-sizes))))
         (required-space (- 30000000 free-space))
         (size-distances (sort (filter (lambda (el)
                                         (>= (cdr el) required-space)) file-sizes)
                               < #:key cdr))
         (smallest-key (caar size-distances))
         (result (cdr (assoc smallest-key file-sizes))))
    result))

(define (solve part filename)
  (case part
    [("1") (solve-part1 filename)]
    [("2") (solve-part2 filename)]))

(module+ main
  (require racket/cmdline)

  (define part (make-parameter "1"))

  (command-line
   #:program "day7"
   #:once-each
   [("-p" "--part") pt "Which part to solve." (part pt)]
   #:args (filename)
   (solve (part) filename)))


(module+ test
  (require rackunit)

  (check-equal? (solve-part1 "sample.txt") 95437)

  (check-equal? (solve-part2 "sample.txt") 24933642))
