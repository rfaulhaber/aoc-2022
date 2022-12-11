#lang errortrace racket/base

(require racket/file)
(require racket/string)
(require racket/list)
(require "../utils/racket/main.rkt")
(require "./filesystem.rkt")

(define (get-raw-file-sizes filesystem)
  (let ((dir-sizes (hash)))
    (for ([file filesystem])
      (let ((parent-dir (get-parent-dir (car file))))
        (set! dir-sizes (hash-set dir-sizes parent-dir (if (hash-has-key? dir-sizes parent-dir)
                                                           (+ (hash-ref dir-sizes parent-dir) (cdr file))
                                                           (cdr file))))))
    dir-sizes))

(define (resolve-subdirs dir-sizes-hash)
  (let ((dir-sizes dir-sizes-hash)
        (dirs-list (hash->list dir-sizes-hash)))

    (for* ([dir dirs-list]
           [other-dir dirs-list]
           #:when (is-subdir? (car other-dir) (car dir)))

      (set! dir-sizes (hash-set dir-sizes (car dir) (+ (cdr dir) (cdr other-dir)))))
    dir-sizes))

(define (sum-dirs dir-sizes-hash dirs)
  (foldl + 0 (map (lambda (dir)
                    (hash-ref dir-sizes-hash dir)) dirs)))

(define (solve-part1 filename)
  (let* ((filesystem (make-filesystem (file->string filename)))
         (raw-file-sizes (get-raw-file-sizes filesystem))
         (resolved-sizes (resolve-subdirs raw-file-sizes))
         (dirs-over (filter (lambda (el)
                              (<= (cdr el) 100000)) (hash->list resolved-sizes))))
    (println (format "raw ~a" dirs-over))
    (foldl + 0 (map cdr dirs-over))))

(define (solve part filename)
  (case part
    [(1) (solve-part1 filename)]))

(module+ main
  (require racket/cmdline)

  (define part (make-parameter 1))

  (command-line
   #:program "day7"
   #:once-each
   [("-p" "--part") pt "Which part to solve." (part pt)]
   #:args (filename)
   (solve (part) filename)))


(module+ test
  (require rackunit)
  (check-equal? (solve-part1 "sample.txt") 95437))
