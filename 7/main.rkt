#lang errortrace racket/base

(require racket/file)
(require racket/string)
(require racket/list)
(require racket/set)
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

(define (find-all-subdirs path dir-list)
  (filter (lambda (dir)
            (is-subdir? dir path)) dir-list))

(define (find-all-dirs dir-list)
  (set->list (list->set (map get-parent-dir dir-list))))

(define (get-all-dirs filesystem)
  (let ((files (map car filesystem)))
    (set->list (list->set (flatten (map get-parent-dirs files))))))

(define (sum-dirs dir-sizes-hash dirs)
  (foldl + 0 (map (lambda (dir)
                    (hash-ref dir-sizes-hash dir)) dirs)))

(define (sum-all-with-prefix dir filesystem)
  (foldl + 0  (map cdr (filter (lambda (d)
                                 (string-prefix? (car d) dir))
                               filesystem))))

(define (get-file-sizes filesystem)
  (let* ((file-hash (make-hash filesystem))
         (dirs (get-all-dirs filesystem))
         (dir-sizes (hash)))

    (for ([dir dirs])
      (let* ([subdirs (find-all-subdirs dir dirs)])
        (for ([subdir subdirs])
          (set! dir-sizes (hash-set dir-sizes subdir (sum-all-with-prefix subdir filesystem))))))

    (hash->list dir-sizes)))

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
  (check-equal? (find-all-subdirs "/foo" '("/foo/bar" "/foo/baz" "/bar" "/bar/baz"))
                '("/foo/bar" "/foo/baz"))

  (check-equal? (sort
                 (get-all-dirs '(("/foo.txt" . 100)
                                 ("/bar.txt" . 200)
                                 ("/foo/foo.txt" . 100)
                                 ("/foo/bar.txt" . 100)
                                 ("/foo/baz.txt" . 100)
                                 ("/foo/bar/baz.txt" . 100)
                                 ("/foo/bar/quux.txt" . 100)
                                 ("/foo/bar/quuz/foo.txt" . 100)
                                 ("/foo/bar/quuz/bar.txt" . 100)
                                 ("/bar/baz.txt" . 100)
                                 ("/bar/baz/quux.txt" . 100)
                                 ("/quux/bar/quuz.txt" . 100)
                                 ))
                 string<?)
                (sort '("/" "/foo" "/foo/bar" "/foo/bar/quuz" "/bar" "/bar/baz" "/quux/bar" "/quux") string<?))

  (check-equal? (sum-all-with-prefix "/foo/bar" '(("/foo.txt" . 100)
                                                  ("/foo/bar/baz" . 100)
                                                  ("/foo/biz" . 100)))
                100)

  (check-equal? (solve-part2 "sample.txt") 24933642))
