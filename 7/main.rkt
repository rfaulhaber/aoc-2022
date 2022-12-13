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
  (set->list (list->set (map get-parent-dir (map car filesystem)))))

(define (resolve-subdirs dir-sizes-hash)
  (let* ((dir-sizes dir-sizes-hash)
         (files-list (sort (hash->list dir-sizes-hash) string<? #:key car))
         (files-only (map car files-list)))

    (for* ([dir files-only]
           [subdir (find-all-subdirs dir files-only)])
      (let ((dir-size 0))
        (println (format "dir ~a, subdir: ~a" dir subdir))
        (set! dir-size (+ dir-size (hash-ref dir-sizes subdir)))

        (when (> dir-size 0)
          (set! dir-sizes (hash-set dir-sizes dir (+ (hash-ref dir-sizes dir) dir-size))))))
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

    ;; (for ([dir (hash->list resolved-sizes)])
    ;;   (displayln (format "~a ~a" (car dir) (cdr dir))))

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
                (sort '("/" "/foo" "/foo/bar" "/foo/bar/quuz" "/bar" "/bar/baz" "/quux/bar") string<?)))
