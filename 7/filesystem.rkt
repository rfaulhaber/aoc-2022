#lang racket

(require racket/string)
(require racket/list)

(provide make-filesystem
         get-parent-dir
         is-subdir?
         get-parent-dirs
         get-next-dir
         find-all-subdirs
         find-all-dirs
         get-all-dirs
         sum-all-with-prefix
         get-file-sizes)

(define (get-parent-dir path)
  (if (string=? path "/")
      "/"
      (string-join (drop-right (string-split path "/") 1) "/" #:before-first "/")))

(define (is-subdir? candidate dir)
  (cond
    [(string=? dir "/") #t]
    [(string=? dir candidate) #f]
    [else (string-prefix? candidate dir)]))

(define (get-parent-dirs path)
  (let ((parent-dir (get-parent-dir path)))
    (cond
      [(string=? path "/") '()]
      [(string=? parent-dir "/") (list "/")]
      (else (append (list (get-parent-dir path)) (get-parent-dirs (get-parent-dir path)))))))

(define (get-next-dir dir path)
  (string-join (append (string-split path "/") (list dir)) "/" #:before-first "/"))

(define (find-all-subdirs path dir-list)
  (filter (lambda (dir)
            (is-subdir? dir path)) dir-list))

(define (find-all-dirs dir-list)
  (set->list (list->set (map get-parent-dir dir-list))))

(define (get-all-dirs filesystem)
  (let ((files (map car filesystem)))
    (set->list (list->set (flatten (map get-parent-dirs files))))))

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

(define (transform-cd dir current-dir)
  (cond
    [(string=? dir "..") (get-parent-dir current-dir)]
    [(string=? dir "/") "/"]
    [else (get-next-dir dir current-dir)]))

(define (transform-ls lines current-dir file-pairs)
  (append file-pairs
          (for/list ([line lines]
                     #:unless (string-prefix? line "dir"))
            (let* ((line-split (string-split line " "))
                   (size (string->number (car line-split)))
                   (name (cadr line-split)))
              (cons (if (string=? current-dir "/")
                        (string-append "/" name)
                        (string-append current-dir "/" name)) size)))))

(define (make-filesystem input)
  (let ((cmds (map string-trim (string-split input "$")))
        (file-pairs '())
        (current-dir ""))

    (for ((cmd cmds))
      (if (string-prefix? cmd "cd")
          (set! current-dir (transform-cd (substring cmd 3) current-dir))
          (set! file-pairs (transform-ls (drop (string-split cmd "\n") 1) current-dir file-pairs))))

    (sort file-pairs string<? #:key car)))

(module+ test
  (require rackunit)

  (require racket/file)

  (define sample (file->string "./sample.txt"))

  (check-equal?
   (get-parent-dir "/") "/")

  (check-equal?
   (get-parent-dir "/foo") "/")

  (check-equal?
   (get-parent-dir "/foo/bar") "/foo")

  (check-equal?
   (get-parent-dir "/foo/bar/baz.bat") "/foo/bar")

  (check-equal?
   (get-next-dir "foo" "/") "/foo")

  (check-equal?
   (get-next-dir "bar" "/foo") "/foo/bar")

  (check-equal?
   (transform-cd "bar" "/foo") "/foo/bar")

  (check-equal?
   (transform-cd "baz" "/foo/bar") "/foo/bar/baz")

  (check-equal?
   (transform-cd ".." "/foo/bar") "/foo")

  (check-equal?
   (transform-cd "/" "/foo/bar") "/")

  ;; - / (dir)
  ;;   - a (dir)
  ;;     - e (dir)
  ;;       - i (file, size=584)
  ;;     - f (file, size=29116)
  ;;     - g (file, size=2557)
  ;;     - h.lst (file, size=62596)
  ;;   - b.txt (file, size=14848514)
  ;;   - c.dat (file, size=8504156)
  ;;   - d (dir)
  ;;     - j (file, size=4060174)
  ;;     - d.log (file, size=8033020)
  ;;     - d.ext (file, size=5626152)
  ;;     - k (file, size=7214296)


  (check-equal?
   (make-filesystem sample)
   '(("/b.txt" . 14848514)
     ("/c.dat" . 8504156)
     ("/a/f" . 29116)
     ("/a/g" . 2557)
     ("/a/h.lst" . 62596)
     ("/a/e/i" . 584)
     ("/d/j" . 4060174)
     ("/d/d.log" . 8033020)
     ("/d/d.ext" . 5626152)
     ("/d/k" . 7214296)))

  (check-equal?
   (is-subdir? "/foo/bar" "/foo") #t)

  (check-equal?
   (is-subdir? "/foo" "/foo") #f)

  (check-equal?
   (is-subdir? "/foo/bar" "/") #t)

  (check-equal?
   (is-subdir? "/foo/bar/baz" "/foo/bar") #t)

  (check-equal?
   (get-parent-dirs "/foo/bar/baz") '("/foo/bar" "/foo" "/"))

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
                100))
