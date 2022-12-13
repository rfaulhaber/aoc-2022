#lang errortrace racket

(require racket/string)
(require racket/list)

(provide make-filesystem
         get-parent-dir
         is-subdir?
         get-parent-dirs)

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

    file-pairs))

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
   (get-parent-dirs "/foo/bar/baz") '("/foo/bar" "/foo" "/")))
