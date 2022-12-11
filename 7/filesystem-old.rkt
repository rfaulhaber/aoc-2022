#lang racket/base

(require racket/string)
(require racket/function)
(require racket/list)

;; a mock filesystem for AOC day 7

(provide filesystem
         make-filesystem)


(define (get-parent-dir path)
  (string-join (drop-right (string-split path "/") 1) "/" #:before-begin "/"))

(struct filesystem (current-dir files))

(define (make-filesystem input)
  (let ((cmds (map string-trim (string-split input "$" #:trim? #t))))
    (foldl eval-cmd (filesystem null '()) cmds)))

(define (eval-cmd cmd filesystem)
  (cond
    ;; [(string-prefix? cmd "ls") (eval-ls (drop (string-split cmd "\n") 1) filesystem)]
    [(string-prefix? cmd "cd") (eval-cd (substring cmd 3) filesystem)]))

;; (define (eval-ls cmd filesystem)
;;   (let ((dir-contents (string-split cmd "\n")))
;;     )
;;   )

(define (eval-cd cmd filesystem)
  (if (string=? cmd "..")
      (get-parent-dir filesystem)
      (get-next-dir cmd filesystem)))

(define (get-parent-dir fs)
  (let ((path (string-split (filesystem-current-dir fs) "/")))
    (if (<= (length path) 1)
        (struct-copy filesystem fs (current-dir "/"))
        (struct-copy filesystem fs (current-dir (string-join (drop-right (string-split (filesystem-current-dir fs) "/") 1) "/" #:before-first "/"))))))

(define (get-next-dir dir fs)
  (let ((path (string-split (filesystem-current-dir fs) "/")))
    (struct-copy filesystem fs (current-dir (string-join (append path (list dir)) "/" #:before-first "/")))))

(define (get-dir filesystem)
  (define (get-dir-from-path path contents)
    (displayln (format "path ~v contents ~v" path contents))
    (if (= (length path) 1)
        (cdr (assoc (car path) contents))
        (get-dir-from-path (cdr path) (cdr (assoc (car path) contents)))))
  (get-dir-from-path (string-split (filesystem-current-dir filesystem) "/") (filesystem-files filesystem)))

(module+ test
  (require rackunit)

  (check-equal?
   (filesystem-current-dir (get-parent-dir (filesystem "/" '())))
   (filesystem-current-dir (filesystem "/" '())))

  (check-equal?
   (filesystem-current-dir (get-parent-dir (filesystem "/foo" '())))
   (filesystem-current-dir (filesystem "/" '())))

  (check-equal?
   (filesystem-current-dir (get-parent-dir (filesystem "/foo/bar" '())))
   (filesystem-current-dir (filesystem "/foo" '())))

  (check-equal?
   (filesystem-current-dir (get-parent-dir (filesystem "/foo/bar/baz" '())))
   (filesystem-current-dir (filesystem "/foo/bar" '())))

  (check-equal?
   (filesystem-current-dir (get-next-dir "foo" (filesystem "/" '())))
   "/foo")

  (check-equal?
   (filesystem-current-dir (get-next-dir "bar" (filesystem "/foo" '())))
   "/foo/bar")

  (check-equal?
   (get-dir (filesystem "/foo/bar" '(("foo" . (("bar" . (("hello" . 123))))))))
   '(("hello" . 123)))

  (check-equal?
   (get-parent-dir )
   )

  )
