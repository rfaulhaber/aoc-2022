#lang racket/base

(require racket/port)
(require racket/string)
(require racket/list)

(provide (rename-out (day5-read-syntax read-syntax)
                     (day5-read read)))

(define (day5-read in)
  (syntax->datum (day5-read-syntax #f in)))

(define (day5-read-syntax src in)
  (define s-exprs (read-code-from in))
  (define module-datum `(module day5 day5/expander
                          ,@s-exprs))
  (datum->syntax #f module-datum))

(define (read-code-from port)
  (let* ((input (port->string port))
         (input-split (string-split input "\n\n"))
         (top (car input-split))
         (bottom (cadr input-split)))

    (display (append (list (parse-stack-code top)) (parse-commands bottom)))
    (append (list (parse-stack-code top)) (parse-commands bottom))))

(define (parse-stack-code stack-string)
  (let* ([lines (string-split stack-string "\n")]
         [pos-line (last lines)]
         [stack-lines (take lines (- (length lines) 1))]
         [pos-list (regexp-match-positions* #px"[[:digit:]]" pos-line)]
         [stacks (for/list ((line stack-lines))
                   (map (lambda (pos)
                          (substring line (car pos) (cdr pos))) pos-list))]
         ;; this is a mess! sorry!
         [out (map (lambda (els)
                     (filter (lambda (el)
                               (not (string=? el " "))) els))
                   (apply map list stacks))])

    (list* 'day5-state (list out))))

(define (parse-commands commands)
  (let* ([lines (string-split commands "\n")]
         [command-lines (map (lambda (line)
           (map string->number (regexp-match* #px"[[:digit:]]" line))) lines)])

    (map (lambda (line)
           `(day5-move ,@line)) command-lines)))

(module+ test
  (require rackunit)

  (check-equal?
   (parse-stack-code "[D]        \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 ")
   '(("D" "N" "Z") ("C" "M") ("P")))

  (check-equal?
   (parse-commands "move 1 from 2 to 3\nmove 2 from 3 to 4")
   '((day5-move 1 2 3) (day5-move 2 3 4))))
