#lang errortrace racket/base

(require racket/string)
(require racket/file)
(require racket/list)

(require "../utils/racket/main.rkt");

(define (get-index-list lst)
  (define (get-index-list-inner ilst index)
    (if (= index (length (car lst))) '()
        (cons (map (lambda (l)
                       (list-ref l index)) ilst) (get-index-list-inner ilst (+ index 1)))))
  (get-index-list-inner lst 0))

(define (load-file-as-string filename)
  (file->string filename))

(define (parse-file-pos-key pos-key)
  (regexp-match-positions* #px"[[:digit:]]+" pos-key))

(define (parse-initial-state file-head)
  (let* ((file-head-lines (string-split file-head "\n"))
         (pos-key (parse-file-pos-key (last file-head-lines)))
         (stack-lines (take file-head-lines (- (length file-head-lines) 1)))
         (stack-list (for/list ([stack-line stack-lines])
                       (map (lambda (pos)
                              (substring stack-line (car pos) (cdr pos))) pos-key))))

    (map (lambda (els)
           (filter (lambda (el)
                     (not (string=? el " "))) els)) (get-index-list stack-list))))

(define (parse-move move)
  (map string->number (regexp-match* #px"[[:digit:]]+" move)))

(define (parse-input filename)
  (let* ((contents (load-file-as-string filename))
        (file-parts (string-split contents "\n\n"))
        (initial-state (parse-initial-state (car file-parts)))
        (moves (map parse-move (string-split (cadr file-parts) "\n"))))

    `((initial-state . ,initial-state)
      (moves . ,moves))))

(define (apply-move-to-state state move reverse?)
  (let-values ([(amount from to) (apply values move)])
    (let* ((from-normalized (- from 1))
           (to-normalized (- to 1))
           (taken (take (list-ref state from-normalized) amount))
           (from-list (drop (list-ref state from-normalized) amount))
           (to-list (append (if reverse? (reverse taken) taken) (list-ref state to-normalized)))
           (state-inter (list-set state from-normalized from-list)))
      (list-set state-inter to-normalized to-list))))

(define (eval-input file-info reverse?)
  (define state (cdr (assoc 'initial-state file-info)))

  (for ([move (cdr (assoc 'moves file-info))])
    (set! state (apply-move-to-state state move reverse?)))

  state)

(module+ main
  (require racket/cmdline)
  (define part (make-parameter 1))

  (define (solve part filename)
    (let* ((input (parse-input filename)))
      (string-join (map car (eval-input input (= (string->number part) 1))) "")))

  (command-line
    #:program "AOC Day 5b"
    #:once-each
    (("-p" "--part") pt
                    "Specify which part to solve for."
                    (part pt))
    #:args (filename)
    (solve (part) filename)))

(module+ test
  (require rackunit)

  (check-equal?
   (parse-file-pos-key " 1   2   3 ") '((1 . 2) (5 . 6) (9 . 10)))

  (check-equal?
   (parse-initial-state "    [D]    \n[N] [C]    \n[Z] [M] [P]\n 1   2   3 ")
   '(("N" "Z") ("D" "C" "M") ("P")))

  (check-equal?
   (get-index-list '((1 2 3) (3 4 5) (5 6 7))) '((1 3 5) (2 4 6) (3 5 7)))

  (check-equal?
   (get-index-list '((1 2 3) (3 4 5) (5 6 7) (8 9 10) (9 10 11)))
   '((1 3 5 8 9) (2 4 6 9 10) (3 5 7 10 11)))

  (check-equal?
   (apply-move-to-state '(("N" "Z") ("D" "C" "M") ("P")) '(1 2 1))
   '(("D" "N" "Z") ("C" "M") ("P")))

  (check-equal?
   (apply-move-to-state '(("D" "N" "Z") ("C" "M") ("P")) '(3 1 3))
   '(() ("C" "M") ("Z" "N" "D" "P"))))
