#lang racket

(define (load-input path)
  (file->lines path))

(define input (load-input "./input.txt"))

(define (get-components line)
  (let* ((component-size (/ (string-length line) 2)))
    (list (substring line 0 component-size) (substring line component-size))))

(define (get-common-components components)
  (let ((left (list->set (map string (string->list (car components)))))
        (right (list->set (map string (string->list (cadr components))))))
    (set->list (set-intersect left right))))

(define (get-letter-score letter)
  (when (> (string-length letter) 1)
    (error "Received invalid letter"))
  (let ((char (car (string->list letter))))
    (if (char-upper-case? char)
        (- (char->integer char) 38)
        (- (char->integer char) 96))))

;; thank you stackoverflow: https://stackoverflow.com/questions/8725832/how-to-split-list-into-evenly-sized-chunks-in-racket-scheme
;; I've written this before but I want to save time
(define (split-by lst n)
   (if (not (empty? lst))
       (cons (take lst n) (split-by (drop lst n) n))
       '() ))

(define (char-list->string lst)
  (map string lst))

(define (get-common-components-in-list lst)
  (set->list (apply set-intersect (map char-list->string (map string->list lst)))))

(define (solve-part-1 input)
  (let* ((components (map get-components input))
         (letter-scores (map (lambda (el) (get-letter-score (car el))) (map get-common-components components))))
    (apply + letter-scores)))

(define (solve-part-2 input)
  (let* ((groups (split-by input 3))
         (common-components (map get-common-components-in-list groups)))
    (apply + (map get-letter-score (map car common-components)))))

(printf "part 1 ~a" (solve-part-1 input));
(printf "part 2 ~a" (solve-part-2 input));

(module+ test
  (require rackunit)

  (define sample (load-input "./sample.txt"))

  (check-equal? (get-components "vJrwpWtwJgWrhcsFMMfFFhFp") '("vJrwpWtwJgWr" "hcsFMMfFFhFp"))
  (check-equal? (get-common-components '("vJrwpWtwJgWr" "hcsFMMfFFhFp")) '("p"))
  (check-equal? (get-letter-score "a") 1)
  (check-equal? (map get-letter-score '("p" "L" "P" "v" "t" "s")) '(16 38 42 22 20 19))

  (check-equal? (solve-part-1 sample) 157)

  (check-equal? (split-by '("foo" "bar" "baz" "quux") 2) '(("foo" "bar") ("baz" "quux")))
  (check-equal? (char-list->string '(#\f #\o)) '("f" "o"))
  (check-equal? (get-common-components-in-list '("vJrwpWtwJgWrhcsFMMfFFhFp" "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL" "PmmdzqPrVvPwwTWBwg")) '("r"))

  (check-equal? (solve-part-2 sample) 70))
