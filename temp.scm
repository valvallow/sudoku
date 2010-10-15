(use srfi-9) ; define-record-type
(use gauche.sequence) ; map-with-index
(use util.list) ; slices
(use liv.onlisp.symbol) ; symb

;; http://gigazine.net/index.php?/news/comments/20100822_hardest_sudoku/kj
(define most-difficult-data
  '((0 0 5 3 0 0 0 0 0)
    (8 0 0 0 0 0 0 2 0)
    (0 7 0 0 1 0 5 0 0)
    (4 0 0 0 0 5 3 0 0)
    (0 1 0 0 7 0 0 0 6)
    (0 0 3 2 0 0 0 8 0)
    (0 6 0 5 0 0 0 0 9)
    (0 0 4 0 0 0 0 3 0)
    (0 0 0 0 0 9 7 0 0)))

(define most-difficult-data-ans
  '((1 4 5 3 2 7 6 9 8)
    (8 3 9 6 5 4 1 2 7)
    (6 7 2 9 1 8 5 4 3)
    (4 9 6 1 8 5 3 7 2)
    (2 1 8 4 7 3 9 5 6)
    (7 5 3 2 9 6 4 8 1)
    (3 6 7 5 4 2 8 1 9)
    (9 8 4 7 6 1 2 3 5)
    (5 2 1 8 3 9 7 6 4)))

(load "./data.scm")

(define (number->list number)
  (map x->number
       (map x->string
            (string->list (number->string number)))))

(define (list-padding ls pad count)
  (let1 len (length ls)
    (if (= len count)
        ls
        (let rec ((ls ls)(cnt (- count len)))
          (if (zero? cnt)
              ls
              (rec (cons pad ls)(- cnt 1)))))))

(define (number->matrix number count)
  (slices (list-padding (number->list number) 0 81) count))

(define-record-type sudoku
  (make-sudoku name level problem answer) sudoku?
  (name sudoku-name)
  (level sudoku-level)
  (problem sudoku-problem)
  (answer sudoku-answer))

(define (data->sudoku ls)
  (map-with-index
   (lambda (level row)
     (map-with-index
      (lambda (index e)
        (make-sudoku (symb level '- index)
                     level
                     (number->matrix (caar e) 9)
                     (number->matrix (caadr e) 9))) row)) ls))

(define sudoku-data-list (data->sudoku data-list))

(define (solve-problems solver problems)
  (map (lambda (s)
         `((name . ,(sudoku-name s))
           (level . ,(sudoku-level s))
           (solve? . ,(equal? (sudoku-answer s)
                              (solver (sudoku-problem s)))))) problems))


;;
;; fix-naked-singles-solver
;;


(print-matrix
 (solve-problems fix-naked-singles-solver (apply append sudoku-data-list)))
;; ((name . 0-0) (level . 0) (solve? . #t))
;; ((name . 0-1) (level . 0) (solve? . #t))
;; ((name . 0-2) (level . 0) (solve? . #t))
;; ((name . 0-3) (level . 0) (solve? . #t))
;; ((name . 0-4) (level . 0) (solve? . #t))
;; ((name . 0-5) (level . 0) (solve? . #t))
;; ((name . 0-6) (level . 0) (solve? . #t))
;; ((name . 0-7) (level . 0) (solve? . #t))
;; ((name . 0-8) (level . 0) (solve? . #t))
;; ((name . 0-9) (level . 0) (solve? . #t))
;; ((name . 0-10) (level . 0) (solve? . #t))
;; ((name . 0-11) (level . 0) (solve? . #t))
;; ((name . 0-12) (level . 0) (solve? . #t))
;; ((name . 0-13) (level . 0) (solve? . #t))
;; ((name . 0-14) (level . 0) (solve? . #t))
;; ((name . 0-15) (level . 0) (solve? . #t))
;; ((name . 1-0) (level . 1) (solve? . #f))
;; ((name . 1-1) (level . 1) (solve? . #f))
;; ((name . 1-2) (level . 1) (solve? . #f))
;; ((name . 1-3) (level . 1) (solve? . #f))
;; ((name . 1-4) (level . 1) (solve? . #f))
;; ((name . 1-5) (level . 1) (solve? . #f))
;; ((name . 1-6) (level . 1) (solve? . #f))
;; ((name . 1-7) (level . 1) (solve? . #f))
;; ((name . 1-8) (level . 1) (solve? . #f))
;; ((name . 1-9) (level . 1) (solve? . #f))
;; ((name . 1-10) (level . 1) (solve? . #f))
;; ((name . 1-11) (level . 1) (solve? . #f))
;; ((name . 1-12) (level . 1) (solve? . #f))
;; ((name . 1-13) (level . 1) (solve? . #f))
;; ((name . 1-14) (level . 1) (solve? . #f))
;; ((name . 1-15) (level . 1) (solve? . #f))
;; ((name . 2-0) (level . 2) (solve? . #f))
;; ((name . 2-1) (level . 2) (solve? . #f))
;; ((name . 2-2) (level . 2) (solve? . #f))
;; ((name . 2-3) (level . 2) (solve? . #f))
;; ((name . 2-4) (level . 2) (solve? . #f))
;; ((name . 2-5) (level . 2) (solve? . #f))
;; ((name . 2-6) (level . 2) (solve? . #f))
;; ((name . 2-7) (level . 2) (solve? . #f))
;; ((name . 2-8) (level . 2) (solve? . #f))
;; ((name . 2-9) (level . 2) (solve? . #f))
;; ((name . 2-10) (level . 2) (solve? . #f))
;; ((name . 2-11) (level . 2) (solve? . #f))
;; ((name . 2-12) (level . 2) (solve? . #f))
;; ((name . 2-13) (level . 2) (solve? . #f))
;; ((name . 2-14) (level . 2) (solve? . #f))
;; ((name . 2-15) (level . 2) (solve? . #f))
;; ((name . 3-0) (level . 3) (solve? . #f))
;; ((name . 3-1) (level . 3) (solve? . #f))
;; ((name . 3-2) (level . 3) (solve? . #f))
;; ((name . 3-3) (level . 3) (solve? . #f))
;; ((name . 3-4) (level . 3) (solve? . #f))
;; ((name . 3-5) (level . 3) (solve? . #f))
;; ((name . 3-6) (level . 3) (solve? . #f))
;; ((name . 3-7) (level . 3) (solve? . #f))
;; ((name . 3-8) (level . 3) (solve? . #f))
;; ((name . 3-9) (level . 3) (solve? . #f))
;; ((name . 3-10) (level . 3) (solve? . #f))
;; ((name . 3-11) (level . 3) (solve? . #f))
;; ((name . 4-0) (level . 4) (solve? . #f))
;; ((name . 4-1) (level . 4) (solve? . #f))
;; ((name . 4-2) (level . 4) (solve? . #f))
;; ((name . 4-3) (level . 4) (solve? . #f))
;; ((name . 4-4) (level . 4) (solve? . #f))
;; ((name . 4-5) (level . 4) (solve? . #f))
;; ((name . 4-6) (level . 4) (solve? . #f))
;; ((name . 4-7) (level . 4) (solve? . #f))
;; ((name . 4-8) (level . 4) (solve? . #f))
;; ((name . 4-9) (level . 4) (solve? . #f))
;; ((name . 4-10) (level . 4) (solve? . #f))
;; ((name . 4-11) (level . 4) (solve? . #f))
;; ((name . 4-12) (level . 4) (solve? . #f))
;; ((name . 4-13) (level . 4) (solve? . #f))
;; ((name . 4-14) (level . 4) (solve? . #f))
;; ((name . 4-15) (level . 4) (solve? . #f))






;;
;; backtrack-solver
;;

;; most difficult data
(time (equal? (backtrack-solver most-difficult-data)
              most-difficult-data-ans))
; real  15.438
; user  15.375
; sys    0.047
;; #t

;; level 0
(print-matrix (solve-problems backtrack-solver (car sudoku-data-list)))
;; ((name . 0-0) (level . 0) (solve? . #t))
;; ((name . 0-1) (level . 0) (solve? . #t))
;; ((name . 0-2) (level . 0) (solve? . #t))
;; ((name . 0-3) (level . 0) (solve? . #t))
;; ((name . 0-4) (level . 0) (solve? . #t))
;; ((name . 0-5) (level . 0) (solve? . #t))
;; ((name . 0-6) (level . 0) (solve? . #t))
;; ((name . 0-7) (level . 0) (solve? . #t))
;; ((name . 0-8) (level . 0) (solve? . #t))
;; ((name . 0-9) (level . 0) (solve? . #t))
;; ((name . 0-10) (level . 0) (solve? . #t))
;; ((name . 0-11) (level . 0) (solve? . #t))
;; ((name . 0-12) (level . 0) (solve? . #t))
;; ((name . 0-13) (level . 0) (solve? . #t))
;; ((name . 0-14) (level . 0) (solve? . #t))
;; ((name . 0-15) (level . 0) (solve? . #t))


;; level 1 - 3
(time (let1 s (list-ref (cadr sudoku-data-list) 3)
        (equal? (backtrack-solver (sudoku-problem s))
                (sudoku-answer s))))
; real   5.969
; user   5.891
; sys    0.078
;; #t


;; level 2 - 1
(time (let1 s (list-ref (caddr sudoku-data-list) 1)
        (equal? (backtrack-solver (sudoku-problem s))
                (sudoku-answer s))))
; real  17.344
; user  17.203
; sys    0.093
;; #t


;; level 2 - 5
(time (let1 s (list-ref (caddr sudoku-data-list) 5)
        (equal? (backtrack-solver (sudoku-problem s))
                (sudoku-answer s))))
; real  54.922
; user  54.578
; sys    0.235
;; #t


;; level 3 - 1
(time (let1 s (list-ref (cadddr sudoku-data-list) 1)
        (equal? (backtrack-solver (sudoku-problem s))
                (sudoku-answer s))))
; real   2.438
; user   2.422
; sys    0.000
;; #t


;; level 3 - 11
(time (let1 s (list-ref (cadddr sudoku-data-list) 11)
        (equal? (backtrack-solver (sudoku-problem s))
                (sudoku-answer s))))
; real  22.203
; user  22.062
; sys    0.125
;; #t


;; level 4 - 0
(time (let1 s (list-ref (cadddr (cdr sudoku-data-list)) 0)
        (equal? (backtrack-solver (sudoku-problem s))
                (sudoku-answer s))))
; real  15.766
; user  15.640
; sys    0.125
;; #t


;; level 4 - 15
(time (let1 s (list-ref (cadddr (cdr sudoku-data-list)) 15)
        (equal? (backtrack-solver (sudoku-problem s))
                (sudoku-answer s))))
; real  39.063
; user  38.906
; sys    0.125
;;  #t






