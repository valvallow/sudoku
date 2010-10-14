;; sudoku - Wikipedia - http://ja.wikipedia.org/wiki/%E6%95%B0%E7%8B%AC

(define test-data
  '((7 8 0 5 2 0 0 0 6)
    (9 0 0 0 0 3 0 7 5)
    (0 0 5 0 0 6 0 4 0)
    (0 0 4 0 6 1 3 0 2)
    (1 0 0 9 0 0 0 0 7)
    (8 0 2 0 7 0 6 0 0)
    (0 5 0 2 0 0 8 0 0)
    (6 3 0 4 0 0 0 0 1)
    (2 0 0 0 3 7 0 5 9)))

(define test-data-ans
  '((7 8 1 5 2 4 9 3 6)
    (9 4 6 1 8 3 2 7 5)
    (3 2 5 7 9 6 1 4 8)
    (5 7 4 8 6 1 3 9 2)
    (1 6 3 9 4 2 5 8 7)
    (8 9 2 3 7 5 6 1 4)
    (4 5 7 2 1 9 8 6 3)
    (6 3 9 4 5 8 7 2 1)
    (2 1 8 6 3 7 4 5 9)))

(use srfi-1) ; drop, split-at
(use util.list) ; slices
(use liv.matrix) ; matrix-*, *-matrix
(use liv.list) ; 

(define-constant region-size 3)
(define-constant fullset-numbers (iota (* region-size region-size) 1))

;; matrix x or y -> region position
(define (region-pos mn)
  (quotient mn region-size))

;; mmatrix x and y -> region index
(define (region-index mx my)
  (+ (region-pos mx)
     (* region-size
        (region-pos my))))

(define (region-ref region n)
  (list-ref region n))

(define (slice-matrix-rows matrix size)
  (map (lambda (row)
         (slices row size)) matrix))

(define (matrix->regions matrix size)
  (let1 sliced (slice-matrix-rows test-data size)
    (let rec ((m sliced)(acc '()))
      (if (null? m)
          (apply append (reverse acc))
          (receive (took dropped)(split-at m size)
            (rec dropped (cons (apply map append took) acc)))))))

(define (

(define (candidates matrix x y)
  (let1 f (pa$ filter (complement zero?))
    (let* ((row-candidates (f (matrix-row-ref matrix y)))
           (col-candidates (f (matrix-col-ref matrix x)))
           (region-candidates (f (region-ref (matrix->regions matrix region-size)
                                             (region-index x y)))))
      (lset-difference = fullset-numbers row-candidates
                       col-candidates region-candidates))))



;; rotate-matrix
