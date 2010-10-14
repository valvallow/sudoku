;; sudoku - Wikipedia - http://ja.wikipedia.org/wiki/%E6%95%B0%E7%8B%AC

(use srfi-1) ; drop, split-at
(use srfi-9) ; define-record-type
(use util.list) ; slices
(use liv.matrix) ; matrix-*, *-matrix
(use liv.onlisp.list) ; single?

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
  (let1 sliced (slice-matrix-rows matrix size)
    (let rec ((m sliced)(acc '()))
      (if (null? m)
          (apply append (reverse acc))
          (receive (took dropped)(split-at m size)
            (rec dropped (cons (apply map append took) acc)))))))

(define (candidates matrix x y . keywords)
  (let-keywords* keywords ((eq? =)
                           (empty? zero?))
    (let1 f (pa$ filter (complement empty?))
      (let* ((row-candidates (f (matrix-row-ref matrix y)))
             (col-candidates (f (matrix-col-ref matrix x)))
             (region-candidates (f (region-ref (matrix->regions matrix region-size)
                                               (region-index x y)))))
        (lset-difference eq? fullset-numbers row-candidates
                         col-candidates region-candidates)))))

(define (has-empty? matrix :optional (empty? zero?))
  (let/cc hop
    (fold (lambda (row acc)
            (if-let1 it (any empty? row)
                     (hop it)
                     acc)) #f matrix)))

;;
;; naked single solve
;;

(define (fix-naked-singles matrix . keywords)
  (let-keywords* keywords ((empty? zero?)
                           (car car))
    (let1 exist? #f
        (values (map-matrix-with-index
                 (lambda (e x y)
                   (if (empty? e)
                       (let1 can (candidates matrix x y)
                         (if (single? can)
                             (begin
                               (unless exist?
                                 (set! exist? (not exist?)))
                               (car can))
                             e))
                       e)) matrix)
                exist?))))

(define (fix-naked-singles-solver matrix
                                  :optional (more (lambda _ 'dead)))
  (let rec ((m matrix))
    (receive (cand exist?)(fix-naked-singles m)
      (cond (exist? (rec cand))
            ((has-empty? cand)(more cand))
            (else cand)))))

;;
;; backtrack solve
;;

(define-record-type cell
  (make-cell value fixed candidates) cell?
  (value cell-value set-cell-value!)
  (fixed cell-fixed set-cell-fixed!)
  (candidates cell-candidates set-cell-candidate!))

(define (copy-cell cell)
  (make-cell (cell-value cell)
             (cell-fixed cell)
             (cell-candidates cell)))

(define (matrix->backtrackable-cells matrix
                                    :optional (empty? zero?))
  (map-matrix-with-index
   (lambda (e x y)
     (if-let1 it (not (empty? e))
              (make-cell e it '())
              (make-cell e it (candidates matrix x y)))) matrix))

(define (copy-backtrackable-cells matrix)
  (map-matrix copy-cell matrix))

(define (print-backtrackable-cell matrix
                                  :optional (get cell-value))
  (newline)
  (print-matrix matrix :element-fun get))








