;; sudoku - Wikipedia - http://ja.wikipedia.org/wiki/%E6%95%B0%E7%8B%AC

(define-module liv.game.sudoku
  (export fix-naked-singles-solver backtrack-solver))
(select-module liv.game.sudoku)

(use srfi-1) ; drop, split-at
(use srfi-9) ; define-record-type
(use util.list) ; slices
(use liv.matrix) ; matrix-*, *-matrix

(define-constant region-size 3)
(define-constant fullset-numbers (iota (* region-size region-size) 1))

(define (single? ls)
  (and (pair? ls)
       (null? (cdr ls))))

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
                           (empty? zero?)
                           (ignore '()))
    (let1 f (pa$ filter (complement empty?))
      (let* ((row-candidates (f (matrix-row-ref matrix y)))
             (col-candidates (f (matrix-col-ref matrix x)))
             (region-candidates (f (region-ref (matrix->regions matrix region-size)
                                               (region-index x y)))))
        (lset-difference eq? fullset-numbers row-candidates
                         col-candidates region-candidates
                         ignore)))))

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
                                  :optional (more identity))
  (let rec ((m matrix))
    (receive (cand exist?)(fix-naked-singles m)
      (cond (exist? (rec cand))
            ((has-empty? cand)(more cand))
            (else cand)))))

;;
;; backtrack solver
;;

(define-record-type point
  (make-point x y) point?
  (x point-x)
  (y point-y))

(define (backtrack-solver matrix)
  (let/cc hop
    (let ((mstack '())(cstack '())(pstack '())(count 0))
      (let backtrack ((m (matrix-copy matrix))(bt-cand '())
                      (bt-point (make-point -1 -1)))
        (for-each-matrix-with-index
         (lambda (e x y)
           (when (zero? e)
             (let1 cand (if (and (= x (point-x bt-point))
                                 (= y (point-y bt-point)))
                            bt-cand
                            (candidates m x y))
               (cond ((null? cand)
                      (inc! count)
                      (backtrack (pop! mstack)
                                 (pop! cstack)
                                 (pop! pstack)))
                     ((single? cand)
                      (matrix-set! m x y (car cand)))
                     (else
                      (push! mstack (matrix-copy m))
                      (push! cstack (cdr cand))
                      (push! pstack (make-point x y))
                      (matrix-set! m x y (car cand))))))) m)
        (hop m count)))))

;; (equal? test-data-1-ans (backtrack-solver test-data-1))
;; ;; #t
;; (equal? test-data-2-ans (backtrack-solver test-data-2))
;; ;; #t
;; (equal? test-data-3-ans (backtrack-solver test-data-3))
;; ;; #t
;; (equal? test-data-4-ans (backtrack-solver test-data-4))
;; ;; #t
;; (equal? most-difficult-data-ans (backtrack-solver most-difficult-data))
;; ;; #t


(provide "liv/game/sudoku")

