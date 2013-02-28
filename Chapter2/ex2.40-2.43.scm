(load "~/Programs/MySICPex/Chapter2/ex2.33-2.39.scm")
(load "~/Programs/MySICPex/Chapter1/ex1.21-1.28.scm")

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;; ex 2.40
(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))


;; ex 2.41

(define (sum-to-s-trilist n s)
  (define (sum-to-s? trible-pair)
    (= (+ (car trible-pair)
          (cadr trible-pair)
          (caddr trible-pair))
       s))
  (define upper-bound (if (< s n) s n))
  (filter
   sum-to-s?
   (flatmap
    (lambda (i)
      (map (lambda (p) (cons i p))
           (unique-pairs (- i 1))))
    (enumerate-interval 3 n))))


;; ex 2.42
(define (safe? k position)
  (define kth (car position))
  (define (safe-iter i pos)
    (if (and (not (null? pos))
             (not (= kth (car pos)))  ;; not in the same line
             (not (= (abs (- (car pos) kth))
                     (- i 1))))       ;; not in same diagonal
        (safe-iter (+ i 1) (cdr pos))
        (if (null? pos)
            true                      ;; pos is null means test completed
            false)))
  (safe-iter 2 (cdr position)))

(define empty-board '())

(define (adjoint-position kth k rest)
  (cons kth rest))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (position) (safe? k position))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoint-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))


;; my version of queens ( filtering before appending )
;; reduce space complexity to O(n)
(define (queens-myversion board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (flatmap
         (lambda (rest-of-queens)
           (filter
            (lambda (position) (safe? k position))
            (map (lambda (new-row)
                   (adjoint-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size))))
         (queen-cols (- k 1)))))
  (queen-cols board-size))

;; ex 2.43
(define (queens-time queens-func k)
  (define start-time (real-time-clock))
  (queens-func k)
  (- (real-time-clock) start-time))

(define (queens-slow board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (position) (safe? k position))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoint-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))
