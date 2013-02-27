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
(define (unique-trilists n)
  ;; generate all possible trible lists (i j k)
  ;; satisfying 1<=i<j<k<min{s,n+1}
  (flatmap
   ())
(define (sum-to-s-trilist n s)
  (define (sum-to-s? trible-pair)
    (= (+ (car trible-pair)
          (cadr trible-pair)
          (caddr trible-pair))
       s))
  )
