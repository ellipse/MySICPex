(define tolerance 0.00001)
(define dx 0.000001)

(define (square x) (* x x))
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average x y)
  (/ (+ x y)
     2))
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))


(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x)
            ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;ex1.40
(define (cube x) (* x x x))
(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

;ex1 . 41
(define (double g)
  (lambda (x)
    (g (g x))))
(define (inc x) (+ x 1))

;ex1 . 42
(define (compose f g)
  (lambda (x)
    (f (g x))))

;ex1 . 43
(define (repeated f n)
  (cond ((<= n 0) (lambda (x) x))
        ((= n 1) f)
        ((even? n) (double (repeated f (/ n 2))))
        ((odd? n) (compose f (repeated f (- n 1))))
        ))
((repeated square 3) 5)

;ex1     . 44
(define (smooth f)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3.0)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

;ex1 . 45
(define (fast-expr b n)
  (define (fast-expr-iter b n p)
    (cond ((= 0 n) p)
          ((even? n) (fast-expr-iter (square b) (/ n 2) p))
          (else (fast-expr-iter b (- n 1) (* p b)))))
  (fast-expr-iter b n 1))

;when the power is less than 2^n, the nth-root needs at least
;n-1 times of average-damp
(define (nth-root x n)
  ;aver-times define the number of calling average-damp
  (define (aver-times)
    (define (aver-times-iter k pow)
      (if (< n pow)
          (- k 1)
          (aver-times-iter (+ k 1) (* pow 2))))
    (aver-times-iter 2 4))
  (fixed-point-of-transform (lambda (y) (/ x (fast-expr y (- n 1))))
                            (repeated average-damp (aver-times))
                            1.0))


