(define tolerance 0.00001)
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
               1 . 0))

(define dx 0 . 000001)
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
  (cond ((= n 1) f)
        ((even? n) (double (repeated f (/ n 2))))
        ((odd? n) (compose f (repeated f (- n 1))))
        ))
((repeated square 3) 5)

;ex1 . 44
(define)
