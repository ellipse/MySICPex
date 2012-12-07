(define tolerance 0.00001)

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
(define (sqrt x)
  (fixed-point (lambda (y) (average (/ x y)
                                    y))
               1.0))

;ex1.35
(fixed-point (lambda (x) (average (+ 1
                                     (/ 1 x))
                                  x))
             1.6)
;ex1.36

(define (fixed-point-display f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(fixed-point-display (lambda (x) (average (/ (log 1000)
                                             (log x))
                                          x))
                     2.0)
(fixed-point-display (lambda (x)  (/ (log 1000)
                                     (log x)))
                     2.0)

;ex1.37

(define (cont-frac-recur n d k)
  (define (frac-obverse counter)
    (if (>= counter k)
        (/ (n k)
           (d k))
        (/ (n counter)
           (+ (d counter)
              (frac-obverse (+ counter 1))))))
  (frac-obverse 1.0))

(cont-frac-recur (lambda (i) 1 . 0)
                 (lambda (i) 1 . 0)
                 10)
(define (cont-frac-iter n d k)
  (define (frac-reverse result counter)
    (if (<= counter 0)
        result
        (frac-reverse (/ (n counter)
                         (+ (d counter)
                            result))
                      (- counter 1))))
  (frac-reverse (/ (n k)
                   (d k))
                (- k 1)))

(cont-frac-iter (lambda (i) 1.0)
                (lambda (i) 1.0)
                30)


;ex1.38
(define (Euler-d i)
  (cond ((= 0 (remainder (+ i 1) 3))
           (/ (* 2
                 (+ i 1))
              3))
          (else 1)))
(cont-frac-iter (lambda (i) 1.0)
                Euler-d
                30)

;ex1 . 39
(define (square x) (* x x))
(define (tan-cf x k)
  (define (lambert-n k)
    (if (= k 1)
        x
        (- (square x))))
  (define (lambert-d k)
    (- (* 2 k)
       1))
  (cont-frac-iter lambert-n lambert-d 30))
(tan-cf 3.14159 10)
