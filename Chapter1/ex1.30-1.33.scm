(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (define (term x) x)
  (define (next x) (+ 1 x))
  (product term 1 next n))

(define (wallis-pi n)
  (define (square x) (* x x))
  (define (numerator-term x)
    (square (* 2 x)))
  (define (denominator-term x)
    (square (- (* 2 x)
               1)))
  (define (next x) (+ x 1))

  (exact->inexact
   (* 4 (/ (/ (product numerator-term 2 next n)
              (product denominator-term 2 next n))
           n))))

(wallis-pi 2000)
;Value: 3 . 1419853772121162

(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (if (filter a)
            (iter (next a)
                  (combiner (term a)
                            result))
            (iter (next a) result))))
    (iter a null-value))

;using filtered-accumulate to implement sum
(filtered-accumulate (lambda (x) #t)
                     +
                     0
                     (lambda (x) x)
                     0
                     (lambda (x) (+ 1 x))
                     100)
;Value: 5050

