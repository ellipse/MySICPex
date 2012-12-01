This is the Scheme process buffer.
Type C-x C-e to evaluate the expression before point.
Type C-c C-c to abort evaluation.
Type C-h m for more information.

MIT/GNU Scheme running under GNU/Linux

Copyright (C) 2011 Massachusetts Institute of Technology
This is free software; see the source for copying conditions. There is NO
warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

Image saved on Thursday October 27, 2011 at 7:44:21 PM
  Release 9.1 || Microcode 15.3 || Runtime 15.7 || SF 4.41 || LIAR/i386 4.118
  Edwin 3.116
(define (square x) (* x x))
;Value: square


(define (fast-expr b n)
  (define (fast-expr-iter b n p)
    (cond ((= 0 n) p)
          ((even? n) (fast-expr-iter (square b) (/ n 2) p))
          (else (fast-expr-iter b (- n 1) (* p b)))))
  (fast-expr-iter b n 1))

(define (double x) (* 2 x))
;Value: double
(define (halve x) (/ x 2))
;Value: halve
(define (fast-multiply a b)
  (define (fast-multiply-iter a b p)
    (cond ((= b 0) p)
          ((even? b) (fast-multiply-iter (double a) (halve b) p))
          (else (fast-multiply-iter a (- b 1) (+ p a)))))
  (fast-multiply-iter a b 0))
;Value: fast-multiply

(define (fast-fib n)
  (define (fast-fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count) (fast-fib-iter a
                                        b
                                        (+ (square q) (square q))
                                        (+ (* 2 p q) (square q))
                                        (/ count 2)))
          (else (fast-fib-iter (+ (* b q) (* a q) (* a p))
                               (+ (* b p) (* a q))
                               p
                               q
                               (- count 1)))))
  (fast-fib-iter 1 0 0 1 n))
; the funtion upward can't get the correct answer,and I cannot figure out why






















