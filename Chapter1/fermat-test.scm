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

(remainder 3 4)
;Value: 3
(define (smallest-divisor n)
  (find-divisor n 2))
;Value: smallest-divisor
(define (square x) (* x x))
;Value: square


;Value: square

(define (divides? a b)
  (= (remainder b a) 0))
;Value: divides?


(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
;Value: find-divisor


;Value: find-divisor

(define (prime? n)
  (= n (smallest-divisor n)))
;Value: prime?

(smallest-divisor 19999)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m))
                                m))
        (else (remainder (* base (expmod base (- exp 1) m))
                         m))))
;Value: expmod

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))
;Value: fermat-test
(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))
;Value: fast-prime?
(fast-prime? 199 20)









