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

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (+ (term a) result))))
  (iter a 0))
;Value: sum

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a)
              (* result (term a)))))
  (iter a 1))
;Value: product

(define (factorial n)
  (define (term x) x)
  (define (next x) (+ x 1))
  (product term 1 next n))
;Value: factorial

(define (filtered-accumulate filter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (if (filter a)
            (iter (next a)
                  (combiner (term a) result))
            (iter (next a) result))))
        (iter a null-value))
;Value: filtered-accumulate

