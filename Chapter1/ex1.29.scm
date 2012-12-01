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
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
;Value: sum

(define (cube x) (* x x x))
;Value: cube


(define (simpson-integral f a b n)
  (define (fix-n n)
    (if (odd? n)
        (+ n 1)
        n))
  (define h (/ (- b a) (fix-n n)))
  (define (y k) (f (+ a (* k h))))
  (define (factor k)
    (cond ((or (= 0 k) (= n k)) 1)
          ((even? k) 2)
          (else 4)))
  (define (term k)
    (* (y k)
       (factor k)))
  (define (next k)
    (+ 1 k))
  (* (/ h 3)
     (sum term 0.0 next (fix-n n))))
(simpson-integral cube 0 1 1000)
;Value: .2500000000000003


;Value: .24999999999999992




