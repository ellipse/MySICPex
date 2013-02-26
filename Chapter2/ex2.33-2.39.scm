(define (filter predicate sequence)
  (cond ((null? sequence) '())
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

;; ex2.33
(define (map-using-ac p sequence)
  (accumulate
   (lambda (x y) (cons (p x) y))
   '()
   sequence))
(define (append-using-ac seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length-using-ac sequence)
  (accumulate
   (lambda (x y) (+ y 1))
   0
   sequence))

;; ex2.34
(define (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff higher-terms)
     (+ this-coeff (* x higher-terms)))
   0
   coefficient-sequence))

;; ex2.35
(define (count-leaves-using-ac t)
  (accumulate ))

;; ex2.36
(define accumulate-n op init seqs
  (if (null? (car seqs))
      '()
      (cons (accumulate op init )
            (accumulate-n op init ))))

;; ex2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))
