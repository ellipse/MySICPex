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

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
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
  (accumulate + 0 (map (lambda (x) 1) (enumerate-tree t))))
(define (count-leaves-using-ac-2 t)
  (accumulate
   (lambda (x rest)
     (if (pair? x)
         (+ (count-leaves-using-ac-2 x) rest)
         (+ rest 1)))
   0
   t))

;; ex2.36
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; ex2.37
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map
   (lambda (line) (dot-product line v))
       m))

(define (transpose mat)
  (accumulate-n
   cons
   '()
   mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (line) (matrix-*-vector cols line))
         m)))

;; ex2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))


;; ex2.39
