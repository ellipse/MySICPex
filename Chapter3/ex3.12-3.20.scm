(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)
(define (last-pair x)
  (if (null? (cdr x))
      x
      (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x))

(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))

;; ex3.17
(define (count-pairs x)
  (let ((appeared '()))
    (if (or (not (pair? x))
            (memq x appeared))
        0
        (begin (cons x appeared)
               (+ (count-pairs (car x))
                  (count-pairs (cdr x))
                  1)))))
;; ex3.17 ends here


;; ex3.19 && ex3.18
(define cyclic?
  (lambda (lst)
    (do ((ptr1 lst)
         (ptr2 (cdr lst)))
        ((or (null? ptr2)
             (null? (cdr ptr2))
             (eq? ptr1 ptr2))
         (if (or (null? ptr2) (null? ptr1))
             false
             true))
      (begin (set! ptr2 (cddr ptr2))
             (set! ptr1 (cdr ptr1))))))
;; ex3.19 && ex3.18 ends here
