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
    (define (count-helper p)
      (if (or (not (pair? p))
              (memq p appeared))
          0
          (begin
            (set! appeared (cons p appeared))
            (+ (count-helper (car p))
               (count-helper (cdr p))
               1))))
    (count-helper x)))
;; ex3.17 ends here


;; ex3.19 && ex3.18
(define cyclic?
  (lambda (lst)
    (define (cyclic?-iter ptr1 ptr2)
      (cond ((or (null? ptr2)
                 ;; If ptr2 reaches the end of lst

                 (not (pair? ptr2)) 
                 ;; If ptr2 reaches a symbol rather than a pair

                 (null? (cdr ptr2))
                 ;; If ptr2 reaches the last pair of lst

                 (not (pair? (cdr ptr2)))
                 ;; If ptr2 reaches the last cons cell of lst
                 )
             false)
            ((eq? ptr1 ptr2)
             ;; If ptr2 catches up ptr1
             true)
            (else (cyclic?-iter (cdr ptr1) (cddr ptr2)))))
    (if (pair? lst)
        (cyclic?-iter lst (cdr lst))
        false)))
;; ex3.19 && ex3.18 ends here
