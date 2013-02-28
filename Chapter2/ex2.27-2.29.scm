(define (reverse l)
  (define (reverse-iter rest result)
    (if (null? rest)
        result
        (reverse-iter (cdr rest) (cons (car rest) result))))
  (reverse-iter l '()))

;; (define (append l1 l2)
;;   (if (null? l1)
;;       l2
;;       (cons (car l1)
;;             (append (cdr l1) l2))))


;; ex 2.27

(define (deep-reverse-myversion l)
  (define (reverse-helper rest result)
    (cond ((null? rest) result)
          ((pair? (car rest)) (reverse-helper (cdr rest)
                                              (cons (reverse-helper (car rest) '())
                                                    result)))
          (else (reverse-helper (cdr rest)
                                (cons (car rest) result)))))
  (reverse-helper l '()))

(define (deep-reverse l)
  (define (helper-iter rest result)
    (if (null? rest)
        result
        (helper-iter (cdr rest)
              (cons (if (pair? (car rest))
                        (deep-reverse (car rest))
                        (car rest))
                    result))))
  (helper-iter l '()))


;; ex 2.28

(define (fringe-myversion l)
  (cond ((null? l) '())
        ((not (pair? l)) (list l))
        ((pair? (car l)) (append (fringe-myversion (car l))
                                 (fringe-myversion (cdr l))))
        (else (append (list (car l))
                      (fringe-myversion (cdr l))))))

(define (fringe l)
  (cond ((null? l) '())
        ((not (pair? l)) (list l))
        (else (append (fringe (car l))
                      (fringe (cdr l))))))

;; ex 2.29
(define (make-mobile left right)
  (list left right))
(define (make-branch length structure)
  (list length structure))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

(define (mobile? l)
  (and (pair? l)
       (pair? (left-branch l))
       (pair? (right-branch l))))
(define (branch? l)
  (and (pair? l)
       (not (pair? (car l)))))
(define (total-weight m)
  (cond ((mobile? m) (+ (total-weight (left-branch m))
                        (total-weight (right-branch m))))
        ((branch? m) (if (mobile? (branch-structure m))
                         (total-weight (branch-structure m))
                         (branch-structure m)))
        (else m)))


(define (moment b)
  (if (branch? b)
      (* (branch-length b)
         (total-weight (branch-structure b)))
      '()))
(define (balance? m)
  (cond ((mobile? m) (and (balance? (left-branch m))
                          (balance? (right-branch m))
                          (= (moment (left-branch m))
                             (moment (right-branch m)))))
        ((branch? m) (if (mobile? (branch-structure m))
                         (balance? (branch-structure m))
                         #t))
        (else #f)))
