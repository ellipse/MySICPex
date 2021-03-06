(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))
(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))
(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

;; ex2.59
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1)
                    (union-set (cdr set1) set2)))))
;; ex2.59 ends here


;; ex2.60
;; set operation on sets with duplicative elements

;; element-of-set? and intersection-set don't need change
(define (adjoin-set-dup x set) (cons x set))
(define (union-set-dup set1 set2)
  (append set1 set2))

;; ex2.60 ends here

(define (sorted-element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (sorted-element-of-set? x (cdr set)))))
(define (intersection-set-sorted set1 set2)
  (if (or (null? set1) (null? set2))
      '()
      (let ((x1 (car set1)) (x2 (car set2)))
        (cond ((= x1 x2)
               (cons x1
                     (intersection-set-sorted (cdr set1)
                                              (cdr set2))))
              ((< x1 x2)
               (intersection-set-sorted (cdr set1) set2))
              ((< x2 x1)
               (intersection-set-sorted set1 (cdr set2)))))))

;; ex2.61 : set operation on sorted set
(define (adjoin-set-sorted x set)
  (cond ((null? set) (list x))
        ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set)
                    (adjoin-set-sorted x (cdr set))))))
;; ex2.61 ends here

;; ex2.62
(define (union-set-sorted set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else (let ((x1 (car set1))
                    (x2 (car set2)))
                (cond ((< x1 x2)
                       (cons x1
                             (union-set-sorted (cdr set1) set2)))
                      ((< x2 x1)
                       (cons x2
                             (union-set-sorted set1 (cdr set2))))
                      (else
                       (cons x1
                             (union-set-sorted (cdr set1) (cdr set2)))))))))
;; ex2.62 ends here

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (tree-element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (tree-element-of-set? x (left-branch set)))
        (else
         (tree-element-of-set? x (right-branch set)))))
(define (adjoin-set-tree x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set-tree x (left-branch set))
                    (right-branch set)))
        (else
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set-tree x (right-branch set))))))

;; ex2.63
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))
(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))
;; ex2.63 ends here


;; ex2.64
(define (list->tree elements)
  (car (partial-tree elements (length elements))))
(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))
;; ex2.64 ends here

;; ex2.65
(define (intersection-set-tree tree1 tree2)
  (let ((l1 (tree->list-2 tree1))
        (l2 (tree->list-2 tree2)))
    (let ((intersected-list (intersection-set-sorted l1 l2)))
      (list->tree intersected-list))))
(define (union-set-tree tree1 tree2)
  (let ((l1 (tree->list-2 tree1))
        (l2 (tree->list-2 tree2)))
    (let ((unioned-list (union-set-sorted l1 l2)))
      (list->tree unioned-list))))
;; ex2.65 ends here


;; ex2.66
(define (lookup-tree given-key set-of-records)
  (cond ((null? set-of-records) false)
        ((equal? given-key (key (car set-of-records)))
         (car set-of-records))
        ((> given-key (key (car set-of-records)))
         (lookup-tree given-key (right-branch set-of-records)))
        (else
         (lookup-tree given-key (left-branch set-of-records)))))
;; ex2.66 ends here
