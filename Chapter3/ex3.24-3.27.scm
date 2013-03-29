;; 1-dimensional table
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
        (cdr record)
        false)))
(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
        (set-cdr! record value)
        (set-cdr! table
                  (cons (cons key value) (cdr table))))))
(define (make-table) (list '*table*))

;; 2-dimensional table
(define (lookup key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (cdr record)
              false))
        false)))
(define (insert! key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table))))))

;; 2-dimensional table implemented as a procedure
(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))


;; ex3.24
(define (make-table same-key?)
  (let ((local-table (list '*table*)))
    (define (assoc key records)
      (cond ((null? records) false)
            ((same-key? key (caar records)) (car records))
            (else (assoc key (cdr records)))))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (cdr record)
                  false))
            false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1
                                  (cons key-2 value))
                            (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))
;; ex3.24 ends here



;; ex3.25
;; an arbitrary N-dimensional table
(define (lookup keylist table)
  (let ((subtable (assoc (car keylist) (cdr table)))
        (restkey (cdr keylist)))
    (if subtable
        (if (null? restkey)
            (cdr subtable)
            (lookup restkey subtable))
        false)))


(define (insert! keylist value table)
  (define (make-subtable klst)
    (if (null? (cdr klst))
        (cons (car klst) value)
        (list (car klst)
              (make-subtable (cdr klst)))))
  (define (insert-helper klst t)
    (let ((record (assoc (car klst) (cdr t)))
          (restkey (cdr klst)))
      (if record
          (if (null? restkey)
              (set-cdr! record value)
              (insert-helper restkey record))
          (set-cdr! t
                    (cons (make-subtable klst)
                          (cdr t)))))
    'ok)
  (insert-helper keylist table))

(define (make-table) (list '*table*))
;; ex3.25 ends here


;; ex3.26
;; keys are orgnized in lexicographic order by using a binary tree.
;; all the same-level keys(or nodes) are on one tree, and the key 
;; of lower level belongs to trees which are "content" of the
;; upper level's nodes

(define (lexi-order x1 x2)
  (let ((str1 (if (number? x1)
                  (number->string x1)
                  (symbol->string x1)))
        (str2 (if (number? x2)
                  (number->string x2)
                  (symbol->string x2))))
    (cond ((string<? str1 str2) '<)
          ((string=? str1 str2) '=)
          ((string>? str1 str2) '>)
          (else
           (error "LEXI-ORDER cannot determine the order")))))
(define (key-select tree) (car tree))
(define (left-branch tree)
  (cadr tree))
(define (set-left-branch tree newnode)
  (set-car! (cdr tree) newnode))
(define (right-branch tree)
  (caddr tree))
(define (set-right-branch tree newnode)
  (set-car! (cddr tree) newnode))
(define (content tree)
      (cadddr tree))
(define (set-content! tree newc)
  (set-cdr! (cddr tree) (list newc)))
(define (assoc-tree key tree . failpos)
  (if (null? failpos)
      (cond ((null? tree) false)
            (else
             (let ((order (lexi-order key (key-select tree))))
               (cond ((eq? order '=) tree)
                     ((eq? order '>)
                      (assoc-tree key (right-branch tree)))
                     ((eq? order '<)
                      (assoc-tree key (left-branch tree)))
                     (else
                      (error "ASSOC-TREE"))))))

      ;; when failpos is not null, return the position
      ;; where search finally fails. the position is included
      ;; in a list which has key "*nonexist*"
      (cond ((null? tree) (cons '*nonexist* (car failpos)))
            (else
             (let ((order (lexi-order key (key-select tree))))
               (cond ((eq? order '=) tree)
                     ((eq? order '>)
                      (assoc-tree key (right-branch tree) tree))
                     ((eq? order '<)
                      (assoc-tree key (left-branch tree) tree))
                     (else (error "ASSOC-TREE"))))))))



(define (lookup keylist tree)
  (let ((subtree (assoc-tree (car keylist) tree))
        (restkey (cdr keylist)))
    (if subtree
        (if (null? restkey)
            (content subtree)
            (lookup restkey (content subtree)))
        false)))


(define (insert! keylist value tree)
  (define (make-subtree klst)
    (if (null? (cdr klst))
        (list (car klst) '() '() value)
        (list (car klst)
              '()
              '()
              (make-subtree (cdr klst)))))
  (define (insert-helper klst t)
    (let ((record (assoc-tree (car klst) t t))
          (restkey (cdr klst))
          (currkey (car klst)))
      (if (eq? (car record) '*nonexist*)

          (let ((inskey (key-select (cdr record)))
                (newsubtree (make-subtree klst)))
            (cond ((eq? (lexi-order currkey inskey)
                        '>)
                   (set-right-branch (cdr record) newsubtree))
                  ((eq? (lexi-order currkey inskey)
                        '<)
                   (set-left-branch (cdr record) newsubtree))
                  (else
                   (error "INSERT! what the fuck"))))

          (if (null? restkey)
              (set-content! t value)
              (insert-helper restkey (content record)))))
    'ok)
  (insert-helper keylist tree))

;; every tree node contains a "key" field, a "left-branch", a "right-branch",
;; and a "content" field which includes lower level keys.
;; the header node containing key "*tree*" has no "content" or "left-branch".
(define (make-tree) (list '*tree* '() '() '()))

;; ex3.26 ends here
