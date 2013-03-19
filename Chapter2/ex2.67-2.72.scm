;; Huffman code

(define (make-leaf symbol weight)
  (list 'leaf symbol weight))
(define (leaf? object)
  (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))
(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit -- CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)
                               (cadr pair))
                    (make-leaf-set (cdr pairs))))))
;; ex2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(decode sample-message sample-tree)
;Value 41: (a d a b b c a)

;; ex2.67 ends here


(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

;; ex2.68

(define (encode-symbol msg tree)
  (cond ((leaf? tree)
         (if (equal? msg (symbol-leaf tree))
             '()
             (error "unexpected character -- ENCODE-SYMBOL"
                    (symbols tree))))
        (else
         (let ((left-set (symbols (left-branch tree)))
               (right-set (symbols (right-branch tree))))
           (cond ((member msg left-set)
                  (cons 0 (encode-symbol msg
                                         (left-branch tree))))
                 ((member msg right-set)
                  (cons 1 (encode-symbol msg
                                         (right-branch tree))))
                 (else
                  (error "can't find character" msg)))))))

;; ex2.68 ends here


(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

;; ex2.69
(define (successive-merge leaf-set)
  (if (null? (cdr leaf-set))
      (car leaf-set)
      (successive-merge
       (adjoin-set (make-code-tree (car leaf-set)
                                   (cadr leaf-set))
                   (cddr leaf-set)))))
;; ex2.69 ends here


;; ex2.70
(define rock-words
  '((A 2) (BOOM 1) (NA 16)
    (SHA 3) (GET 2) (YIP 9)
    (JOB 2) (WAH 1)))
(define rocktree (generate-huffman-tree rock-words))

;; the generated huffman tree:
;; ((leaf na 16)
;;   ((leaf yip 9)
;;    (((leaf a 2)
;;      ((leaf wah 1)
;;       (leaf boom 1)
;;       (wah boom) 2)
;;      (a wah boom)
;;      4)
;;     ((leaf sha 3)
;;      ((leaf job 2)
;;       (leaf get 2)
;;       (job get)
;;       4)
;;      (sha job get)
;;      7)
;;     (a wah boom sha job get) 11)
;;    (yip a wah boom sha job get) 20)
;;   (na yip a wah boom sha job get) 36)

(encode '(Get a job
              Sha na na na na na na na na
              Get a job
              Sha na na na na na na na na
              Wah yip yip yip yip yip yip yip yip yip
              Sha boom)
        rocktree)
;; the huffman code of previous song
;; (1 1 1 1 1 1 1 0 0 1 1 1 1 0
;;    1 1 1 0 0 0 0 0 0 0 0 0 1 1
;;    1 1 1 1 1 0 0 1 1 1 1 0 1 1
;;    1 0 0 0 0 0 0 0 0 0 1 1 0 1
;;    0 1 0 1 0 1 0 1 0 1 0 1 0 1
;;    0 1 0 1 0 1 1 1 0 1 1 0 1 1)
;; length = 84


;; ex2.70 ends here


;; ex2.71

(generate-huffman-tree '((a 1) (b 2) (c 4) (d 8) (e 16)))
;; the symbol occuring most frequently occupies 1 bit
;; and that with lowest frequecy occupies n-1 bits
;;   31
;;   / \
;;  e   15
;;     / \
;;    d   7
;;       / \
;;      c   3
;;         / \
;;        b   a

;; ex2.71 ends here

