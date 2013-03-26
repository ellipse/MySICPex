(define (front-ptr queue) (car queue))
(define (rear-ptr queue) (cdr queue))
(define (set-front-ptr! queue item) (set-car! queue item))
(define (set-rear-ptr! queue item) (set-cdr! queue item))
(define (empty-queue? queue) (null? (front-ptr queue)))
(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

;; ex3.21
(define (print-queue queue)
  (define (pr-iter qu)
    (if (not (null? qu))
        (begin
          (display (car qu))
          (display " ")
          (pr-iter (cdr qu)))))
  (if (empty-queue? queue)
      (display "Empty queue")
      (pr-iter (front-ptr queue))))
;; ex3.21 ends here

;; ex3.22
;; queue structure implemented as a procedure
(define (make-queue)
  (let ((front-ptr '()))
    (let ((rear-ptr front-ptr))
      (define (dequeue!)
        (if (null? front-ptr)
            (error "DELETE! called with an empty queue")
            (begin
              (set! front-ptr (cdr front-ptr))
              front-ptr)))
      (define (enqueue! x)
        (cond ((null? front-ptr)
               (set! front-ptr (cons x front-ptr))
               (set! rear-ptr front-ptr)
               front-ptr)
              (else
               (set-cdr! rear-ptr (list x))
               (set! rear-ptr (cdr rear-ptr))
               front-ptr)))
      (define (dispatch m)
        (cond
         ((eq? m 'empty-queue?) (null? front-ptr))
         ((eq? m 'front-queue) (car front-ptr))
         ((eq? m 'rear-queue) (car rear-ptr))
         ((eq? m 'insert-queue!)
          enqueue!)
         ((eq? m 'delete-queue!)
          (dequeue!))
         ))
      dispatch)))
(define (empty-queue? queue) (queue 'empty-queue?))
(define (front-queue queue) (queue 'front-queue))
(define (rear-queue queue) (queue 'rear-queue))
(define (insert-queue! queue item) ((queue 'insert-queue!) item))
(define (delete-queue! queue) (queue 'delete-queue!))
;; ex3.22 ends here


;; ex3.23
;; the double-end deque structure
(define (front-ptr deque) (car deque))
(define (rear-ptr deque) (cdr deque))

;; these procedures should be invisible to users
(define (element cell) (cdar cell))
;; define prev to be a procedure which returns a pointer to
;; the previous cell, hence prevent the interpreter from
;; getting into a infinite loop when tries to print the queue.
(define (make-prev prev) (lambda () prev))
(define (next cell) (cdr cell))
(define (set-next! c1 c2) (set-cdr! c1 c2))
(define (previous cell) ((caar cell)))
(define (set-prev! cell prev)
  (let ((oldprev (car cell)))
    (set-car! oldprev (make-prev prev))))

;; these fundamental procedures are nearly the same as single-end queue
(define (set-front-ptr! deque item) (set-car! deque item))
(define (set-rear-ptr! deque item) (set-cdr! deque item))
(define (empty-deque? deque) (null? (front-ptr deque)))
(define (make-deque) (cons '() '()))
(define (print-deque deque)
  (define (pr-iter dq)
    (if (not (null? dq))
        (begin
          (display (element dq))
          (display " ")
          (pr-iter (next dq)))))
  (pr-iter (front-ptr deque)))
(define (front-deque deque)
  (if (empty-deque? deque)
      (error "FRONT called with an empty deque" deque)
      (element (front-ptr deque))))
(define (rear-deque deque)
  (if (empty-deque? deque)
      (error "REAR called with an empty deque" deque)
      (element (rear-ptr deque))))

;; insertion needs to set the previous field of the new cell
(define (front-insert-deque! deque item)
  (let ((new-cell (cons (cons '() item) '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-cell)
           (set-rear-ptr! deque new-cell)
           deque)
          (else
           (set-prev! (front-ptr deque) new-cell)
           (set-next! new-cell (front-ptr deque))
           (set-front-ptr! deque new-cell)
           deque))))
(define (rear-insert-deque! deque item)
  (let ((new-cell (cons (cons '() item) '())))
    (cond ((empty-deque? deque)
           (set-front-ptr! deque new-cell)
           (set-rear-ptr! deque new-cell)
           deque)
          (else
           (set-next! (rear-ptr deque) new-cell)
           (set-prev! new-cell (rear-ptr deque))
           (set-rear-ptr! deque new-cell)
           deque))))

;; note that only rear-delete-deque! uses "previous" to find
;; the previous cell
(define (front-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "FRONT-DELETE! called with an empty deque" deque))
        (else
         (set-front-ptr! deque (next (front-ptr deque)))
         (set-prev! (front-ptr deque) '())
         deque)))
(define (rear-delete-deque! deque)
  (cond ((empty-deque? deque)
         (error "REAR-DELETE! called with an empty deque" deque))
        (else
         (set-rear-ptr! deque (previous (rear-ptr deque)))
         (set-next! (rear-ptr deque) '())
         deque)))

;; ex3.23 ends here
