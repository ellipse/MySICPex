;; ex3.1
(define (make-accumulator init)
  (lambda (x)
    (set! init (+ x init))
    init))
;; ex3.1 ends here

;; ex3.2
(define (make-monitored func)
  (let ((called 0))
    (lambda (x)
      (cond ((eq? x 'how-many-calls?) called)
            ((eq? x 'reset-count) (set! called 0))
            (else
             (begin (set! called (1+ called))
                    (func x)))))))
;; ex3.2 ends here

;; ex3.3
(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pwd op)
    (if (eq? pwd password)
        (cond ((eq? op 'withdraw) withdraw)
              ((eq? op 'deposit) deposit)
              (else (error "Unknown request -- MAKE-ACCOUNT" op)))
        (lambda (x)
          (display "Incorrect password: ")
          (display (string pwd))
          #f)))
  dispatch)
;; ex3.3 ends here

;; ex3.4
(define (make-account balance password)
  (define failed 0)
  (define (call-the-cops x)
    (display "Please stay here, the repairman is coming"))
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch pwd op)
    (if (eq? pwd password)
        (begin
          (set! failed 0)
          (cond ((eq? op 'withdraw) withdraw)
               ((eq? op 'deposit) deposit)
               (else (error "Unknown request -- MAKE-ACCOUNT" op))))
        (if (>= failed 7)
            call-the-cops
            (begin (set! failed (1+ failed))
                   (lambda (x)
                     (display "Incorrect password: ")
                     (display (string pwd))
                     #f)))))
  dispatch)
;; ex3.4 ends here


(define random-init 3)
(define (rand-update x)
  (random (cond ((= x 0) random-init)
                ((<= x 10) (* 4 x))
                (else (* 2 x)))))
(define rand
  (let ((x random-init))
    (lambda ()
      (set! x (rand-update x))
      x)))
(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (/ trials-passed trials))
          ((experiment)
           (iter (- trials-remaining 1) (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1) trials-passed))))
  (exact->inexact (iter trials 0)))

;; ex3.5
(define estimate-integral
  (lambda (P trials x1 x2 y1 y2)
    (define (rand-in-range low high)
      (let ((range (- high low)))
        (+ low (random range))))
    (define (P-test)
      (P (rand-in-range x1 x2) (rand-in-range y1 y2)))
    (* (monte-carlo trials P-test)
       (* (- y2 y1) (- x2 x1)))))
;; ex3.5 ends here

;; ex3.6
(define rand
  (let ((x random-init))
    (lambda (op)
      (cond ((eq? op 'generate)
             (begin (set! x (rand-update x))
                    x))
            ((eq? op 'reset)
             (lambda (new-val) (set! x new-val)))
            (else
             (error "unknown option"))))))
;; ex3.6 ends here

;; ex3.7
(define (make-joint account oldpwd newpwd)
  (lambda (pwd op)
    (if (eq? pwd newpwd)
        (account oldpwd op)
        (lambda (x) (display "new password incorrect") #f))))
;; ex3.7 ends here


;; ex3.8
(define f
  (let ((hehe 1))
    (lambda (x)
      (set! hehe (* hehe x))
      hehe)))
;; on my computer (Debian 3.2.39-2 x86_64 GNU/Linux),
;; the evaluation begins from the right to the left

;; ex3.8 ends here
