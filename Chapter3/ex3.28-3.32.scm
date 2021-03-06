;; Digital Circuit Simulator

;; if arguments are NOT all legal, then return true
(define (logical-illegal? s . rest)
  (define (illegal-iter a lst)
    (or (and (not (= a 1))
             (not (= a 0)))
        (if (null? lst)
            false
            (illegal-iter (car lst) (cdr lst)))))
  (illegal-iter s rest))


(define (logical-not s)
  (cond ((= s 0) 1)
        ((= s 1) 0)
        (else (error "Invalid Signal" s))))
(define (logical-and s1 s2)
  (cond ((and (= s1 1) (= s2 1)) 1)
        ((logical-illegal? s1 s2)
         (error "Invalid Signal" s1 s2))
        (else 0)))
(define (logical-or s1 s2)
  (cond ((logical-illegal? s1 s2)
         (error "Invalid Signal" s1 s2))
        ((or (= s1 1) (= s2 1)) 1)
        (else 0)))

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input))

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
           (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

;; ex 3.28 ============================================================
(define (or-gate a1 a2 output)
  (define (or-action-procedure)
    (let ((new-value
           (logical-or (get-signal a1) (get-signal a2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value))))
    (add-action! a1 or-action-procedure)
    (add-action! a2 or-action-procedure)
    'ok))

;; ex 3.29 ============================================================
;; (define (or-gate a1 a2 output)
;;   (let ((i1 (make-wire))
;;         (i2 (make-wire))
;;         (m (make-wire)))
;;     (inverter a1 i1)
;;     (inverter a2 i2)
;;     (and-gate i1 i2 m)
;;     (inverter m output)))
;; This type of or-gate has a delay equals
;; 3 * inverter-delay + and-gate-delay
;;  ============================================================


(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))
(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

;; ex 3.30 ============================================================
(define (ripple-carry-adder lst-a lst-b c s-lst)
  ())
