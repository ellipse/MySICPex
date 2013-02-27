(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (square x) (* x x))

(define (divides? a b)
  (= (remainder b a) 0))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m))
                                m))
        (else (remainder (* base (expmod base (- exp 1) m))
                         m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))


;ex1.21
(smallest-divisor 1999)

;ex1.22
(define (timed-prime-test n)
  (start-prime-test n (real-time-clock)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (real-time-clock) start-time) n)))

(define (report-prime elapsed-time n)
  (display n)
  (display " *** ")
  (display elapsed-time)
  (newline))

(define (search-for-primes a b)
  (define start (if (even? a) (+ a 1) a))
  (define end (if (even? b) (- b 1) b))
  (define (sfp-iter n)
    (if (<= n end)
        (begin (timed-prime-test n)
               (sfp-iter (+ n 2)))))
  (sfp-iter start))


;; ex 1.24
(define (find-divisor n test-divisor)
  (define (next n) (if (= n 2) 3 (+ n 2)))
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))

;; ex 1.28
(define (mr-test n)
 (define (try-iter n a)
   (= (expmod a (- n 1) n)
      1))
 (try-iter n (+ 1 (random (- n 1)))))
(define (mr-prime? n)
  (define (mrt-iter n trytime)
    (cond ((= trytime 0) true)
          ((mr-test n) (mrt-iter n (- trytime 1)))
          (else false)))
  (mrt-iter n (if (< n 100) n 100)))
;; 出错几率为0.5^100
