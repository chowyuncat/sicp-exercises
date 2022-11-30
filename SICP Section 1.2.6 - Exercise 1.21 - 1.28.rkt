#lang sicp
(define (square n)
  (* n n))

(define (smallest-divisor n)
  (find-divisor-naive n 2))

(define (find-divisor-naive n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor-naive n (+ test-divisor 1)))))

(define (find-divisor-fast n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor-fast n (next test-divisor)))))


(define (find-divisor-fast2 n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor-fast2 n
                                 (if (= test-divisor 2)
                                     3
                                     (+ test-divisor 2))))))

(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

; Exercise 1.21
(display "Exercise 1.21\n")
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))


(define (timed-prime-test primefunc n)
  (newline)
  (display n)
  (start-prime-test primefunc n (runtime)))

(define (start-prime-test primefunc n start-time)
  (run-n-times 10000 primefunc n)
  (report-prime (primefunc n) (- (runtime) start-time)))

(define (report-prime primality elapsed-time)
  (display " *** ")
  (display primality)
  (display " ")
  (display (/ elapsed-time 1000000.0))
  (display " s\n"))

; Exercise 1.22
(define (search-for-primes minbound count)
  ;(display "search-for-primes ")
  ;(display minbound)
  ;(display " ")
  ;(display count)
  ;(newline)
  (cond ((= count 0)
         (newline))
        ((prime? (+ minbound 1))
         (display (+ minbound 1))
         (display " ")
         (search-for-primes (+ minbound 1) (- count 1)))
        (else
         (search-for-primes (+ minbound 1) count))))

(define (run-n-times count f n)
  ;(display "run-n-times\n")
  (f n)
  (if (> count 0)
    (run-n-times (- count 1) f n)))
             
(define (timeit f a b)
  (define (timeit-internal start-time f)
    (run-n-times 3 f a b)
    ; (display (- (runtime) start-time))
    (display (/ (- (runtime) start-time) 1000.0))
    (display " ms\n"))
  (timeit-internal (runtime) f))
      

(if (= 0 1)
 ((timeit search-for-primes 1000 3)
  (timeit search-for-primes 10000 3)
  (timeit search-for-primes 100000 3)
  (timeit search-for-primes 1000000 3)
  (timeit search-for-primes 10000000000 3))
 (display "skipped if"))

; (timeit search-for-primes 10000000000000 3)
; (timeit search-for-primes 100000000000000 3)
; (timeit search-for-primes 1000000000000000 3)

#| The primes found above:

1009 1013 1019 

10007 10009 10037 

100003 100019 100043 

1000003 1000033 1000037 
|#

; fast        naive
; sqrt(n/2)  / sqrt(n)
; sqrt(10/2) / sqrt(10)
; sqrt(5)    / sqrt(10) ~= 0.7 of naive runtime

; (timed-prime-test prime?      1000000007)
(timed-prime-test fermat-test 1000000007)

(define (error-if-not-equal f g base exp m)
  (display base)
  (display "^")
  (display exp)
  (display " / ")
  (display m)
  (newline)
  (display (f base exp m))
  (newline)
  (display (g base exp m))
  (newline)
  ; (error "different")
  (if (not (= (f base exp m) (g base exp m)))
      (error "different")))
  #t

(define (fuzz-test f g count)
  (display (error-if-not-equal f g (random 1000000) (random 1000000) (random 1000000)))
  (newline)
  (if (> count 0)
      (fuzz-test f g (- count 1))
      0))


(define (fast-expt-iter a b n)
  (cond ((< n 2) (* a b))
        ((even? n) (fast-expt-iter a (square b) (/ n 2)))
        (else (fast-expt-iter (* a b) b (- n 1)))))

(define (expt-iter b n)
  (fast-expt-iter 1 b n))

(define (expmod-alternative base exp m)
  (remainder (expt-iter base exp) m))

; Exercise 1.25
(display "Exercise 1.25:\n")
; (fuzz-test expmod expmod-fast 3)


; Exercise 1.27
; A Carmichael number is both not prime and passes the Fermat Test
(define (carmichael-fast? n)
  (and (fermat-test n) (not (prime? n))))

; for every a < n, test whether rem(a^n, n) = rem(a, n)
(define (congruent? a n)
  (= (expmod a n n) (remainder a n)))

(define (carmichael-test-internal a n)
  (if (< a n)
      (and (congruent? a n) (carmichael-test-internal (+ a 1) n))
      #t))

(define (carmichael-iter? n)
  (carmichael-test-internal 1 n))

(define (carmichael? n)
  (display "carmichael? ")
  (display n)
  (display ": ")
  (carmichael-iter? n))
  (newline)

; Should be false
(carmichael? 64)   ; not prime and fails Fermat test
(carmichael? 1999) ; prime and passes Fermat test
; Should be true
(carmichael? 561)
(carmichael? 1105)
(carmichael? 1729)
(carmichael? 2465)
(carmichael? 2821)
(carmichael? 6601)

; @TODO  Exercise 1.28 

; expmod with Miller-Rabin test
(define (nontrivial-sqrt? n)
  (and (> 1 n) (= (square n) (remainder 1 n))))

(define (expmod-mr base exp m)
  (cond ((= exp 0) 1)
        ;((nontrivial-sqrt? exp) 0)
        ((even? exp)
         (remainder (square (expmod-mr base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod-mr base (- exp 1) m))
                    m))))

(define (miller-rabin-test n)
  (display "miller-rabin ")
  (display n)
  (display ": ")
  ; a^(n-1) modulo n
  (define (try-it a)
    (= (expmod-mr a (- n 1) n) a))
  (try-it (+ 1 (random (- n 1)))))
  (newline)

(remainder 1 9999)

(fermat-test 561)  ; false positive
(fermat-test 1999) ; true  positive
(miller-rabin-test 561)
(miller-rabin-test 1999)

