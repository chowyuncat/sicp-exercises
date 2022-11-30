#lang sicp

(define (assert x)
  (if (not x)
      (error "drion failed: " + x)
      ))

(define (assertequal a b)
  (if (not (= a b))
      (error "assertion failed: " a  '!=  b)
      ))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a) (sum term (next a) next b))))

(define (cube x) (* x x x))

(assertequal (sum cube 1 inc 10) 3025)

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

;(integral cube 0 1 0.001) ; 0.25

; End code from SIC

(define (even? n)
  (= (remainder n 2) 0))

; Exercise 1.29
; (h / 3) * [ y0 + 4*y1 + 2*y2 + 4*y3 + ... + 4yn-1 + yn]
; yprod = 1 4 2 4 2 4 ... 1
(define (simpson f a b n)
  (define (yprod i)
    (cond ((= i 0) 1)
          ((= i n) 1)
          ((even? i) 2)
          (else 4)))
  
  (define (h)
    (/ (- b a) n))

  (define (y k)
    (* (yprod k) (f (+ a (* k (h))))))

  ; (assertequal (sum yprod 0 inc 4) 12)

  (* (/ (h) 3) (sum y 0 inc n)))

;(display "Integral: ")
;(integral sin 0 1 .000001)
;(display "Simpson : ")
;(simpson sin 0 1 100)

; Exercise 1.30
(define (sum-iter term a next b)
  (define (iter accum i)
    (if (> i b)
        accum
        (iter (+ accum (term i)) (next i))))
  (iter 0 a))
              

(assertequal (sum cube 1 inc 10) (sum-iter cube 1 inc 10))

; Exercise 1.31

(define (product-rec term low next high)
  (if (> low high)
      1
      (* (term low)
         (product-rec term (next low) next high))))

(define (product-iter term low next high)
  (define (product-iter-internal accum i)
    (if (> i high)
        accum
        (product-iter-internal (* accum (term i)) (next i))))
  (product-iter-internal 1 low))

(define (pi-approx product-func n)
  (define (numerator n)
    (cond ((even? n) (+ 2 n))
          (else (+ n 3))))

  (define (denominator n)
    (cond ((even? n) (+ 3 n))
          (else (+ n 2))))

  (define (term n)
    (/ (numerator n) (denominator n)))

  (* 4.0 (product-func term 0 inc n)))

(define (timeit-two-arg func arg0 arg1)
  (define (show-time start-time)
    (display (func arg0 arg1)) (newline)
    (display (/ (- (runtime) start-time) 1000000.0)))
  
  (show-time (runtime)))


(display "Computing iterative pi approximation: ...")
(timeit-two-arg pi-approx product-iter 5000)
;(pi-approx product-iter 1000)
(newline)

(display "Computing recursive pi approximation: ...")
(timeit-two-arg pi-approx product-rec  5000)
(newline)


; Exercise 1.32
(define (accumulate-rec combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-rec combiner null-value term (next a) next b))))

(define (sum-accum term a next b)
  (accumulate-rec + 0 term a next b))

(assertequal 3025 (sum-accum cube 0 inc 10))

(define (prod-accum term a next b)
  (accumulate-rec * 1 term a next b))

(assertequal (product-rec cube 1 inc 10) (prod-accum cube 1 inc 10))

; Exercise 1.33
(define (filtered-accumulate pred combiner null-value term a next b)
  (define (term-or-null x)
    (if (pred x)
        (term x)
        null-value))
  
  (if (> a b)
      null-value
      (combiner (term-or-null a)
                (filtered-accumulate pred combiner null-value term (next a) next b))))

(define (square n)
  (* n n))

(define (smallest-divisor n)
  (find-divisor-fast2 n 2))

(define (find-divisor-fast2 n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor-fast2 n
                                 (if (= test-divisor 2)
                                     3
                                     (+ test-divisor 2))))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (sum-of-primes-squared low high)
  (filtered-accumulate prime? + 0 square low inc high))

(assertequal (sum-of-primes-squared 1 10) 88); (1 + 4 + 9 + 25 + 49)