#lang sicp
(define (fib-recursive n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (fib-iter a b n)
  (if (= n 0)
      b
      (fib-iter (+ a b) a (- n 1))))

(define (fib n)
  (fib-iter 1 0 n))

; Exercise 1.11
(define (frec n)
  (if (< n 3)
      n
      (+ (frec (- n 1)) (frec (- n 2)) (frec (- n 3)))))

(define (fiter a b c n)
  (cond ((< n 3) a)
        (else (fiter (+ a b c) a b (- n 1)))))

(define (f n)
  (fiter 2 1 0 n))

"rec 3, 4, 5, 6:"
(frec 3)
(frec 4)
(frec 5)
(frec 6)
(frec 7)
(frec 20)
"iter 3, 4, 5, 6:"
(f 3)
(f 4)
(f 5)
(f 6)
(f 7)
(f 20)



