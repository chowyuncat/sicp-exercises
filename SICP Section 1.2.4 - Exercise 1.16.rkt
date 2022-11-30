#lang sicp
(define (expt-rec b n)
  (if (= n 0)
      1
      (* b (expt-rec b (- n 1)))))

(expt-rec 2 4)
(expt-rec 3 3)

(define (even? n)
  (= (remainder n 2 ) 0))

(even? 1)
(even? 2)
(even? 3)

(define (square n)
  (* n n))

(define (fast-expt-iter a b n)
  (display " f ")
  (display a)
  (display " ")
  (display b)
  (display " ")
  (display b)
  (display " ")
  (display n) (newline)

  (cond ((< n 2) (* a b))
        ((even? n) (display " even: ") (fast-expt-iter a (square b) (/ n 2)))
        (else (display " odd:  ") (fast-expt-iter (* a b) b (- n 1)))))

(define (expt-iter b n)
  (display "exp-iter ")
  (display b)
  (display " ")
  (display n) (newline)
  (fast-expt-iter 1 b n))

(display "function under test:\n")
(expt-iter 2 4)
(expt-iter 2 3)
(expt-iter 2 5)
(expt-iter 2 7)
(expt-iter 3 5)
(expt-iter 3 14)
(expt-iter 3 15)



