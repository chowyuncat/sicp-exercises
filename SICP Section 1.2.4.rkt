#lang sicp
(define (expt-rec b n)
  (if (= n 0)
      1
      (* b (expt-rec b (- n 1)))))

(expt-rec 2 4)
(expt-rec 3 3)

(define (expt-iter accum b n)
  (if (= n 0)
      accum
      (expt-iter (* b accum) b (- n 1))))

(expt-iter 1 2 4)
(expt-iter 1 3 3)

