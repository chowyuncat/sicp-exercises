#lang sicp

(define (pascal x y)
  (cond ((< x 0) 0)
        ((= x 0) 1)
        ((< y 2) 1)
        ((= x y) 1)
        (else (+ (pascal (- x 1) (- y 1)) (pascal x (- y 1))))))

(define (p x y)
  (pascal x y))

(define (binomial-coefficient x y a n)
  (display "step:")
  (display n) (newline)
  (if (= n 1)
      a
      (binomial-coefficient x y (* (+ x y) (+ x y)) (- n 1))))

(display "computing ...") (newline)
(binomial-coefficient 0 0 1 1)
(display "") (newline)
(binomial-coefficient 0 0 1 2)
(binomial-coefficient 0 0 1 2)
(display "") (newline)
(binomial-coefficient 0 0 1 3)
(binomial-coefficient 0 0 1 3)
(binomial-coefficient 0 0 1 3)
(binomial-coefficient 0 0 1 3)

(error "success")
(p 0 0)
""
(p 0 1)
(p 1 1)
""
(p 0 2)
(p 1 2)
(p 2 2)
""
(p 0 3)
(p 1 3)
(p 2 3)
(p 3 3)

""
(define y 10)
(p 0 y)
(p 1 y)
(p 2 y)
(p 4 y)
(p 5 y)
(p 6 y)
(p 7 y)
(p 8 y)

