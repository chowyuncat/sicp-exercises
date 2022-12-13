#lang sicp

(define (iterative-improve good-enough? improve-guess)
  (define (named-lambda guess)
    (if (good-enough? guess)
        guess
        (named-lambda (improve-guess guess))))
  named-lambda)

(define tolerance 0.00001)
(define (square x) (* x x))

(define (sqrt x)
  (define (sqrt-good-enough? guess)
    (< (abs (- (square guess) x)) tolerance))

  (define (sqrt-improve-guess guess)
    (/ (+ guess (/ x guess)) 2))
  ((iterative-improve sqrt-good-enough? sqrt-improve-guess)
   1.0))

(sqrt 25)