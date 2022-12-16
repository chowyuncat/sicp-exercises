#lang sicp

(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr z)
  (z (lambda (p q) q)))

;
; Exercise 2.4
;
(define pair (cons 1 2)); -> f(1, 2)
(car pair); -> f(
(cdr pair)

(pair (lambda (a b) (display a)))
