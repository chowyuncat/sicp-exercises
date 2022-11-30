#lang sicp
(define (cube x) (*  x x x))

(define (p x) (- (* 3 x ) (* 4 (cube x))))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
    angle
    (p (sine (/ angle 3.0)))))

(sine 12.15)

(p (p (p (p (p 0.05)))))

; Order of growth
(define (sine-print angle n)
  (display n) (newline)
  (if (not (> (abs angle) 0.1))
    angle
    (p (sine-print (/ angle 3.0) (+ n 1)))))

(sine-print 3 0)
(sine-print 9 0)
(sine-print 27 0)
(sine-print 72 0)
(sine-print 121 0)