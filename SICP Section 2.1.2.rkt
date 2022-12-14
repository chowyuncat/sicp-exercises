#lang sicp
;
; Exercise 2.2
;
(define make-point cons)
(define x-point car)
(define y-point cdr)

(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define (midpoint-segment segment)
  (define (mid coord-sel)
    (/ (+ (coord-sel (start-segment segment))
          (coord-sel (end-segment segment)))
       2))
  (make-segment (mid x-point) (mid y-point)))

(midpoint-segment (make-segment (make-point -1 -1) (make-point 1 1)))
(midpoint-segment (make-segment (make-point 0 0) (make-point 10 10)))
