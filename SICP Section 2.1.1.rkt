#lang sicp

; Debuggable implementatation of ctor and selectors:
; (define (make-rat n d) (cons n d))
; (define (numer x) (car x))
; (define (denom x) (cdr x))

; More efficient, removes a procedure call for each operation:
(define make-rat cons)
(define numer car)
(define denom cdr)

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define one-half (make-rat 1 2))
(add-rat one-half one-half)