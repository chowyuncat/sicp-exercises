#lang sicp
;
; Section 2.1.1
;

; gcd defition from Section 1.2.5:
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; Debuggable implementatation of ctor and selectors:
; (define (make-rat n d) (cons n d))
; (define (numer x) (car x))
; (define (denom x) (cdr x))

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

; More efficient, removes a procedure call for the selectors
(define numer car)
(define denom cdr)

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define one-half (make-rat 1 2))
(add-rat one-half one-half)

(define one-fourth (make-rat 1 4))
(add-rat one-fourth one-fourth)

;
; Exercise 2.1
;
(define (xor a b) (if a (not b) b))

(define (make-rat-normalize n d)
  (let ((g (abs (gcd n d)))
        (an (abs n))
        (ad (abs d)))
    (if (xor (< n 0) (< d 0))
        (cons (/ (* -1 an) g) (/ ad g))
        (cons (/ n g) (/ d g)))))

(make-rat-normalize 1 -4)
(make-rat-normalize -1 4)