#lang sicp
;
; Exercise 2.5
;
; number = 2^a * 3^b

(define (cons a b)
  (* (expt 3 a) (expt 7 b)))

(define number (cons 211 309))

; 2^a = number - 3^b
; a is the number of times `number` is evenly divisble by 2
; b is the number of times `number` is evenly divisble by 3
; @TODO: Why? I found these axioms on the internet, but I don't fully
; understand why it's true.

(define (divn divisor number)
  (define (divn-internal n i)
    (if (not (= (remainder n divisor) 0))
        i
        (divn-internal (/ n divisor) (+ i 1))))
  (divn-internal number 0))

(define (car number)
  (divn 3 number))

(define (cdr number)
  (divn 7 number))
  
(car number)
(cdr number)
