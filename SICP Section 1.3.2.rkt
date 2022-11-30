#lang sicp

(define (square x)
  (* x x))

(define (f g)
  (g 2))

(f square)

; cannot apply '2' to '2'
(f f)

