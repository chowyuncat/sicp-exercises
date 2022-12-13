#lang sicp

;
; Section 1.3.4
;

; Whoa, operators are first class procedures!
(define (assert operator x y)
  (define (show)
    (display "x: ")(display x)(newline)
    (display "y: ")(display y)(newline)
    (error "assertion failed"))
  (if (not (operator x y))
      (show)))
     
(define (square x) (* x x))

(define tolerance 0.000001)
(define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
; takes as input two parameters, a function and first input to the function
(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define dx 0.00001)

(define (derive g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((derive g) x)))))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method
   (lambda (y) (- (* y y) x))
   1))

(sqrt (* 789 789))

;
; Exercise 1.40
; x^3 + a*x^2 + b*x + c
;
(define (cubic a b c)
  (lambda (x) (+ (* x x x) (* a x x) (* b x) c)))

; https://www.wolframalpha.com/input?i=x%5E3+%2B+2x%5E2+%2B+3x+%2B+4
(newtons-method (cubic 2 3 4) 1)

;
; Exercise 1.41
;
(define (double f)
  (lambda (x) (f (f x))))

(define (inc x) (+ x 1 ))

((double inc) 1)

(((double (double double)) inc) 5)

(define (compose f g)
  (lambda (x) (f (g x))))

;
; Exercise 1.42
;
((compose square inc) 6)
(assert = ((compose square inc) 6) 49) 

;
; Exercise 1.43
;
(define (repeated f n)
  (if (= n 1)
      f
      (repeated (compose f f) (- n 1))))

((repeated square 2) 5)
(assert = ((repeated square 2) 5) 625)

;
; Exercise 1.44
;
(define (smooth f)
  (define dx 0.001)
  (lambda (x)
    (/ (+ (f (- x dx))
          (f x)
          (f (+ x dx)))
       3)))

(define (jagged x)
  (* (sin x) (sin x) (sin x)))

(define (smooth-n f n)
  (repeated (smooth f) n))


(jagged .5)
((smooth jagged) 0.5)
((smooth-n jagged 1) 0.5)
((smooth-n jagged 2) 0.5)

; (#%require plot)

;
; Exercise 1.45
;
(display "Exercise 1.45\n")
(define (average a b) (/ (+ a b) 2))
(assert = (average 7 3) 5)

(define (sqrenout-avg-damp x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))

(define (cuberoot x)
  (fixed-point (lambda (y) (average y (/ x (* y y))))
               1.0))

(cuberoot 27)

(define (fourthroot x)
  (fixed-point (lambda (y) (average y (average y (/ x (* y y y)))))
               1.0))

(define (pow-slow b n)
  (define (pow-int a step)
    (if (= 0 step)
        a
        (pow-int (* a b) (- step 1))))
  (pow-int 1 n))

(define (even? n)
  (= (remainder n 2 ) 0))

(define (fast-expt-iter a b n)
  (cond ((< n 2) (* a b))
        ((even? n) (fast-expt-iter a (square b) (/ n 2)))
        (else (fast-expt-iter (* a b) b (- n 1)))))

(define (pow b n) (fast-expt-iter 1 b n))

(assert = (pow 2 5) 32)

(define (nth-root-avg-damp2 x n)
  (fixed-point (lambda (y) (average y (average y (/ x (pow y (- n 1))))))
               1.0))

(define (nth-root-avg-damp3 x n)
  (fixed-point (lambda (y) (average y (average y (average y (/ x (pow y (- n 1)))))))
               1.0))

; returns a function that takes single parameter `x` as input
(define (average-damp f)
  (lambda (x) (average x (f x))))

(define (fixed-point-verbose f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (display "guess: ") (display guess) (newline)
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (nth-root-bad x n)
  (display "x: ") (display x) (display " ")
  (display "n: ") (display n) (newline)
  ; @TODO compute floor of sqrt of n
  (define repetitions (floor (sqrt n)))
  (display repetitions)(newline)

  ; (define i 1)
  (define (average-damp-of-f)
    (average-damp (lambda (y) (/ x (pow y (- n 1))))))
  ; r = repeated
  ; g = average
  ; f = our fixed point transformation
  ; Incorrect order of application of functions:
  ; (r (g f) 3) ->
  ;(fixed-point-verbose (repeated (average-damp-of-f) repetitions)
  ;                1.0)
 
  #|
  (define (average-damp-i f)
    ((repeated average-damp 3) f))
 
  (fixed-point-verbose (average-damp-i
                        (lambda (y) (/ x (pow y (- n 1)))))
               1.0)
  |#

  (display "incorrect:")
  (display
  ((repeated (average-damp-of-f) 2)
   1.5))
  (newline)
  (display "correct:")
  (display
  (((repeated average-damp 2) (lambda (y) (/ x (expt y (- n 1)))))
   1.5))
  (newline)
  
  ; Correct order of application of functions:
  ; ((r g 3) f)
  (fixed-point-verbose ((repeated average-damp repetitions)
                       (lambda (y) (/ x (expt y (- n 1)))))
               1.0))

(define (nth-root x n)
  (define repetitions (floor (sqrt n)))
  (fixed-point-verbose
     ((repeated average-damp repetitions)
        (lambda (y) (/ x (expt y (- n 1)))))
     1.0))

(define (roughly? v1 v2)
    (< (abs (- v1 v2)) 0.001))

(assert close-enough? (fourthroot (pow 121.0 4)) 121.0)
(assert close-enough? (nth-root-avg-damp2 81 2) 9)
(assert close-enough? (nth-root-avg-damp2 (pow 2 6) 6) 2)
(assert close-enough? (nth-root-avg-damp2 (pow 3 7) 7) 3)
(nth-root-avg-damp3 (pow 3 8) 8)
(nth-root-avg-damp3 (pow 3 15) 15)
; Need floor( sqrt ( base ) ) average damps for x^(1/base)
(display "Computing ...\n")
; (assert roughly? (nth-root (pow 25 3) 3) 25)
; (assert roughly? (nth-root (pow 11 5) 5) 11)
(assert roughly? (nth-root-bad (pow 4 2) 2) 4)
; (assert roughly? (nth-root-bad (pow 10 20) 20) 10)
; (assert roughly? (nth-root (pow 4 9) ) 4)

