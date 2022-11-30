#lang sicp



(define (search-half-interval f a b)
  (let ((midpoint (/ (+ a b) 2.0)))
    (let ((avg (f midpoint)))
      (cond ((< (abs avg) 0.001)
              midpoint)
            ((> avg 0)
             (search-half-interval f a midpoint))
            (else (search-half-interval f midpoint b))))))
          

(define (f x)
  (+ x 1.0))

(search-half-interval sin 4.0 2.0)
(search-half-interval (lambda (x) (- (* x x x) (* 2 x) 3))
                      1.0
                      2.0)

(define (fixed-point f guess prevguess)
  (let ((foff (f guess)))
    (if (< (abs (- guess prevguess)) 0.0000000001)
        foff
        (fixed-point f foff guess))))
        
(fixed-point cos 1.0 10000000.0)



; Exercise 1.35
#|
Derive transformation:
x^2 = x + 1, subtract x from both sides
x^2 - x = 1, factor out x
x * (x - 1) = 1, divide both sides by x
x - 1 = 1/x, add 1 to both sides
x = 1/x + 1
|#
(define (goldenratio-transformation x)
  (+ 1 (/ 1 x)))

(fixed-point goldenratio-transformation 1.0 100000000.0)

; Exercise 1.36
(define (fixed-point-prints f guess prevguess)
  (let ((foff (f guess)))
    (display foff) (newline)
    (if (< (abs (- guess prevguess)) 0.001)
        foff
        (fixed-point-prints f foff guess))))

; (fixed-point-prints (lambda (x) (/ (log 1000) (log x))) 10 10000000.0)

; Exercise 1.37
(define (cont-frac-rec n d k)
 (define (cont-frac-rec-internal i)
    ; (display p) (newline)
    (if (= k i)
        0
        (/ (n i) (+ (d i) (cont-frac-rec-internal (+ i 1))))))
  (cont-frac-rec-internal 0))

(define (cont-frac-iter n d k)
 ; the variable `r` is an accumulator that represents the i+1th term
 (define (cont-frac-iter-internal r i)
    (display (/ (n i) (+ (d i) r))) (newline)
    (if (= i 0)
        (/ (n i) (+ (d i) r))
        (cont-frac-iter-internal (/ (n i) (+ (d i) r)) (- i 1))))
  (cont-frac-iter-internal 0 (- k 1)))


(display "Iterative:\n")
(cont-frac-iter
 (lambda (i) 1.0)
 (lambda (i) 1.0)
   10)


(display "Recursive:\n")
(cont-frac-rec
 (lambda (i) 1.0)
 (lambda (i) 1.0)
   10)

; Exercise 1.38
(define (euler-denom i)
  (if (= (remainder (- i 1) 3) 0)
      (* (/ 2 3) (+ i 2))
      1))

(display "euler denoms: \n")
(euler-denom 0)
(euler-denom 1)
(euler-denom 2)
(euler-denom 3)
(euler-denom 4)

; 0.718281828459045 = 2 - e
; 0.7183            ~= 4 decimal places
; 7 iterations required
(display "Recursive:\n")
(cont-frac-rec
 (lambda (i) 1.0)
 euler-denom
   7)

(display "Iterative:\n")
(cont-frac-iter
 (lambda (i) 1.0)
 euler-denom
   7)

; Exercise 1.39
; `x` is in radians
(define (fast-expt-iter a b n)
  (cond ((< n 2) (* a b))
        ((even? n) (fast-expt-iter a (* b b) (/ n 2)))
        (else (fast-expt-iter (* a b) b (- n 1)))))

(define (pow b n)
  (fast-expt-iter 1 b n))

(define (square x)
  (display "(square x)\n")
  (* x x))

(define (tan-cf x k)
  (let ( (xsquared (square x)) )
  (define (n i)
    
  
    (if (= i 1)
        x
        xsquared))
    
  (define (d i)
    (- (* 2 i) 1))
 
 (define (tan-cf-internal i)
    ;(display (n i)) (display " / ") (display (d i)) (newline)
    (if (= k i)
        0
        (/ (n i) (- (d i) (tan-cf-internal (+ i 1))))))
  (tan-cf-internal 1)))

(tan-cf 1.0 20)
(tan-cf 3.14159 20)