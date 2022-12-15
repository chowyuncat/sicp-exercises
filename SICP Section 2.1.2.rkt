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

;
; Exercise 2.3
;
; (define (make-rectangle lower-left upper-right))
; @TODO: in ctor, assert order of arguments or normalize internally
(define make-rectangle cons)
(define lower-left car)
(define upper-right cdr)

(define r (make-rectangle (make-point 0 0) (make-point 10 10)))

(define (width rect)
  (define llx (x-point (lower-left rect)))
  (define urx (x-point (upper-right rect)))
  (- urx llx))

(define (height rect)
  (define lly (y-point (lower-left rect)))
  (define ury (y-point (upper-right rect)))
  (- ury lly))

(width r)
(height r)

(define (area rect)
  (* (width rect) (height rect)))

(area r)
