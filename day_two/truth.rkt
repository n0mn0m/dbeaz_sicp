#lang racket

; Reimplementing Cons

;(define (cons a b)
;  (lambda (m)
;    (cond ((= m 0) a)
;          ((= m 1) b))))
;
;(define (car p) (p 0))
;(define (cdr p) (p 1))

;(define a (cons 3 4))


(define (cons x y)
  (lambda (m) (m x y)))

(define (car z)
  (z (lambda (p q) p)))

(define (cdr y)
  (y (lambda (p q) q))) ; The arguement to cons lambda N which unpacks and passes back the second arg

(define x (cons 3 4))
 
(car x)

; 2.5

(define (int-cons a b)
 (* (expt 2 a) (expt 3 b)))

(define (car-i z)
  (if (= (remainder z 2) 0)
      (+ 1 (car-i (/ z 2)))
      0))

(define (cdr-i y)
  (if (= (remainder y 3) 0)
      (+ 1 (cdr-i (/ y 3)))
      0))

(define g (int-cons 3 4))
(car-i g)
(cdr-i g)

; 2.6

(define zero (lambda (f) (lambda (x) x)))
(define two (lambda (f) (lambda (x) (f (f x)))))
(define three (lambda (f) (lambda (x) (f (f (f x))))))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; How would you implement plus (+)
; a and b are the numbers above, not "normal" numbers

(define (plus a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x))))
  )

(define five (plus two three))

(define (inc x) (+ x 1))

((five inc) 0)




; What is truth?

(define (TRUE x y) x)
(define (FALSE x y) y)

(define t (TRUE 0 1))
(define f (FALSE 0 1))

t
f
; Challenge, can you implement all of boolean logic?

; Hint: x is either TRUE/FLASE above. It is a procedure, it takes two arguments.
;(define (NOT (lambda (x) (f x) x)))
(define (NOT x) (x FALSE TRUE))
(NOT TRUE)
(NOT FALSE)

; (NOT TRUE) --> FALSE
; (NOT FLASE) --> TRUE

(define (AND x y) (x y x))
(AND TRUE FALSE)
; Implement (AND x y)
; (AND TRUE TRUE) -> TRUE
; (AND TRUE FALSE) -> FALSE
; (AND FALSE TRUE) -> FALSE
; (AND FALSE FALSE) -> FALSE

(define (OR x y) (x x y))
(OR FALSE FALSE)
(OR TRUE FALSE)
(OR FALSE TRUE)



(define (zero? n) ((n (lambda (x) #f)) #t)) ; Always returns false, except for zero, because zero says don't do the function so we get back true

;2.18

