#lang racket

; Day 2 refresh

;(define a 10)
;
;(if (= a 0) 1 2)
;
;(cond ((< a 0) 1)
;      ((= a 0) 2)
;      ((> a 0) 3))
;
;(if (< a 0) 1
;    (if (= a 0) 2 3))
;
;(let ((x (+ a 10))
;      (y (- a 10)))
;  (+ x y))
;
;
;(define (f a)
;  (define x (+ a 10))
;  (define y (- 1 10))
;  (+ x y)
;  )
;
;(f 2)
;
;(define (g a)
;  (let ((x (+ a 10))
;        (y (- a 10)))
;    (+ x y)))
;
;;(define (g a)
;;  (let ((x (+ a 10))
;;        (y (- x 10))) ; Can't do this because expressions are evaluated before variables.
;;    (+ x y)))
;
;(define (shadow a)
;  (let ((x (+ a 10))
;        (a (- a 10)))
;    (+ x a)))
;
;(define (e a)
;  (define (h x)
;    (+ (square x) 2))
;  (define (square x)
;    (* x x))
;  (h 10)
;  )
;
;(define (h a)
;  (let ((x (+ a 10)))
;    (let ((y (+ x 2)))
;    (+ x y))))
;
;(h 2)
;
;(define (k a)
;  (let* ((x (+ a 10))
;         (y (+ x 2)))
;    (+ x y)))
;
;(k 2)

(define (square x)
  (* x x))

(define (inc x)
  (+ x 1))

;(define (fixed-point f first-guess tolerance)
;  ; Check if two guesses were close enough
;  (define (close-enough? v1 v2)
;    (< (abs (- v1 v1)) tolerance))
;
;  ; Try the next value
;  (define (try guess)
;    (let ((next (f guess)))
;      (if (close-enough? guess next)
;          next
;          (try next))))
;
;  ; Run it
;  (try first-guess))
;
;(define tolerance 0.00001)
;
;(define (fixed-point f first-guess)
;  (define (close-enough? v1 v2)
;    (< (abs (- v1 v1)) tolerance))
;  (define (try guess)
;    (let ((next (f guess)))
;      (if (close-enough? guess next)
;          next
;          (try next))))
;  (try first-guess)
;  )
;
;(fixed-point sqrt 4)
;(fixed-point cos 1.0)
;
;
;
;(define (double f)
;  (lambda (x) (f (f x))))
;
;(((double (double double)) inc) 5)

; (double double) ---> (lambda (x) (double (double x)))
; ((double double) inc) ---> (double (double inc))
;                       ---> (double (lambda (x) (inc (inc x))))
;                       ---> (lambda (x) (inc (inc x)) (lambda (x) (inc (inc x))))
;                       ---> (lambda (x) ((+ x 1) (+ x 1)) (lambda (x) ((+ x 1) (+ x 1))
;
;
;(define a (lambda (y) ((lambda (x) (inc (inc x))) ((lambda (x) (inc (inc x))) y))))
;
;(a 0)
;
;(define (f x)
;  (lambda (x) (x)))
;
;(define (g y)
;  (lambda (y) (y)))


; Exercise 1.42
; Side-comment: function composition is the semi colon of functional programming

;(define (compose f g)
;  (lambda (x) f(g x)))
;
;((compose square inc) 6)
;
;(define (repeated f n)
;  (if (= n 0)
;      (lambda (x) x)
;      (compose f (repeated f (- n 1)))))
;
;((repeated inc 100) 10)


; Excercise 1.46

;(define (iterative-improve good-enough? refine-guess x)
;  (lambda (guess)
;    (define (iter guess)
;      (if (good-enough? guess x)
;          guess
;          (iter (refine-guess guess x))))
;    (iter guess)))

(define (iterative-improve good-enough? improve)
  (define (iter guess)
    (if (good-enough? guess)
        guess
        (iter (improve guess)))
    )
  iter
  )

; Implement sqrt in terms of iterative improvement
(define (average x y) (/ (+ x y) 2))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (* guess guess) x )) 0.0001)
    )
  (define (improve guess)
    (average guess (/ x guess)))

  ((iterative-improve good-enough? improve) 1.0)
  )

(sqrt 2)
(sqrt 4)


(define (fixed-point f first-guess tolerance)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
    (try first-guess))

(fixed-point cos 1.0 0.0001)

(define (fixed-point2 f first-guess tolerance)
  (define (close-enough? v1)
    (let ((v2 (f v1)))
      (< (abs (- v1 v1)) tolerance))
    )
  ((iterative-improve close-enough? f) first-guess)
  )

(fixed-point2 cos 1.0 0.0001)