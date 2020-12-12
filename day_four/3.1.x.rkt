#lang racket

; Exercise 3.1

(define (make-accumulator value)
  (define (update delta)
    (set! value (+ value delta))
    value
    )
  update
  )

(define (make-monitored proc)
  ; Create a new procedure that wraps around proc and keeps
  ; a running count of how many calls
  (define ncalls 0)
  (define (mf arg)
    (set! ncalls(+ ncalls 1))
    (cond ((eq? arg) 'how-many-call?)
          (else (proc arg))))
  mf
  )

(define a (make-accumulator 5))

(a 10)

(a 10)


; 3.8

(define (f x y)
  (+ x y))

(f (+ (* 3 5)) (- (* 3 (+ 2 10))))


; Create an f

(define (mutate x)
  (define inner 0)
  (if (eq? inner x) inner
      (set! x inner))
      inner
)

(+ (mutate 0) (mutate 1)) ; --> Return 0
(+ (mutate 1) (mutate 0)) ; --> Return 1 Different answer than above

