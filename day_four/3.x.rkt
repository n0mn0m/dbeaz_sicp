#lang racket

(define a 42)
(set! a 13)

(define (counter n)
  (define (incr)
    (set! n (+ n 1))
          n
    )
  incr
  )

a

(define c (counter 10))

(c)
(c)
(c)


(define balance 100)

(define (withdraw amt)
  (begin
    (set! balance (- balance amt))
    balance
    )
  )

(withdraw 10)
(withdraw 50)

(begin
  (display balance)
  (newline)
  (set! balance 75)
  (display balance)
  (newline)
  balance
  )


(define (make-counter n)
  (define (incr)
    (begin
      (set! n (+ n 1))
      n
      ))
  incr)

(define cc (make-counter 1000))
(define bb (make-counter 10))

(cc)
(cc)
cc

(bb)
(bb)
bb