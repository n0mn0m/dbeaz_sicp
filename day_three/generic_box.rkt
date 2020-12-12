#lang racket

(define (make-bob-box x y w h)
  (define (dispatch message)
    (cond ((eq? message 'width) w)
          ((eq? message 'height) h)
          ((eq? message 'type) 'bob-box)
          (error "Bad message")))
  dispatch)

(define (make-alice-box x1 y1 x2 y2)
  (define (dispatch message)
    (cond ((eq? message 'width) (abs (- x2 x1)))
          ((eq? message 'height) (abs (- y2 y1)))
          ((eq? message 'type) 'alice-box)
          (error "Bad message")))
  dispatch)

(define (width obj) (obj 'width))
(define (height obj) (obj 'height))
(define (area obj) (* (width obj) (height obj)))

(define a (make-alice-box 1 2 1 2))
(a 'type)
(a 'height)
