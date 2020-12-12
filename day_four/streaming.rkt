#lang racket

(define (enumerate-interval start stop)
  (if (> start stop)
      empty-stream
      (stream-cons start
                   (enumerate-interval (+ start 1) stop))))

(define (stream-map proc s)
  (if (stream-empty? s)
      empty-stream
      (stream-cons (proc (stream-first s))
                    (stream-map proc (stream-rest s)))))

(define nums (enumerate-interval 1 5))
(define squares (stream-map (lambda (x) (* x x)) nums))
(define invsquares (stream-map (lambda (x) (/ 1.0 x)) squares))

(define (stream-sum s)
  (if (stream-empty? s)
      0
      (+ (stream-first s) (stream-sum (stream rest s)))))

(stream-for-each displayln invsquares)