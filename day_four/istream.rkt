#lang racket

(define (add-streams s1 s2)
  (if (or (stream-empty? s1) (stream-empty? s2))
      empty-stream
      (stream-cons (+ (stream-first s1) (stream-first s2))
                   (add-streams (stream-rest s1) (stream-rest s2)))))

(define ones (stream-cons 1 ones))
(stream-ref ones 173)

(define integers (stream-cons 1 (add-streams ones integers)))
(stream-ref integers 122)

(define fibs (stream-cons 0
                          (stream-cons 1
                                       (add-streams (stream-rest fibs) fibs))))

(stream-ref fibs 0)

(stream-ref fibs 10000000)