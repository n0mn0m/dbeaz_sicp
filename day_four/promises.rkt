#lang racket

(define p (delay (+ 10 20)))
p
(force p)