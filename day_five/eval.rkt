#lang racket
(define prog
'(define (fact n) (if (= n 1) 1 (* n (fact (- n 1))))))

prog

(car prog)

(caddr prog)