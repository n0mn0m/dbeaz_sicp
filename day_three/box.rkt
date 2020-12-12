#lang racket

; The Box problem

; attach a tag
; Type-tagging

(define (attach-tag tag contents)
  (cons tag contents)
  )

(define (type-tag datum) (car datum))
(define (contents datum) (cdr datum))

; ----- Bob box
(define (make-bob-box x y w h)
  (attach-tag 'bob-box
    (cons (cons x y) (cons w h))))

(define (bob-box? box)
  (eq? (type-tag box) 'bob-box))

(define (bob-width box)
  (car (cdr box)))

(define (bob-height box)
  (cdr (cdr box)))

(define (bob-area box)
  (* (bob-width box) (bob-height box)))

(define a (make-bob-box 1 2 1 2))
a

; ----- Alice Box
(define (make-alice-box x1 y1 x2 y2)
  (attach-tag 'alice-box
    (cons (cons x1 y1) (cons x2 y2))))

(define (alice-box? box)
  (eq? (type-tag box) 'alice-box))

(define (alice-width box)
  (let ((box (contents box)))
    (abs (- (caar box) (cadr box)))))

(define (alice-height box)
  (let ((box (contents box)))
    (abs (- (cdar box) (cddr box)))))

(define (alice-area box)
  (* (alice-width box) (alice-height box)))

(define b (make-alice-box 2 2 2 2))
b

; Generic Procedures
(define (width box)
  (cond ((bob-box? box) (bob-width box))
        ((alice-box? box) (alice-width box))))

(define (height box)
  (cond ((bob-box? box) (bob-height box))
        ((alice-box? box) (alice-height box))))

(define (area box)
  (* (width box) (height box)))

(height b)






; Method registry
(define registry (make-hash))

(define (register name tag func)
 (hash-set! registry (list name tag) func))

(define (lookup name tag)
 (hash-ref registry (list name tag)))


 ; Registration of functions
(register 'width 'bob-box bob-width)
(register 'width 'alice-box alice-width)

 ; Generic procedure
(define (width box)
 ((lookup 'width (type-tag box)) box))

