#lang racket

(define (square x) (* x x))

(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items)) (map proc (cdr items)))))


; only return items where proc item is true
;; (define (filter proc items)
;;   (cond ((null? items) null)
;;         ((proc (car items)) (cons (car items) (filter proc (cdr items)))) ; predicate is true keep the item
;;         (else (filter proc (cdr items))) ;predicate is false
;;   )
;; )

(map (lambda (x) (+ x 1)) '(1 2 3 4))
;; (filter odd? '(1 2 3 4 5 6 7 8 9 0))
;; (filter even? '(1 2 3 4 5 6 7 8 9 0))

;; (define (accumulate ))

; Give combinations of cars and cdrs that will pick 7 from each of the following list

(define o '(1 3 (5 7) 9))
(car(cdr(caddr o)))

; Let's use Church numerals

(define (plus a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x))))
  )

(define p '((7)))
(caar p)

(define q '(1 (2 (3 (4 (5 (6 7)))))))
(car(cdr(car(cdr(car(cdr(car(cdr(car(cdr(car(cdr q))))))))))))

(define x '(1 2 3))
(define y '(4 5 6))

(append x y)
(cons x y)
(list x y)

(define (scale-tree tree factor)
  (cond ((null? tree) null)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                   (scale-tree (cdr tree) factor)))))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
         (else (filter predicate (cdr sequence)))))

(filter odd? '(1 2 3 4 5))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(accumulate + 0 '(1 2 3 4 5))


;; (define (map p sequence)
;;   (accumulate (lambda (x y) __ ) null sequence))

;; (define (append seq1 seq2)
;;   (accumulate cons__ __ ))

;; (define (length sequence)
;;   (accumulate __ 0 sequence))

; Exercise 2.39

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; Exercise 2.54

(define (memq item x)
  (cond ((null? x) false)
        ((eq? item (car x)) x)
        (else (memq item (cdr x)))))

(list 'a 'b 'c)
(list (list 'george))
(cdr '((x1 x1) (y1 y2)))
(cadr '((x1 x2) (y1 y2)))

(pair? (car '(a short list)))
(memq 'red '((red shoes) (blue socks)))
(memq 'red '(red shoes blue socks))

;; (define (equal x y)
;;   (cond ((and (null? x) (null? y)) true)
;;         ((or (null? x) (null? y)) false)
;;         ((and (symbol? x) (symbol? y)) (eq? x y))
;;         ((and (list? x) (list? y) (equal? car(x) car(y)))
;;          (equal? (cdr x) (cdr y)))
;;         (else false)))

(define (equal? seq1 seq2)
  (cond ((and (null? seq1) (null? seq2)) true)
        ((or (null? seq1) (null? seq2)) false)
        ((and (symbol? seq1) (symbol? seq2)) (eq? seq1 seq2))
        ((and (list? seq1) (list? seq2) (equal? (car seq1) (car seq2)))
         (equal? (cdr seq1) (cdr seq2)))
        (else false)))

;; (equal? (list 1 2 3) (list 4 5 6))
(equal? '(this is a list) '(this is a list))
(equal? '(this is a list) '(this (is a) list))

; Exercise how does assoc work?
(define record '((foo 2) (bar 3) (spam 4)))

(define (assoc key items)
  ;; (cond ((null? x) false)
  ;;       ((eq? (car items)) (cdr x))
  ;;       (else assoc items (cdr x))))
  (if (null? items)
      false
      (if (eq? (caar items) key)
          (car items)
          (assoc key (cdr items)))))

(assoc 'bar record)


; Exercise: implement (add-entry key value record) that adds/replaces a value
; (add-entry 'bar 42 record) --> '((foo 2) (bar 42) (spam 4))

(define (add-entry k v items)
  ;; (if (null? record)
      ; add new record and return
      ;; (append record cons(k v))
      ;; (if (eq? (caar record) key)
      ;;     ; update value
      ;; )
  (cond ((null? items) (list (list k v)))
        ((eq? (caar items) k) (cons (list k v) (cdr items)))
        (else (cons (car items) (add-entry k v (cdr items))))))

(add-entry 'bar 43 record)

; Exercise implement deleting a record
(define (del-entry k items)
  (cond ((null? items) '())
        ((eq? (caar items) k) (cdr items))
        (else (cons (car items) (del-entry k (cdr items))))))

(del-entry 'bar record)


; Exercise Implement pattern matching
  ;; (define (equal? seq1 seq2)
  ;;   (cond ((and (null? seq1) (null? seq2)) true)
  ;;         ((or (null? seq1) (null? seq2)) false)
  ;;         ((and (symbol? seq1) (symbol? seq2)) (eq? seq1 seq2))
  ;;         ((and (list? seq1) (list? seq2) (equal? (car seq1) (car seq2)))
  ;;          (equal? (cdr seq1) (cdr seq2)))
  ;;         (else false)))

(define (match pat dat)
;; For a given sequence check if any item(s) in the sequence match
;; the provided pattern
  (cond ((and (null? pat) (null? dat)) true)
        ((or (null? pat) (null? dat)) false)
        ((eq? pat '?) true)
        ((and (list? pat) (list? dat) (match (car pat) (car dat)))
         (match (cdr pat) (cdr dat)))
        ;; (match '(job ? ?) record) ;--> #t (match '(job ? (? coder)) record) ;--> #f
        ;; (match '(job ? (? coder)) record) ;--> #f
        ;; (match '(? ? (computer ?))) ; --> #t
        (else (equal? pat dat)))
)

(define rec '(job (Hacker Alyssa P) (computer programmer)))
(match '(job ? ?) rec)
(match '( job ? (? coder)) rec)
(match '(? ? (computer ?)) rec)

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        (else
         (error "Unknown expression type -- DERIV" exp))))

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2) (list '+ a1 a2))

(define (make-product m1 m2) (list '* m1 m2))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(deriv '(+ x 3) 'x)

(deriv '(* x y) 'x)

(deriv '(* (* x y) (+ x 3)) 'x)
