#lang racket
; larger programs saved to files

;(define (square x) (* x x))
;(define (sum_of_square x y) (+ (square x) (square y)))
;
;(define (two_largest_sum_of_squares a b c)
;  (cond ((and (<= a b) (<= a c)) (sum_of_square b c)) ; if a is the smallest sum square b c
;        ((and (<= b a) (<= b c)) (sum_of_square a c)) ; else check if b or c is smaller
;        ((and (<= c a) (<= c b)) (sum_of_square a b)) ; if b is the smallest sum square a c
;  )
;)
; 
;(square 2)
;
;(two_largest_sum_of_squares 1 2 3)
;
;(+ 1 2 4 5 6 7 9 0)
;
;
;
;(define (improve guess x)
;  (average guess (/ x guess)))
;
;(define (average x y)
;  (/ (+ x y) 2))
;
;(define (good-enough? guess x)
;  (< (abs (- (square guess) x)) 0.001))
;
;#; (define (sqrt-iter guess x)
;  (if (good-enough? guess x)
;      guess
;      (sqrt-iter (improve guess x)
;                 x)))
;
;#; (define (sqrt x)
;  (sqrt-iter 1.0 x))
;
;(sqrt 9)
;
;
;
;
;
;
;
;
;(define (new-if predicate then-clause else-clause)
;  (cond (predicate then-clause)
;        (else else-clause)))
;
;(new-if (= 2 3) 0 5)
;
;(define (sqrt-iter guess x)
;  (new-if (good-enough? guess x)
;          guess
;          (sqrt-iter (improve guess x)
;                     x)))
;
;(sqrt 9)
;
;
;
; Exercise 1.8

;(define (cubert-iter guess x)
;  (if (good-enough? guess x)
;      guess
;      (cubert-iter (improve guess x) x)))
;
;(define (cube x) (* x x x))
;
;(define (good-enough? guess x)
;  (< (abs (- (cube guess) x)) 0.0001)
;  )
;
;(define (improve guess x)
;  (/ (+ (/ x (* guess guess)) (* 2 guess))
;     3))
;
;(define (cubert x)
;  (cubert-iter 1.0 x))
;
;(cubert 9)

;; (define (average x y)
;;   (/ (+ x y) 2))

;; (define (square x) (* x x))

;; (define (sqrt x)
;;   (define (good-enough? guess x)
;;     (< (abs (- (square guess) x)) 0.001))
;;   (define (improve guess x)
;;     (average guess(/ x guess)))
;;   (define (sqrt-iter guess x)
;;     (if (good-enough? guess x)
;;         guess
;;         (sqrt-iter (improve guess x) x)))
;;   (sqrt-iter 1.0 x))

;; (sqrt 9)

;; (define (cubert x)
;;   (define (cubert-iter guess x)
;;     (if (good-enough? guess x)
;;         guess
;;         (cubert-iter (improve guess x) x)))
;;   (define (good-enough? guess x)
;;     (< (abs (- (cube guess) x)) 0.0001)
;;     )
;;   (define (cube x) (* x x x))
;;   (define (improve guess x)
;;     (/ (+ (/ x (* guess guess)) (* 2 guess))
;;        3))
;;   (cubert-iter 1.0 x))

;; (cubert 9)

;; (define (+ a b)
;;   (if (= a 0)
;;       b
;;       (inc (+ (dec a) b))))

;; dec 4 inc 5
;;

;; (+ 4 5)

;; (define (+ a b)
;;   (if (= a 0)
;;       b
;;       (+ (dec a) (inc b))))

;; (+ 4 5)

;; (define (expt b n)
;;   (expt-iter b n 1))

;; (define (expt-iter b counter product)
;;   (if (= counter 0)
;;   product
;;   (expt-iter b
;;              (- counter 1)
;;              (* b product))))

;; (define (fast-expt b n)
;;   (cond ((= n 0) 1)
;;         ((even? n) (square (fast-expt b (/ n 2))))
;;         (else (* b (fast-expt b (- n 1))))))

;; (define (even? n)
;;   (= (remainder n 2) 0))

;; (define (square x) (* x x))

;; (define (fast-expi b n)
;;   (define (fast-iter b n result)
;;     (cond ((= n 0) result) ; base case ...... return the result
;;           ((even? n) (fast-iter (square b) (/ n 2) result)) ; Recursive call, but no operations left behind
;;           ((odd? n) (fast-iter b (- n 1) (* b result)))
;;           )
;;     )
;;   (fast-iter b n 1)
;;   )

;; (fast-expi 9 3)


;; (define (cube x) (* x x x))

;; (define (sum term a next b)
;;   (if (> a b)
;;       0
;;       (+ (term a)
;;          (sum term (next a) next b))))

;; (define (inc n) (+ n 1))

;; (define (sum-cubes a b)
;;   (sum cube a inc b))

;; (sum-cubes 1 10)



;; (define (sum term a next b)
;;   (define (iter a result)
;;     (if (> a b)
;;         result
;;         (iter (next a) (+ (term a) result))
;;         (iter a 0)
;;         )
;;     )
;; )

;; (define (product term a next b)
;;   (define (iter a result)
;;   (if (> a b)
;;       result
;;       (iter (next a) (* (term a) result))
;;       )
;;     )
;; )

;; (define (product term a next b)
;;   (if (> a b)
;;       1
;;       (* (term a)
;;          (product term (next a) next b))))

;; (define (identity x) x)
;; (define (factorial n) (product identity 1 inc n))
;; (factorial 6)


(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate combiner null-value term (next a) next b))
  )
)

(define (inc n) (+ n 1))

(define (identity x) x)

(accumulate * 1 identity 1 inc 6)

(define (sum-a term a next b)
  (accumulate + 0 term a next b))

;;(define (inc n) (+ n 1))

;;(define (accumulate combiner null-value term a next b)
;;  (if (> a b)
;;      null-value
;;      (combiner (term a)
;;                (accumulate combiner null-value term (next a) next b))
;;  )
;;)

(define (accumulate-i combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner (term a) result)))
    )
    (iter a null-value)
    )

(accumulate * 1 identity 1 inc 6)
;;(accumulate-i * 1 identity 1 inc 6)(define (accumulate-i combiner null-value term a next b)
;;  (define (iter ... result)
;;    )
;;  (iter ... )
;;  k)


;; 1.31
;; pi/4 = term = (n-1) * (n+1) \ n*n
