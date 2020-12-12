#lang racket

; Metacircular evaluator (Chapter 4)

; Three things:
;   1.  Primitives
;   2.  Symbols
;   3.  Combinations (tuples)

(define (seval sexp environment)
  (displayln sexp)
  (cond ((primitive? sexp) (seval-primitive sexp environment))
        ((symbol? sexp) (seval-symbol sexp environment))
        ((combination? sexp) (seval-combination sexp environment))
        (else (error "Bad expression")))
  )

(define (primitive? sexp)
  (or (number? sexp) (boolean? sexp))
  )

(define (combination? sexp)
  (list? sexp)
  )

(define (seval-primitive sexp environment) sexp)  ; Primitives just return

(define (seval-symbol sexp environment)
  (lookup-environment environment sexp))

; Special forms/procedures
(define (seval-combination sexp environment)
  (cond ((define? sexp) (seval-define sexp environment))
        ((if? sexp) (seval-if sexp environment))
        ((begin? sexp) (seval-begin sexp environment))
        ((lambda? sexp) (seval-lambda sexp environment))
        ; There will be others...
        ((procedure? sexp) (seval-procedure sexp environment))
        (else "Unknown combination")))

; (begin expr1 expr2 ... exprn)
; --> evalutes each expr1, expr2, ... exprn.  Returns exprn

(define (begin? sexp)
  (eq? (car sexp) 'begin))

(define (begin-expression-list sexp)
  (cdr sexp))

(define (seval-begin sexp environment)
  ; Suggestion: Make a procedure that evaluates a list of expressions
  (seval-expression-list (begin-expression-list sexp) environment))

; Evaluate a list of expressions one-by-one, return the value of the last one
(define (seval-expression-list exprs environment)
  (cond ((null? (cdr exprs)) (seval (car exprs) environment))  ; Last item
        (else (seval (car exprs) environment)
              (seval-expression-list (cdr exprs) environment))))


; (lambda (args) expr1 expr2 ... exprn)
; Technicality: Lambda takes a sequence of expressions (like begin).  Returns
; value of evaluating the last one. 

(define (lambda? sexp)
  (eq? (car sexp) 'lambda))

(define (lambda-args sexp)
  (cadr sexp))

(define (lambda-exprs sexp)
  (cddr sexp))

(define (seval-lambda sexp environ)
  (make-procedure (lambda-args sexp) (lambda-exprs sexp) environ)
  )

; (if test consequence alternative)
(define (if? sexp)
  (eq? (car sexp) 'if))

(define (if-test sexp)    ; Data "selectors" 
  (cadr sexp))

(define (if-consequence sexp)
  (caddr sexp))

(define (if-alternative sexp)
  (cadddr sexp))

(define (seval-if sexp environment)
  (if (seval (if-test sexp) environment)
      (seval (if-consequence sexp) environment)
      (seval (if-alternative sexp) environment)))


; (define name value)
(define (define? sexp)
  (eq? (car sexp) 'define)
)

(define (define-name sexp)
  (cadr sexp)
  )

(define (define-value sexp)
  (caddr sexp)
  )

(define (seval-define sexp environment)
  (define-in-environment! environment
    (define-name sexp)
    (seval (define-value sexp) environment)
  )
)

; Evaluate a procedure
(define (procedure? sexp) (pair? sexp))

(define (procedure-proc sexp)
  (car sexp))

(define (procedure-args sexp)
  (cdr sexp))

(define (seval-procedure sexp environment)
  (let ((proc (seval (procedure-proc sexp) environment))
        ; Must evaluate each argument in the environment before calling the proc
        (args (map (lambda (arg) (seval arg environment)) (procedure-args sexp))))
    ; Call the procedure.... how????
    (if (user-procedure? proc)
        (apply-user-procedure proc args)  ; User-define procedure (lambda)
        (apply proc args))                ; Built-in procedure (racket)   
  )
  )

; Must make some data structure representing the elements of a procedure
; (the arguments, expressions, and environment in which it was defined)

(define (make-procedure args exprs environ)
  (list 'user-procedure args exprs environ))  ; A "lambda" object

(define (procedure-environment proc)
  (cadddr proc))

(define (procedure-exprs proc)
  (caddr proc))

(define (procedure-argnames proc)
  (cadr proc))

(define (user-procedure? proc)
  (and (list? proc) (eq? (car proc) 'user-procedure)))

(define (apply-user-procedure proc argvalues)
  ; 1. Create a new environment
  (let ((newenv (make-child-environment (procedure-environment proc))))
    
  ; 2. Bind the argument names/values in the new environment
    (bind-names newenv (procedure-argnames proc) argvalues)
    
  ; 3. Evaluate the procedure expressions in the new environment
    (seval-expression-list (procedure-exprs proc) newenv))
  )

(define (bind-names env argnames argvalues)
  (cond ((and (null? argnames) (null? argvalues)) 'ok)
        ((or (null? argnames) (null? argvalues)) (error "Argument mismatch"))
        (else (define-in-environment! env (car argnames) (car argvalues))
              (bind-names env (cdr argnames) (cdr argvalues)))))

; ---------- Environments. 
(define (make-environment)
  (cons (make-hash) null)    ; Racket Hash table
  )

(define (make-child-environment env)
  (cons (make-hash) env)
  )

; Recursively check environments until found
(define (lookup-environment environment symbol)
  (if (null? environment)
      (error "undefined name")
      (if (hash-has-key? (car environment) symbol)
          (hash-ref (car environment) symbol)
          (lookup-environment (cdr environment) symbol))))

(define (define-in-environment! environment symbol value)
  (hash-set! (car environment) symbol value)   ; Racket built-in
  )

; Create the global environment
(define env (make-environment))
; Define "Built-in" procedures
(define-in-environment! env '+ +)
(define-in-environment! env '* *)
(define-in-environment! env '- -)
(define-in-environment! env '/ /)
(define-in-environment! env '< <)
(define-in-environment! env '> >)
(define-in-environment! env '<= <=)
(define-in-environment! env '>= >=)
(define-in-environment! env '= =)
(define-in-environment! env 'true true)
(define-in-environment! env 'false false)

  

; ------ Try it
(seval '(define foo 42) env)
(seval 'foo env)
;(define-in-environment! env 'foo 42)
;(lookup-in-environment env 'foo)
(seval '23 null)
(seval '(+ 2 3) env)
(seval '(+ (* 2 3) (* 4 5)) env)
(seval '(begin 1 2 3) env)
(seval '(define square (lambda (x) (* x x))) env)
(seval '(square 10) env)
(seval '(define fact (lambda (n) (if (= n 0) 1 (* n (fact (- n 1)))))) env)
(seval '(fact 5) env)

