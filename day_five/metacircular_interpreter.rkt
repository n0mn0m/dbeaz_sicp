#lang racket

; ---------- Environments
(define (make-environment)
  (cons (make-hash) null)    ; Racket Hash table
  )

(define (make-child-environment env)
  (cons (make-hash) env)
  )

(define (lookup-environment environment symbol)
  (if (null? environment)
      (error "undefined name")
      (if (hash-has-key? (car environment) sybol)
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



; Metacircular evaluator (Chapter 4.2)

; 1. Primitives
; 2. Symbols
; 3. Combinations (tuples)

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
  (lookup-in-environment environment sexp))

; Special forms/procedures
(define (seval-combination sexp environment)
  (cond ((define? sexp) (seval-define sexp environment))
        ((if? sexp) (seval-if sexp environment))
        ; There will be others...
        ((procedure? sexp) (seval-procedure sexp environment))
        ((begin? sexp) (seval-begin sexp environment))
        ; and
        ; or
        ; cond
        ; lambda
        (else "Unknown combination")))

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
    (apply proc args)     ; apply is a built-in scheme feature    
  )
  )

; Begin

(define (begin? sexp)
  (eq? (car sexp) 'begin))

(define (begin-expression-list sexp)
  (cdr sexp))

(define (seval-begin sexp environment)
  ; Implement a function to process the list
  (seval-expression-list (begin-expression-list sexp) environment))

(define (seval-expression-list exprs environment)
  ; evaluate a list of expressions one-by-one, return the value of the last one
  (cond ((null? (cdr exprs)) (seval (car exprs) environment)) ; Last item
        (else (seval (car exprs) environment)
              (seval-expression-list (cdr exprs) environment)))
  )


; Lambda
(define (lambda? sexp)
  (eq? (car sexp) 'lambda))

(define (lambda-args sexp)
  (cadr sexp))

(define (lambda-exprs sexp)
  (cddr sexp))

  (define (make-procedure args expres environ)
    (list 'user-procedure args exprs environ)) ; A "lambda" object

(define (user-procedure? proc)
  (and (list? proc) (eq? (car proc) 'user-procedure)))

(define (procedure-environment proc)
  (cadddr proc))

(define (procedure-args proc)
  (cadr proc))

(define (apply-user-procedure proc args)
  ; 1. Create a new environment
  (let ((newenv (make-child-environment ( procedure-environment proc))))
  ; 2. Bind the argument names in the new environment
    (bind-names newenv (procedure-argnames proc) args)
  ; 3. Evaluate the procedure expressions in the new environment
    (seval-expression-list (procedure-exprs proc) newenv))
  )

(define (bind-names env argnames argvalues)
  (cond ((and (null? argnames) (null? argvalues)) 'ok)
        ((or (null? argnames) (null? argvalues)) (error "Argument mismatch"))
        (else (define-in-environment! env (car argnames) (car argvalues))
              (bind-names env (cdr argnames) (cdr argvalues)))))
  

; ------ Try it
(seval '(define foo 42) env)
(seval 'foo env)
;(define-in-environment! env 'foo 42)
;(lookup-in-environment env 'foo)
(seval '23 null)
(seval '(+ 2 3) env)
(seval '(+ (* 2 3) (* 4 5)) env)
(seval '(begin 1 2 3) env)