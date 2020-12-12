#lang racket

; AMB evaluator
; Three things:
;   1.  Primitives
;   2.  Symbols
;   3.  Combinations (tuples)


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


; All (seval-* procedures now take success and fail arguments.
; sucess is a procedure that is called to make forward progress
; fail is a procedure that is called to backtrack


; success is called with (success result fail). (2 ags)
; fail is called with no arguments.
;
; You can never return any result from seval. ALL RESULTS are communicated.
;

(define (seval sexp success fail environment)
  (displayln sexp)
  (cond ((primitive? sexp) (seval-primitive sexp success fail environment))
        ((symbol? sexp) (seval-symbol sexp success fail environment))
        ((combination? sexp) (seval-combination sexp success fail environment))
        (else (error "Bad expression")))
  )

(define (primitive? sexp)
  (or (number? sexp) (boolean? sexp))
  )

(define (combination? sexp)
  (list? sexp)
  )

(define (seval-primitive sexp success fail environment)
  (success sexp fail))  ; Primitives just return

; Symbol lookup
(define (seval-symbol sexp success fail environment)
  (success (lookup-environment environment sexp) fail))

; Special forms/procedures
(define (seval-combination sexp  success fail environment)
  (cond ((define? sexp) (seval-define sexp success fail environment))
        ((if? sexp) (seval-if sexp success fail environment))
        ((begin? sexp) (seval-begin sexp success fail environment))
        ((lambda? sexp) (seval-lambda sexp success fail environment))
        ((amb? sexp) (seval-amb sexp success fail environment))
        ; There will be others...
        ((procedure? sexp) (seval-procedure sexp success fail environment))
        (else "Unknown combination")))

; (begin expr1 expr2 ... exprn)
; --> evalutes each expr1, expr2, ... exprn.  Returns exprn

(define (begin? sexp)
  (eq? (car sexp) 'begin))

(define (begin-expression-list sexp)
  (cdr sexp))

(define (seval-begin sexp success fail environment)
  ; Suggestion: Make a procedure that evaluates a list of expressions
  (seval-expression-list (begin-expression-list sexp) environment))

; Evaluate a list of expressions one-by-one, return the value of the last one
(define (seval-expression-list exprs  success fail environment)
  (cond ((null? (cdr exprs)) (seval (car exprs)  success fail environment))  ; Last item
        (else (seval (car exprs)  success fail environment)
              (seval-expression-list (cdr exprs)  success fail environment))))


; (lambda (args) expr1 expr2 ... exprn)
; Technicality: Lambda takes a sequence of expressions (like begin).  Returns
; value of evaluating the last one. 

(define (lambda? sexp)
  (eq? (car sexp) 'lambda))

(define (lambda-args sexp)
  (cadr sexp))

(define (lambda-exprs sexp)
  (cddr sexp))

(define (seval-lambda sexp success fail environ)
  (success (make-procedure (lambda-args sexp) (lambda-exprs sexp)  success fail environ) fail)
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

(define (seval-if sexp success fail environment)
  (seval (if-test sexp)
         (lambda (result fail2)
           (if result
               (seval (if-consequence sexp) success fail2 environment)
               (seval (if-alternative sexp) success fail2 environment)
                      )
               ) fail environment))


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

(define (seval-define sexp  success fail environment)
  (seval (define-value sexp)
         (lambda (result fail)
           (define-in-environment! environment (define-name sexp) result)
           (success 'ok fail))
         fail
         environment)
  )

; Evaluate a procedure
(define (procedure? sexp) (pair? sexp))

(define (procedure-proc sexp)
  (car sexp))

(define (procedure-args sexp)
  (cdr sexp))

;(define (seval-procedure sexp  success fail environment)
;  (let ((proc (seval (procedure-proc sexp)  success fail environment))
;        ; Must evaluate each argument in the environment before calling the proc
;        (args (map (lambda (arg) (seval arg success fail environment)) (procedure-args sexp))))
;    ; Call the procedure.... how????
;    (if (user-procedure? proc)
;        (apply-user-procedure proc args)  ; User-define procedure (lambda)
;        (apply proc args))                ; Built-in procedure (racket)   
;  )
;  )


(define (seval-procedure sexp success fail environment)
  (seval (procedure-proc sexp)
         (lambda (proc fail2)
           ; proc is the procedure
           (seval-all-arguments (procedure-args sexp)
                                (lambda (args fail3)
                                  (success (if (user-procedure? proc)
                                               (apply-user-procedure proc args)
                                               (apply proc args)) fail3)
                                  ) fail2 environment)
           ) fail environment))

(define (seval-all-arguments argexprs success fail environment)
  (define (iter remaining argvalues fail2)
    ; remainig is arguments not yet evaluated
    ; argvalues is a list of arguments evaluated so far
    (if (null? remaining)
        (success argvalues fail2)
        (seval (car remaining)
               (lambda (value fail3)
                 (iter (cdr remaining) (append argvalues (list value)) fail3)
                 )
               fail environment))
        )
    (iter argexprs '() fail)
  )
; Must make some data structure representing the elements of a procedure
; (the arguments, expressions, and environment in which it was defined)

(define (make-procedure args exprs  success fail environ)
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

; New feature, ambiguous evaluations
(define (amb? sexp)
  (eq? (car sexp) 'amb)
  )

(define (amb-exprs sexp)
  (cdr sexp))

(define (seval-amb sexp success fail environment)
  (define (try-next remaining)
    (if (null? remaining)
        (fail)
        (seval (car remaining) success
               (lambda () (try-next (cdr remaining)))
               environment)))
  (try-next (amb-exprs sexp))
  )

; ------ Try it
(seval 23 (lambda (result fail) (displayln result)) (lambda () (displayln "Fail")) env)
(seval '+ (lambda (result fail) (displayln result)) (lambda () (displayln "Fail")) env)
(seval '(define x 42) (lambda (result fail) (displayln result)) (lambda () (displayln "Fail")) env)
(seval 'x (lambda (result fail) (displayln result)) (lambda () (displayln "Fail")) env)
(seval '(if false 2 3) (lambda (result fail) (displayln result)) (lambda () (displayln "Fail")) env)
(seval '(+ 2 3) (lambda (result fail) (displayln result)) (lambda () (displayln "Fail")) env)
(seval '(- 1 0) (lambda (result fail) (displayln result)) (lambda () (displayln "Fail")) env)
(seval '(+ (* 2 3) (* 4 6)) (lambda (result fail) (displayln result)) (lambda () (displayln "Fail")) env)
(seval '(amb) (lambda (result fail) (displayln result)) (lambda () (displayln "Fail")) env)
(seval '(amb 1 2 3) (lambda (result fail) (displayln result) (fail)) (lambda () (displayln "Fail")) env)
(seval '(amb 1 (amb 2 3 4) 5) (lambda (result fail) (displayln result) (fail)) (lambda () (displayln "Fail")) env)


;(define (multiple-dwelling)
;  (let ((baker (amb 1 2 3 4 5))
;        (cooper (amb 1 2 3 4 5))
;        (fletcher (amb 1 2 3 4 5))
;        (miller (amb 1 2 3 4 5))
;        (smith (amb 1 2 3 4 5)))
;    (require
;      (distinct? (list baker cooper fletcher miller smith)))
;    (require (not (= baker 5)))
;    (require (not (= cooper 1)))
;    (require (not (fletcher 5)))
;    (require (not (= fletcher 1)))
;    (require (> miller cooper))
;    (require (not (= (abs (- smith fletcher)) 1)))
;    (require (not (= (abs (- fletcher cooper)) 1)))
;    (list (list 'baker baker)
;          (list 'cooper cooper)
;          (list 'fletcher fletcher)
;          (list 'miller miller)
;          (list 'smith smith))))

