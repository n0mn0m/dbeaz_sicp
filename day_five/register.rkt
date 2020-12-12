#lang racket

(define (execute)
  (let ((instructions (pc 'get)))
    (cond ((null? instructions) 'done)
          (else
           (pc 'set! (cdr instructions))
           ((car instructions))
           (execute)
           ))))

(define (make-register)
  (let ((value 'unassigned))
    (define (dispatch m . args)
      (cond ((eq? m 'get) value)
            ((eq? m 'set!) (set! value (car args)))
            (else (error "Bad op"))))
    dispatch)
  )


(define (dec->n)
  (n 'set! (- (n 'get) 1)))

(define (p->result)
  (result 'set! (* (n 'get) (result 'get))))

(define (1->result)
  (result 'set! 1))

(define (n-is-one?)
  (= (n 'get) 1))

(define (goto-fact-test)
  (pc 'set! fact-test))


(define (branch-fact-done-if-n-is-1)
  (if (= (n 'get) 1)
      (pc 'set! fact-done)
      '()
      )
 )


(define (goto-fact-update)
  (pc 'set! fact-update))

(define fact-entry (list
                    1->result
                    goto-fact-test
                    ))

(define fact-test (list
                   branch-fact-done-if-n-is-1
                   goto-fact-update))
 
(define fact-update (list
                     p->result
                     dec->n
                     goto-fact-test
))
 
(define fact-done (list))

(define pc (make-register))




(define n (make-register))
(define result (make-register))
(n 'set! 4)
(pc 'set! fact-entry)
(execute)
(result 'get)
