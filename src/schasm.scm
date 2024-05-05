(import (scheme base)
        (scheme write)
        (srfi 1)
        (srfi 111))

;;; expression

(define (pattern-match pattern)
  (cond ((procedure? pattern)
         pattern)
        ((pair? pattern)
         (let ((car-pattern (pattern-match (car pattern)))
               (cdr-pattern (pattern-match (cdr pattern))))
           (lambda (value)
             (and (pair? value)
                  (car-pattern (car value))
                  (cdr-pattern (cdr value))))))
        (else
         (lambda (value)
           (equal? pattern value)))))

(define (list-of predicate)
  (lambda (value)
    (and (list? value))
    (every predicate value)))

(define (expression? value)
  (or (literal?  value)
      (variable? value)
      (lambda?   value)

      (ref?      value)
      (unref?    value)
      (set-ref!? value)

      (if?       value)

      (reset?    value)
      (shift?    value)

      (call?     value)))

(define (literal? value)
  (or (boolean? value)
      (number?  value)))

(define variable? symbol?)

(define lambda? (pattern-match `(lambda ,(list-of variable?) ,expression?)))
(define lambda-variables cadr)
(define lambda-expression caddr)

(define ref? (pattern-match `(ref ,expression?)))
(define ref-expression cadr)

(define unref? (pattern-match `(unref ,expression?)))
(define unref-expression cadr)

(define set-ref!? (pattern-match `(set-ref! ,expression? ,expression?)))
(define set-ref!-left cadr)
(define set-ref!-right caddr)

(define if? (pattern-match `(if ,expression? ,expression? ,expression?)))
(define if-condition cadr)
(define if-consequent caddr)
(define if-alternative cadddr)

(define reset? (pattern-match `(reset ,expression?)))
(define reset-expression cadr)

(define shift? (pattern-match `(shift ,variable? ,expression?)))
(define shift-variable cadr)
(define shift-expression caddr)

(define call? (pattern-match `(,expression? . ,(list-of expression?))))
(define call-function car)
(define call-arguments cdr)

;;; environment

(define empty-environment '())

(define (environment-lookup environment variable)
  (let ((entry (assoc variable environment)))
    (unless entry
      (error "environment-lookup: unbound variable" variable))
    (cdr entry)))

(define (environment-add environment variable value)
  (cons (cons variable value) environment))

(define (environment-add* environment variables values)
  (unless (= (length variables) (length values))
    (error "environment-add*: not same length" variables values))
  (fold (lambda (variable value environment)
          (environment-add environment variable value))
        environment
        variables
        values))

;;; store

(define-record-type <store>
  (make-store location content)
  store?
  (location store-location)
  (content store-content))

(define empty-store (make-store 0 '()))

(define (store-lookup store location)
  (let* ((entry (assoc location (store-content store))))
    (unless entry
      (error "store-lookup: unallocated memory" location store))
    (cdr entry)))

(define (store-allocate store value continuation)
  (let* ((location (store-location store))
         (content  (store-content  store))
         (store (make-store (+ 1 location)
                           (cons (cons location value) content))))
    (continuation store location)))

(define (store-update store location value)
  (make-store (store-location store)
              (cons (cons location value)
                    (store-content store))))

;;; metacontext

(define (empty-metacontext store value)
  value)

(define (metacontext-resume metacontext store value)
  (metacontext store value))

(define (metacontext-add metacontext context)
  (lambda (store value)
    (context metacontext store value)))

;;; context

(define empty-context metacontext-resume)

(define (context-resume context metacontext store value)
  (context metacontext store value))


;;; values

(define (make-immediate expression)
  expression)

(define (make-closure environment variables expression)
  (lambda (metacontext context store arguments)
    (unless (= (length variables) (length arguments))
      (error "closure: arity error" variables arguments))
    (evaluate expression
              (environment-add* environment
                                variables
                                arguments)
              store
              context
              metacontext)))

(define (make-continuation context)
  (lambda (metacontext context* store arguments)
    (unless (= 1 (length arguments))
      (error "continuation: arity error" arguments))
    (context-resume context
                    (metacontext-add metacontext context*)
                    store
                    (car arguments))))

(define-record-type <reference>
  (make-reference location)
  reference?
  (location reference-location))

;;; interpreter

(define (evaluate expression environment store context metacontext)
  (cond ((literal?  expression)
         (context-resume context
                         metacontext
                         store
                         (make-immediate expression)))

        ((variable? expression)
         (context-resume context
                         metacontext
                         store
                         (environment-lookup environment expression)))

        ((lambda?   expression)
         (let ((variables  (lambda-variables  expression))
               (expression (lambda-expression expression)))
           (context-resume context
                           metacontext
                           store
                           (make-closure environment variables expression))))

        ((if?    expression)
         (let ((condition   (if-condition   expression))
               (consequent  (if-consequent  expression))
               (alternative (if-alternative expression)))
           (evaluate condition
                     environment
                     store
                     (lambda (metacontext store value)
                       (evaluate (if value consequent alternative)
                                 environment
                                 store
                                 context
                                 metacontext))
                     metacontext)))

        ((ref? expression)
         (let ((expression (ref-expression expression)))
           (evaluate expression
                     environment
                     store
                     (lambda (metacontext store value)
                       (store-allocate
                        store value
                        (lambda (store location)
                          (context-resume context
                                          metacontext
                                          store
                                          (make-reference location)))))
                     metacontext)))

        ((unref? expression)
         (let ((expression (unref-expression expression)))
           (evaluate expression
                     environment
                     store
                     (lambda (metacontext store reference)
                       (let* ((location (reference-location reference))
                              (value (store-lookup store location)))
                        (context-resume context
                                        metacontext
                                        store
                                        value)))
                     metacontext)))

        ((set-ref!? expression)
         (let ((left  (set-ref!-left  expression))
               (right (set-ref!-right expression)))
           (evaluate* (list left right)
                      environment
                      store
                      (lambda (metacontext store values)
                        (let* ((left  (reference-location (first  values)))
                               (right (second values))
                               (store (store-update store left right)))
                            (context-resume context
                                            metacontext
                                            store
                                            right)))
                      metacontext)))

        ((reset? expression)
         (let ((expression (reset-expression expression)))
           (evaluate expression
                     environment
                     store
                     empty-context
                     (metacontext-add metacontext context))))

        ((shift? expression)
         (let* ((variable   (shift-variable   expression))
                (expression (shift-expression expression))
                (continuation (make-continuation context))
                (environment (environment-add environment
                                              variable
                                              continuation)))
           (evaluate expression
                     environment
                     store
                     empty-context
                     metacontext)))

        ((call?  expression)
         (let* ((function  (call-function  expression))
                (arguments (call-arguments expression))
                (expressions (cons function arguments)))
           (evaluate* expressions
                      environment
                      store
                      (lambda (metacontext store values)
                        (let ((function  (car values))
                              (arguments (cdr values)))
                          (invoke function arguments store context metacontext)))
                      metacontext)))

        (else
         (error "evaluate: not valid expression" expression))))

(define (evaluate* expressions environment store context metacontext)
  (if (null? expressions)
      (context-resume context metacontext store '())
      (let ((expression  (car expressions))
            (expressions (cdr expressions)))
        (evaluate expression
                  environment
                  store
                  (lambda (metacontext store value)
                    (evaluate* expressions
                               environment
                               store
                               (lambda (metacontext store values)
                                 (context-resume context
                                                 metacontext
                                                 store
                                                 (cons value values)))
                               metacontext))
                  metacontext))))

(define (invoke function arguments store context metacontext)
  (function metacontext context store arguments))

;;; testing

(define (procedure->value procedure)
  (lambda (metacontext context store arguments)
    (context-resume context metacontext store (apply procedure arguments))))

(define default-environment
  (environment-add*
   empty-environment
   `(< + -)
   (map procedure->value `(,< ,+ ,-))))

(define program
  `((lambda (fib)
      ((lambda (_)
         ((unref fib) 10))
       (set-ref! fib
                 (lambda (x)
                   (if (< x 2)
                       x
                       (+ ((unref fib) (- x 1))
                          ((unref fib) (- x 2))))))))
    (ref #f)))

(define result (evaluate program
                         default-environment
                         empty-store
                         empty-context
                         empty-metacontext))

(display result)
(newline)
