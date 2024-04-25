(define-library (schasm expression)
  (export expression?
          literal? boolean? number?
          variable?
          lambda? lambda-variables lambda-expression
          begin? begin-expressions
          if? if-condition if-consequent if-alternative
          let? let-bindings let-variables let-initializations let-expression
          reset? reset-expression
          shift? shift-variable
          shift-expression call?
          call-function call-arguments)
  (import (scheme base)
          (scheme cxr)
          (scheme write)
          (scheme read)
          (srfi 1))
  (begin
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

    (define begin? (pattern-match `(begin ,expression? . ,(list-of expression?))))

    (define begin-expressions cdr)

    (define if? (pattern-match `(if ,expression? ,expression? ,expression?)))

    (define if-condition cadr)

    (define if-consequent caddr)

    (define if-alternative cadddr)

    (define binding? (pattern-match `(,variable? ,expression?)))

    (define binding-variable car)

    (define binding-expression cadr)

    (define let? (pattern-match `(let ,(list-of binding?) ,expression?)))

    (define let-bindings cadr)

    (define (let-variables value)
      (map binding-variable (let-bindings value)))

    (define (let-initializations value)
      (map binding-expression (let-bindings value)))

    (define let-expression caddr)

    (define reset? (pattern-match `(reset ,expression?)))

    (define reset-expression cadr)

    (define shift? (pattern-match `(shift ,variable? ,expression?)))

    (define shift-variable cadr)

    (define shift-expression caddr)

    (define call? (pattern-match `(,expression? . ,(list-of expression?))))

    (define call-function car)

    (define call-arguments cdr)))
