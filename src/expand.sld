(define-library (schasm expand)
  (export expand
          alpha-conversion
          anf-conversion)
  (import (scheme base)
          (srfi 1)
          (schasm expression))
  (begin
    (define gensym
      (let ((count 0))
        (lambda _
          (set! count (+ count 1))
          (string->symbol
           (string-append "sym-" (number->string count))))))

    (define (transform fn expression)
      (define (unwrap expression)
        (cond ((literal? expression)
               expression)

              ((quote? expression)
               expression)

              ((variable? expression)
               expression)

              ((lambda? expression)
               (let ((variables  (lambda-variables  expression))
                     (expression (lambda-expression expression)))
                 `(lambda ,variables ,(transform fn expression))))

              ((begin? expression)
               (let ((expressions (begin-expressions expression)))
                 `(begin . ,(map (lambda (e) (transform fn e)) expressions))))

              ((if? expression)
               (let ((condition   (if-condition   expression))
                     (consequent  (if-consequent  expression))
                     (alternative (if-alternative expression)))
                 `(if ,(transform fn condition)
                      ,(transform fn consequent)
                      ,(transform fn alternative))))

              ((let? expression)
               (let ((variables       (let-variables       expression))
                     (initializations (let-initializations expression))
                     (expression      (let-expression      expression)))
                 `(let ,(map (lambda (v e)
                               `(,v ,(transform fn e)))
                             variables
                             initializations)
                    ,(transform fn expression))))

              ((reset? expression)
               (let ((expression (reset-expression expression)))
                 `(reset ,(transform fn expression))))

              ((shift? expression)
               (let ((variable   (shift-variable   expression))
                     (expression (shift-expression expression)))
                 `(shift ,variable ,(transform fn expression))))

              ((call? expression)
               (let ((function  (call-function expression))
                     (arguments (call-arguments expression)))
                 `(,(transform fn function)
                   . ,(map (lambda (e) (transform fn e)) arguments))))

              (else
               (error "transform: invalid expression" expression))))

      (fn unwrap expression))

    (define (substitution x y expression)
      (define (lambda-capture? x expression)
        (and (lambda? expression)
             (member x (lambda-variables expression))))

      (define (let-capture? x expression)
        (and (let? expression)
             (member x (let-variables expression))))

      (define (shift-capture? x expression)
        (and (shift? expression)
             (equal? x (shift-variable expression))))

      (transform
       (lambda (unwrap expression)
         (cond ((equal? x expression)
                y)
               ((lambda-capture? x expression)
                expression)
               ((let-capture? x expression)
                expression)
               ((shift-capture? x expression)
                expression)
               (else
                (unwrap expression))))
       expression))

    (define (alpha-conversion expression)
      (transform
       (lambda (unwrap expression)
         (cond ((lambda? expression)
                (let* ((variables  (lambda-variables expression))
                       (expression (lambda-expression expression))
                       (temporaries (map gensym variables))
                       (expression (fold (lambda (variable temporary expression)
                                           (substitution variable
                                                         temporary
                                                         expression))
                                         (unwrap expression)
                                         variables
                                         temporaries)))
                  `(lambda ,temporaries ,expression)))

               ((let? expression)
                (let* ((variables       (let-variables expression))
                       (initializations (let-initializations expression))
                       (expression      (let-expression expression))
                       (temporaries     (map gensym variables))
                       (initializations (map unwrap initializations))
                       (expression (fold (lambda (variable temporary expression)
                                           (substitution variable
                                                         temporary
                                                         expression))
                                         expression
                                         variables
                                         temporaries))
                       (bindings (map list temporaries initializations)))
                  `(let ,bindings ,expression)))

               ((shift? expression)
                (let* ((variable   (shift-variable   expression))
                       (expression (shift-expression expression))
                       (temporary (gensym))
                       (expression (substitution variable temporary expression)))
                  `(shift ,temporary ,(unwrap expression))))

               (else
                (unwrap expression))))
       expression))

    (define (anf-conversion expression)
      (transform
       (lambda (unwrap expression)
         (cond ((begin? expression)
                (let* ((expressions (begin-expressions expression))
                       (expression  (car expressions))
                       (expressions (cdr expressions)))
                  (if (null? expressions)
                      (unwrap expression)
                      (let ((temporary (gensym)))
                        `(let ((,temporary ,(unwrap expression)))
                           ,(unwrap `(begin . ,expressions)))))))
               ((if? expression)
                (let ((condition   (if-condition expression))
                      (consequent  (if-consequent expression))
                      (alternative (if-alternative expression))
                      (temporary (gensym)))
                  `(let ((temporary ,condition))
                     (if ,temporary
                         ,(unwrap consequent)
                         ,(unwrap alternative)))))
               ((call? expression)
                expression)
               (else
                (unwrap expression))))
       expression))

    (define (expand expression)
      (anf-conversion (alpha-conversion expression)))))
