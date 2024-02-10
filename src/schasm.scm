(import (scheme base)
        (scheme cxr)
        (scheme write)
        (scheme read)
        (srfi 1)
        (schasm expression)
        (schasm expand))

;;; environment

(define empty-environment '())

(define (environment-add environment variable value)
  (cons (cons variable value) environment))

(define (environment-add* environment variables values)
  (fold (lambda (variable value environment)
          (environment-add environment variable value))
        environment
        variables
        values))

(define (environment-get environment variable)
  (let ((entry (assoc variable environment)))
    (unless entry
      (error "environment-get: variable not found in environment"
             variable
             environment))
    (cdr entry)))

;;; metacontext

(define empty-metacontext '())

(define (metacontext-add metacontext context)
  (cons context metacontext))

(define (metacontext-resume metacontext value)
  (if (null? metacontext)
      value
      (let ((context     (car metacontext))
            (metacontext (cdr metacontext)))
        (context-resume context metacontext value))))

;;; context

(define empty-context '())

(define (context-add context frame)
  (cons frame context))

(define (context-resume context metacontext value)
  (if (null? context)
      (metacontext-resume metacontext value)
      (let ((frame   (car context))
            (context (cdr context)))
        (cond ((begin-frame? frame)
               (begin-frame-resume frame context metacontext value))

              ((if-frame? frame)
               (if-frame-resume frame context metacontext value))

              ((let-frame? frame)
               (let-frame-resume frame context metacontext value))

              ((call-frame? frame)
               (call-frame-resume frame context metacontext value))))))

;;; frame

(define-record-type <begin-frame>
  (make-begin-frame environment expressions)
  begin-frame?
  (environment begin-frame-environment)
  (expressions begin-frame-expressions))

(define (begin-frame-resume frame context metacontext value)
  (let ((environment (begin-frame-environment frame))
        (expressions (begin-frame-expressions frame)))
    (if (null? expressions)
        (context-resume context
                        metacontext
                        value)
        (let ((expression  (car expressions))
              (expressions (cdr expressions)))
          (evaluate expression
                    environment
                    (context-add context
                                 (make-begin-frame environment
                                                   expressions))
                    metacontext)))))

(define-record-type <if-frame>
  (make-if-frame environment consequent alternative)
  if-frame?
  (environment if-frame-environment)
  (consequent  if-frame-consequent)
  (alternative if-frame-alternative))

(define (if-frame-resume frame context metacontext value)
  (let ((environment (if-frame-environment frame))
        (consequent  (if-frame-consequent  frame))
        (alternative (if-frame-alternative frame)))
    (if value
        (evaluate consequent
                  environment
                  context
                  metacontext)
        (evaluate alternative
                  environment
                  context
                  metacontext))))

(define-record-type <let-frame>
  (make-let-frame environment variables expression done todo)
  let-frame?
  (environment let-frame-environment)
  (variables   let-frame-variables)
  (expression  let-frame-expression)
  (done        let-frame-done)
  (todo        let-frame-todo))

(define (let-frame-resume frame context metacontext value)
  (let* ((environment (let-frame-environment frame))
         (variables   (let-frame-variables   frame))
         (expression  (let-frame-expression  frame))
         (todo        (let-frame-todo        frame))
         (done        (let-frame-done        frame))
         (done (cons value done)))
    (if (null? todo)
        (evaluate expression
                  (environment-add* environment variables (reverse done))
                  context
                  metacontext)
        (let ((next (car todo))
              (todo (cdr todo)))
          (evaluate next
                    environment
                    (context-add context
                                 (make-let-frame environment
                                                 variables
                                                 expression
                                                 done
                                                 todo))
                    metacontext)))))

(define-record-type <call-frame>
  (make-call-frame environment done todo)
  call-frame?
  (environment call-frame-environment)
  (done        call-frame-done)
  (todo        call-frame-todo))

(define (call-frame-resume frame context metacontext value)
  (let* ((environment (call-frame-environment frame))
         (todo        (call-frame-todo        frame))
         (done        (call-frame-done        frame))
         (done (cons value done)))
    (if (null? todo)
        (let* ((done (reverse done))
               (function  (car done))
               (arguments (cdr done)))
          (invoke function arguments context metacontext))
        (let ((next (car todo))
              (todo (cdr todo)))
          (evaluate next
                    environment
                    (context-add context
                                 (make-call-frame environment
                                                  done
                                                  todo))
                    metacontext)))))

;;; values

(define-record-type <primitive>
  (make-primitive operation arity meta)
  primitive?
  (operation primitive-operation)
  (arity     primitive-arity)
  (meta      primitive-meta))

(define-record-type <foreign>
  (make-foreign operation arity meta)
  foreign?
  (operation foreign-operation)
  (arity     foreign-arity)
  (meta      foreign-meta))

(define-record-type <continuation>
  (make-continuation context meta)
  continuation?
  (context continuation-context)
  (meta    continuation-meta))

(define-record-type <closure>
  (make-closure environment variables expression meta)
  closure?
  (environment closure-environment)
  (variables   closure-variables)
  (expression  closure-expression)
  (meta        closure-meta))

;;; interpreter

(define (invoke function arguments context metacontext)
  (cond ((primitive? function)
         (let ((operation (primitive-operation function))
               (arity     (primitive-arity     function)))
           (unless (= arity (length arguments))
             (error "invoke: arity error" arity arguments))
           (operation context metacontext arguments)))

        ((foreign? function)
         (let ((operation (foreign-operation function))
               (arity     (foreign-arity     function)))
           (unless (= arity (length arguments))
             (error "invoke: arity error" arity arguments))
           (context-resume context metacontext (operation arguments))))

        ((continuation? function)
         (let ((captured-context (continuation-context function)))
           (unless (= 1 (length arguments))
             (error "invoke: arity error" arity arguments))
           (context-resume captured-context metacontext (car arguments))))

        ((closure? function)
         (let* ((environment (closure-environment function))
                (variables   (closure-variables   function))
                (expression  (closure-expression  function))
                (environment (environment-add* environment variables arguments)))
           (unless (= (length variables) (length arguments))
             (error "invoke: arity error" variables arguments))
           (evaluate expression environment context metacontext)))))

(define (evaluate expression environment context metacontext)
  (cond ((literal? expression)
         (context-resume context metacontext expression))

        ((quote? expression)
         (context-resume context metacontext (quote-expression expression)))

        ((variable? expression)
         (context-resume context
                         metacontext
                         (environment-get environment expression)))

        ((lambda? expression)
         (context-resume context
                         metacontext
                         (make-closure environment
                                       (lambda-variables  expression)
                                       (lambda-expression expression)
                                       expression)))

        ((begin? expression)
         (let* ((expressions (begin-expressions expression))
                (expression  (car expressions))
                (expressions (cdr expressions)))
           (evaluate expression
                     environment
                     (context-add context
                                  (make-begin-frame environment
                                                    expressions))
                     metacontext)))

        ((if? expression)
         (let ((condition   (if-condition expression))
               (consequent  (if-consequent expression))
               (alternative (if-alternative expression)))
           (evaluate condition
                     environment
                     (context-add context
                                  (make-if-frame environment
                                                 consequent
                                                 alternative))
                     metacontext)))

        ((let? expression)
         (let* ((variables       (let-variables       expression))
                (initializations (let-initializations expression))
                (initialization  (car initializations))
                (initializations (cdr initializations))
                (expression      (let-expression      expression)))
           (evaluate initialization
                     environment
                     (context-add context
                                  (make-let-frame environment
                                                  variables
                                                  expression
                                                  '()
                                                  initializations))
                     metacontext)))

        ((reset? expression)
         (let* ((expression  (reset-expression expression))
                (metacontext (metacontext-add metacontext context)))
            (evaluate expression environment empty-context metacontext)))

        ((shift? expression)
         (let* ((variable    (shift-variable expression))
                (expression*  (shift-expression expression))
                (environment (environment-add environment
                                              variable
                                              (make-continuation context expression))))
            (evaluate expression* environment empty-context metacontext)))

        ((call? expression)
         (let ((function  (call-function expression))
               (arguments (call-function expression)))
            (evaluate function
                      environment
                      (context-add context
                                   (make-call-frame environment
                                                    '()
                                                    (call-arguments expression)))
                      metacontext)))

        (else
         (error "evaluate: invalid expression" expression))))

;;; testing

(define program
  `((lambda (x) x)
    (reset
     ((lambda (x y) y)
      (shift k (lambda (y) y))
      (lambda (x y) x)))))

(define program2
  `(let ((a 12)
         (b ((lambda (x) x) 42)))
     a))

(define result (evaluate (expand program2)
                         empty-environment
                         empty-context
                         empty-metacontext))

(display result)
(newline)
