(import (scheme base)
        (scheme write)
        (srfi 1)
        (schasm expression)
        (schasm expand)
        (micro-benchmark))

;;; environment

(define (empty-environment variable)
  (error "environment: variable not found" variable))

(define (environment-add environment variable value)
  (lambda (variable*)
    (if (equal? variable* variable) value (environment variable*))))

(define (environment-add* environment variables values)
  (unless (= (length variables) (length values))
    (error "environment-add*: not same length" variables values))
  (fold (lambda (variable value environment)
          (environment-add environment variable value))
        environment
        variables
        values))

;;; store

(define start-address 0)

(define (start-search address)
  (error "store: unallocated memory" address))

(define (search-add search address value)
  (lambda (address*)
    (if (equal? address* address) value (search address*))))

(define (make-store address search)
  (lambda (continuation)
    (continuation address search)))

(define empty-store (make-store start-address start-search))

(define (store-get store address)
  (store (lambda (_ search) (search address))))

(define (store-update store address value)
  (store
   (lambda (address* search)
     (make-store address* (search-add search address value)))))

(define (store-allocate store value continuation)
  (store (lambda (address search)
           (continuation (make-store (+ 1 address)
                                     (search-add search address value))
                         address))))

(define (store-allocate* store values continuation)
  (if (null? values)
      (continuation store '())
      (let ((value  (list-ref  values 0))
            (values (list-tail values 1)))
        (store-allocate
         store value
         (lambda (store* value*)
           (store-allocate*
            store* values
            (lambda (store** values*)
              (continuation store** (cons value* values*)))))))))

;;; context

(define (empty-context metacontext store value)
  (metacontext store value))

;;; metacontext

(define (empty-metacontext store value)
  value)

;;; interpreter

(define (evaluate expression environment store context metacontext)
  (cond ((literal? expression)
         (context metacontext
                  store
                  expression))

        ((variable? expression)
         (context metacontext
                  store
                  (store-get store (environment expression))))

        ((lambda? expression)
         (let ((variables  (lambda-variables expression))
               (expression (lambda-expression expression)))
           (context metacontext
                    store
                    (lambda (metacontext* context* store* arguments)
                      (unless (= (length variables) (length arguments))
                        (error "Arity error" variables arguments))
                      (store-allocate*
                       store* arguments
                       (lambda (store** addresses)
                         (evaluate expression
                                   (environment-add* environment
                                                     variables
                                                     addresses)
                                   store**
                                   context*
                                   metacontext*)))))))

        ((begin? expression)
         (let* ((expressions (begin-expressions expression)))
           (evaluate* expressions
                      environment
                      store
                      (lambda (metacontext* store* values)
                        (context metacontext* store* (last values)))
                      metacontext)))

        ((if? expression)
         (let ((condition   (if-condition expression))
               (consequent  (if-consequent expression))
               (alternative (if-alternative expression)))
           (evaluate condition
                     environment
                     store
                     (lambda (metacontext* store* value)
                       (evaluate (if value consequent alternative)
                                 environment
                                 store*
                                 context
                                 metacontext*))
                     metacontext)))

        ((let? expression)
         (let* ((variables       (let-variables       expression))
                (initializations (let-initializations expression))
                (expression      (let-expression      expression)))
           (evaluate* initializations
                      environment
                      store
                      (lambda (metacontext* store* values)
                        (store-allocate*
                         store* values
                         (lambda (store** addresses)
                           (evaluate expression
                                     (environment-add* environment
                                                       variables
                                                       addresses)
                                     store**
                                     context
                                     metacontext*))))
                      metacontext)))

        ((reset? expression)
         (evaluate (reset-expression expression)
                   environment
                   store
                   empty-context
                   (lambda (store* value)
                     (context metacontext store* value))))

        ((shift? expression)
         (evaluate (shift-expression expression)
                   (environment-add environment
                                    (shift-variable expression)
                                    (lambda (metacontext* context* store* arguments)
                                      (unless (= 1 (length arguments))
                                        (error "Arity error" 1 arguments))
                                      (context (lambda (store** value)
                                                 (context* metacontext* store** value))
                                               store*
                                               (list-ref arguments 0))))
                   store
                   empty-context
                   metacontext))

        ((set!? expression)
         (let* ((variable   (set!-variable   expression))
                (expression (set!-expression expression))
                (address (environment variable)))
           (evaluate expression
                     environment
                     store
                     (lambda (metacontext* store* value)
                       (context metacontext*
                                (store-update store* address value)
                                value))
                     metacontext)))

        ((call? expression)
         (evaluate* expression
                    environment
                    store
                    (lambda (metacontext* store* values)
                      (let ((function  (list-ref  values 0))
                            (arguments (list-tail values 1)))
                        (function metacontext* context store* arguments)))
                    metacontext))

        (else
         (error "evaluate: invalid expression" expression))))

(define (evaluate* expressions environment store context metacontext)
  (if (null? expressions)
      (context metacontext store '())
      (let ((expression  (list-ref  expressions 0))
            (expressions (list-tail expressions 1)))
        (evaluate expression
                  environment
                  store
                  (lambda (metacontext* store* value)
                    (evaluate* expressions
                               environment
                               store*
                               (lambda (metacontext** store** values)
                                 (context metacontext** store** (cons value values)))
                               metacontext*))
                  metacontext))))

;;; testing

(define (procedure->value f)
  (lambda (metacontext context store arguments)
    (context metacontext store (apply f arguments))))

(define (values->environment+store environment store variables values continuation)
  (store-allocate*
   store values
   (lambda (store* addresses)
     (continuation (environment-add* environment variables addresses)
                   store*))))

(define (println . objs)
  (for-each display objs)
  (newline))

(define fib-test
  `(let ((fib #f))
     (begin
       (set! fib
             (lambda (x)
               (reset
                (let ((test #f)
                      (x-1 #f)
                      (x-2 #f))
                  (begin
                    (set! test (< x 2))
                    (set! x-1 (- x 1))
                    (set! x-2 (- x 2))
                    (if test
                        (shift k x)
                        (let ((left  #f)
                              (right #f))
                          (begin
                            (set! left  (fib x-1))
                            (set! right (fib x-2))
                            (shift k (+ left right))))))))))
       (fib 5))))

(define-values (env store)
  (values->environment+store
   empty-environment empty-store
   `(< + -)
   (map procedure->value `(,< ,+ ,-))
   values))

(define (run)
  (evaluate fib-test
            env
            store
            empty-context
            empty-metacontext))

(println (run))
(for-each println (benchmark-run (run)))
