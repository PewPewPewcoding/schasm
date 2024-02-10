#!/usr/bin/csi -s

(import (scheme base)
        (prefix (scheme process-context) sys:)
        (prefix (shell) sh:))

;;; print help message
(define (help)
  (define msg
    "Usage: meta <command>
Commands:
  [help | h]  - print help
  [clean | c] - clean build directory
  [build | b] - make build directory
  [run | r] name        - run  program with name
  [build-run | br] name - build and run program with name")
  (display msg)
  (newline))

;;; clean build directory
(define (clean)
  (sh:run (rm -rf build)))

;;; make build directory
(define (build)
  (clean)
  (sh:run (mkdir build
           && cp *.egg build/
           && cd build
           && chicken-install -n)))

;;; run program in build directory
(define (run name)
  (sh:run (cd build && ,(string-append "./" name))))

;;; make build directory and run program in build directory
(define (build-run name)
  (build)
  (run name))

;;; exit successfully after operation
(define (ok fn . args)
  (apply fn args)
  (sys:exit))

;;; exit success depends on operation result
(define (try fn . args)
  (sys:exit (apply fn args)))

;;; exit unsuccessfully after operation
(define (err fn . args)
  (apply fn args)
  (sys:exit 1))

;;; dispatch on command
(define (main as)
  (unless (<= 1 (length as))
    (err help))
  (let ((command (string->symbol (list-ref as 0))))
    (case command
      ((help)      (ok help))
      ((h)         (ok help))

      ((clean)     (ok clean))
      ((c)         (ok clean))

      ((build)     (ok build))
      ((b)         (ok build))
      (else
       (if (= 2 (length as))
           (case command
             ((run)       (ok run (list-ref as 1)))
             ((r)         (ok run (list-ref as 1)))

             ((build-run) (ok build-run (list-ref as 1)))
             ((br)        (ok build-run (list-ref as 1)))

             (else        (err help)))
           (err help))))))

(main (list-tail (sys:command-line) 1))
