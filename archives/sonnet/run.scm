#!/usr/bin/env gsi

(import (gambit))
(import (srfi 13))

(define (run-command cmd)
  (let ((status (shell-command cmd)))
    (if (not (zero? status))
        (begin
          (display "Error: Command failed: ") (display cmd) (newline)
          (exit 1)))))

(define (clean) (run-command "rm -rf build"))

(define (build)
  (run-command "mkdir -p build")
  (run-command "gsi pre.scm")
  (run-command "gcc -o build/sonnet build/sonnet.c -Wall -Wextra -Wswitch -Wshadow -Wfloat-equal -Wpointer-arith -Wshadow -Wcast-align -Wunreachable-code"))

(define (run args)
  (let ((cmd (string-append "./build/sonnet " (string-join args " "))))
    (run-command cmd)))

(define (bad) 
  (display "Supposed Usage: ./run.scm [clean|build|run <filename>]") 
  (newline) 
  (exit 1))

(let* ((full-args (command-line))
       (args (if (>= (length full-args) 2) (cdr full-args) '())))
  (cond
    ((null? args) (bad))
    ((string=? (car args) "clean") (clean))
    ((string=? (car args) "build") (clean) (build))
    ((string=? (car args) "run")   (clean) (build) (run (cdr args)))
    (else (bad))))