#!/usr/bin/env gsi

(define (run-command cmd)
  (let ((status (shell-command cmd)))
    (if (not (zero? status))
        (begin
          (display "Error: Command failed: ") (display cmd) (newline)
          (exit 1)))))

(run-command "typst compile sonnet.typ")