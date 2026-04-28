(import (gambit))

(define TEMP_DIR ".temp/")

(if (not (file-exists? TEMP_DIR)) (create-directory TEMP_DIR))

(define (format-c-string code-str)
  (let ((tmp-in (string-append TEMP_DIR "tmp_in.c")) 
        (tmp-out (string-append TEMP_DIR "tmp_out.c")))
    
    (call-with-output-file tmp-in (lambda (p) (display code-str p)))
    
    (shell-command (string-append "clang-format " tmp-in " > " tmp-out))
    
    (let ((result (call-with-input-file tmp-out (lambda (p) (read-line p #f)))))
      (delete-file tmp-in)
      (delete-file tmp-out)
      result)))

(define (process-files input-path)
  (let* ((in (open-input-file input-path))
         ;; Output paths
         (tmp-typ-path (string-append TEMP_DIR "temp_sonnet.typ"))
         (out-typ (open-output-file tmp-typ-path))
         (out-c (open-output-file "build/sonnet.c")))
    
    (let loop ((line (read-line in)) (capturing? #f) (buffer ""))
      (cond
        ((eof-object? line) 
         (close-input-port in) 
         (close-output-port out-typ) 
         (close-output-port out-c)
         (shell-command (string-append "mv " tmp-typ-path " sonnet.typ")))
        
        ((string-contains line "```c")
         (display line out-typ) (newline out-typ)
         (loop (read-line in) #t ""))
        
        ((and capturing? (string-contains line "```"))
         (let ((formatted (format-c-string buffer)))
           (display formatted out-typ)
           (display line out-typ) (newline out-typ)
           (display formatted out-c)
           (loop (read-line in) #f "")))
        
        (capturing?
         (loop (read-line in) #t (string-append buffer line "\n")))
        
        (else
         (display line out-typ) (newline out-typ)
         (loop (read-line in) #f ""))))))

(define (string-contains str sub)
  (let ((len-str (string-length str)) (len-sub (string-length sub)))
    (let loop ((i 0))
      (if (> (+ i len-sub) len-str) #f
          (if (string=? (substring str i (+ i len-sub)) sub) #t (loop (+ i 1)))))))

(if (not (file-exists? "build")) (create-directory "build"))

(process-files "sonnet.typ")