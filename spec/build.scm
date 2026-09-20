(define (assert cond msg)
  (if cond
      '()
      (error msg)))

(define (xml-value->string value)
  (cond
    ((string? value) value)
    ((symbol? value) (symbol->string value))
    ((number? value) (number->string value))
    ((boolean? value) (if value "true" "false"))
    (else
     (error "Invalid XML attribute value."))))

(define (convert-keys keys)
  (assert (even? (length keys))
          "XML keys must be key/value pairs.")

  (if (null? keys)
      ""
      (let ((key (car keys))
            (value (cadr keys)))
        (assert (symbol? key)
                "XML attribute name must be a symbol.")

        (string-append
         " "
         (symbol->string key)
         "=\""
         (xml-value->string value)
         "\""
         (convert-keys (cddr keys))))))

(define (data->xml list)
  (define (convert-indent list indent)
    (if (or (symbol? list) (string? list) (number? list))
      (let
        ((ind (make-string (* indent 4) #\space )))
        (string-append ind (xml-value->string list)))
      (let*
        (
          (ind (make-string (* indent 4) #\space ))
          (first (car list))
          (_ (assert (not (list? first)) "Name of an XML-Node can not be a list."))
          (name (symbol->string first))
          (rest (cdr list))
          (has-keys
            (and
              (not (null? rest))
              (pair? (car rest))
              (eq? (car (car rest)) '=)))
          (keys (if has-keys
            (convert-keys (cdr (car rest)))
            ""))
          (children (if has-keys
            (cdr rest)
            rest))
        )

        (string-append
          ind "<" name keys ">"
          (apply string-append (map (lambda (x) (string-append "\n" (convert-indent x (+ indent 1)))) children))
          "\n" ind "</" name ">"
        )
      )
    )
  )
  (convert-indent list 0)
)

(define (parse data)
  (define (find data id)
    (cond
      ((not (pair? data)) #f)
      ((not (pair? (car data))) (find (cdr data) id))
      ((eq? (caar data) id) (cdar data))
      (else (find (cdr data) id))
    )
  )
  (lambda (id)
    (cond
      ((eq? id 'title)   (find data 'title))
      ((eq? id 'authors) (find data 'authors))
      ((eq? id 'version) (find data 'version))
      ((eq? id 'date)    (find data 'date))
      ((eq? id 'section) (find data 'section))
      ((eq? id 'rule)    (find data 'rule))
      (else
        (display (symbol->string id))
        (display " is not supported as a document node type.\n")
        #f)
    )
  )
)

(define (data->typst data)
  (string-append "#align(center)[" (car (data 'title)) "]\n")
)


(define doc-path "spec.scm")
(define in (open-input-file doc-path))
(define data (read in))
(close-input-port in)

(define get-data (parse data))
(display (car (get-data 'title)))
(newline)
(display (data->typst get-data))

(define html-out-path "out/spec.typ")
(if (not (file-exists? "out"))
    (create-directory "out"))
(define html-out (open-output-file html-out-path))
(display (data->xml data) html-out)
(newline html-out)
(close-output-port html-out)
