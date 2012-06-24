#lang racket

(require "utils.rkt")
(require "defs.rkt")

(require xml)
(require net/sendurl)

(define (scanlisp (dir #f))
  (define report
    (lambda (results (sort? #t))
      (xexpr->string
       `(html
         (body
          (h1 "ScanLisp Report")
          (hr)
          (table
           ,@(let/cc ret
               (let loop ((row (car results))
                          (rest (cdr results))
                          (lines null))
                 (define (tr-format (type cdr))
                   `(tr ,@(map (lambda (p)
                                 `(td ,(format "~a" (type p))))
                               (if sort?
                                   (sort row string>?
                                         #:key (lambda (item)
                                                 (symbol->string (car item))))
                                   row))))
                 (let ((tr (tr-format)))
                   (when (null? rest)
                     (ret (cons (tr-format car) (cons tr lines))))
                   (loop (car rest)
                         (cdr rest) 
                         (cons tr lines)))))))))))
  (define current-dialect (make-parameter empty))
  (define (scan-project (dir #f))
    (define lisp-regexp #rx"(?:[.])(rkt|scm|ss|lisp|lisp-expr|asd)$")
    (let ((dir (expand-user-path (or dir (current-directory))))
          (result null))
      (unless (directory-exists? dir)
        (error "Project directory inexists."))
      (for ((path (in-directory dir)))
        (when (regexp-match? lisp-regexp path)
          (let-values (((base file base-p) (split-path path)))
            (parameterize 
                ((current-directory base)
                 (current-dialect (string->symbol 
                                   (second (regexp-match lisp-regexp 
                                                         path)))))
              (set! result (cons (cons (cons 'path path) 
                                       (scan-file path)) 
                                 result))))))
      (send-url/contents (report result #f))))
  
  (define (scan-file path)
    (generate-collect counter
      (define (scan-form form)
        (cond ((not (pair? form)) form)
              ((pair? (car form))
               (sub-count (car form))
               (scan-form (car form))
               (scan-form (cdr form)))
              (else
               (scan-form (cdr form)))))
      (call-with-input-file path
        (lambda (in)
          (with-handlers ((exn:fail? (lambda (exn) (error-count exn))))
            (do ((form (read-toplevel-form in path)
                       (read-toplevel-form in path)))
              ((eof-object? form))
              (top-count form)
              (sub-count form)
              (scan-form form)))))))
  
  (define (read-toplevel-form in dialect)
    (case (current-dialect)
      ((lisp lisp-expr asd)
       (parameterize ((current-readtable 
                       (make-readtable #f 
                                       #\# #\ #f
                                       #\. #\ #f
                                       #\' #\ #f
                                       #\` #\ #f
                                       #\, #\ #f
                                       #\" 'terminating-macro read-unescape-string)))
         (read in)))
      ((rkt scm ss)
       (parameterize ((read-accept-lang #t)
                      (read-accept-reader #t))
         (read in)))
      (else
       (error "Unsupport dialect" (current-dialect)))))
  
  (define read-unescape-string
    (case-lambda
      ((ch in)
       (define string-regexp #rx"(\\\\.|[^\\\"])*\"")
       (let* ((str (bytes->string/utf-8 (car (regexp-match string-regexp in))))
              (len (string-length str)))
         (substring str 0 (- len 1))))
      ((ch in src line col pos)
       (datum->syntax #f (read-unescape-string ch in)))))
  
  (scan-project dir))