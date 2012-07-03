#lang racket

(require "utils.rkt")
(require "defs.rkt")
(require "report.rkt")

(define (scanlisp (dir #f) #:report (report identity) #:summary? (summary? #f) #:row-sort (row-sort identity) #:column-sort (column-sort identity))
  (define current-dialect (make-parameter empty))
  (define lisp-regexp #rx"(?:[.])(rkt|scm|ss|lisp|lisp-expr|asd)$")
  (define (dialect-type path) (string->symbol (second (regexp-match lisp-regexp path))))
  
  (define (scan-project (dir #f))
    (let ((dir (expand-user-path (or dir (current-directory)))))
      (unless (directory-exists? dir)
        (error "Project directory inexists."))
      (let ((details null) (summary null) (ths null))
        (generate-collect sum-counters (:into-alist summary)
          (collect (details (:into-values details))
            (collect (ths (:into-values ths))
              (for ((path (in-directory dir)))
                (when (regexp-match? lisp-regexp path)
                  (let-values (((base file base-p) (split-path path)))
                    (parameterize ((current-directory base)
                                   (current-dialect (dialect-type path)))
                      (ths (thread (lambda ()
                                     (let ((r (scan-file path)))
                                       (sum-prj r)
                                       (details r))))))))))
            (thread-wait-all ths)))
        (report (list (row-sort (column-sort details))
                      (and summary? summary)
                      (and summary? dir))))))
  
  (define (scan-file path)
    (generate-collect counters ()
      (letrec ((scan-form (lambda (form)
                            (cond ((not (pair? form)) form)
                                  ((pair? (car form))
                                   (count-sub (car form))
                                   (scan-form (car form))
                                   (scan-form (cdr form)))
                                  (else
                                   (scan-form (cdr form)))))))
        (count-pth path)
        (call-with-input-file path
          (lambda (in)
            (with-handlers ((exn:fail? (lambda (exn) (count-err exn))))
              (do ((form (read-toplevel-form in path)
                         (read-toplevel-form in path)))
                ((eof-object? form))
                (when (pair? form)
                  (count-top form)
                  (count-sub form)
                  (scan-form form)))))))))
  
  (define (read-toplevel-form in dialect)
    (case (current-dialect)
      ((lisp lisp-expr asd)
       (parameterize ((current-readtable (make-readtable #f 
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

(define (scanlisp.html (dir #f) #:row-sort (row-sort identity) #:column-sort (column-sort identity) #:rich (rich #t))
  (scanlisp dir #:report (if rich pretty-html-reporter simple-html-reporter) #:summary? #t #:row-sort row-sort #:column-sort column-sort))