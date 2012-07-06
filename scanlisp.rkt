#lang racket

(require "utils.rkt")
(require "defs.rkt")
(require "report.rkt")

(provide scanlisp
         (rename-out (scanlisp.summary summary)
                     (scanlisp.summarize summarize)
                     (scanlisp.html report)
                     (scanlisp.compare compare)
                     (scanlisp.histogram histogram)))

(define (scanlisp (dir #f))
  (define current-dialect (make-parameter empty))
  (define lisp-regexp #rx"(?:[.])(rkt|scm|ss|lisp|lisp-expr|asd)$")
  (define (dialect-type path) (string->symbol (second (regexp-match lisp-regexp path))))
  
  (define (scan-project (dir #f))
    (let ((dir (expand-user-path (or dir (current-directory)))))
      (unless (directory-exists? dir)
        (error "Project directory inexists."))
      (let ((details null) (ths null))
        (collect (details (:into-values details))
          (collect (ths (:into-values ths))
            (for ((path (in-directory dir)))
              (when (regexp-match? lisp-regexp path)
                (let-values (((base file base-p) (split-path path)))
                  (parameterize ((current-directory base)
                                 (current-dialect (dialect-type path)))
                    (ths (thread (lambda ()
                                   (let ((r (scan-file path)))
                                     (details r))))))))))
          (thread-wait-all ths))
        details)))
  
  (define (scan-file path)
    (collect-counters ()
      (letrec ((scan-form (lambda (form)
                            (cond ((not (pair? form)) form)
                                  ((pair? (car form))
                                   (count-subforms (car form))
                                   (scan-form (car form))
                                   (scan-form (cdr form)))
                                  (else
                                   (scan-form (cdr form)))))))
        (count-files path)
        (call-with-input-file path
          (lambda (in)
            (with-handlers ((exn:fail? (lambda (exn) (count-errors exn))))
              (do ((form (read-toplevel-form in path)
                         (read-toplevel-form in path)))
                ((eof-object? form))
                (when (pair? form)
                  (count-toplevels form)
                  (count-subforms form)
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

(define (scanlisp.summarize (details (scanlisp)) #:by (by '()))
  (list->values
   (map (lambda (arg)
          (let ((name (first arg))
                (op (second arg)))
            (apply op (get-values-of name details))))
        by)))

(define (scanlisp.histogram (details (scanlisp)) #:by (by '()) #:compact (compact 'none))
  (scanlisp.summarize details 
                      #:by (map (lambda (by) 
                                  (list by (histogram-plotter #:x-label (get-description-of by (car details)) #:compact compact)))
                                by)))

(define (scanlisp.summary (details (scanlisp)))
  (summary-counters-only-project details))

(define (scanlisp.html (details (scanlisp)) #:row-sort (row-sort identity) #:column-sort (column-sort identity) #:column-filter (column-filter identity) #:rich (rich #t))
  (let ((summary (row-sort (column-sort (column-filter (summary-counters details)))))
        (details (row-sort (column-sort (column-filter details)))))
    ((if rich pretty-html-reporter simple-html-reporter) 
     (get-value-of 'file-path (car summary))
     summary 
     details)
    (void)))

(define (scanlisp.compare . args)
  (map scanlisp.summary args))