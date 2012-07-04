#lang racket

(require "utils.rkt")
(require "defs.rkt")
(require "report.rkt")

(define (scanlisp (dir #f) #:report (report (lambda (prj sum det) det)) #:summary (summary (lambda (det) null)) #:row-sort (row-sort identity) #:column-sort (column-sort identity) #:column-filter (column-filter identity))
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
        (let ((summary (row-sort (column-sort (column-filter (summary details)))))
              (details (row-sort (column-sort (column-filter details)))))
          (report dir summary details)))))
  
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
    
(define (scanlisp.html (dir #f) #:row-sort (row-sort identity) #:column-sort (column-sort identity) #:column-filter (column-filter identity) #:rich (rich #t))
  (scanlisp dir #:report (if rich pretty-html-reporter simple-html-reporter) #:summary (lambda (d) (summary-counters d)) #:row-sort row-sort #:column-sort column-sort #:column-filter column-filter))

(define-syntax (scanlisp.summary stx)
  (syntax-case stx ()
    ((_ details (summaries ...))
     (with-syntax ((summary-details (unhygienize 'summary-details #'details)))
       #'(values
          (generate-collect summaries (:into-alist)
            (for ((detail (in-list details)))
              (summary-details detail))) ...)))))

(define-syntax scanlisp.hist
  (syntax-rules ()
    ((scanlisp.hist details ((name title (low high step)) ...))
     (let ((ds details))
       (values 
        (let ((hist #f)
              (l #f)
              (h #f)
              (s #f))
          (collect ((l +inf.0 min) (h 0 max) (:group (l h :as name)) (:into-values l h))
            (for ((detail (in-list ds)))
              (name (get-value 'name detail))))
          (let* ((l (round (inexact->exact (or low l 0))))
                 (h (round (inexact->exact (or high h 500))))
                 (s (round (inexact->exact (or step (expt (abs (- h l)) 1/3)))))
                 (s (if (zero? s) 1 s)))
            (collect ((name (make-hist l h s) (lambda (cv v) (class-hist cv v))) (:into-values hist))
              (for ((detail (in-list ds)))
                (name (get-value 'name detail))))
            (plot-hist hist #:title title #:width-ratio (/ (- h l) (* s 3/4) 10.0) #:x-label (get-description 'name (car ds)) #:y-label ""))) ...)))
    ((scanlisp.hist details ((name) ...))
     (scanlisp.hist details ((name "" (#f #f #f)) ...)))
    ((scanlisp.hist details ((name title) ...))
     (scanlisp.hist details ((name title (#f #f #f)) ...)))
    ((scanlisp.hist details ((name title (low)) ...))
     (scanlisp.hist details ((name title (low #f #f)) ...)))
    ((scanlisp.hist details ((name title (low high)) ...))
     (scanlisp.hist details ((name title (low high #f)) ...)))    
    ((scanlisp.hist details (name ...))
     (scanlisp.hist details ((name "") ...)))))