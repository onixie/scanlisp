#lang racket

(require "utils.rkt")

(define (scanlisp (dir #f))
  (define current-dialect (make-parameter empty))
  (define (scan-project (dir #f))
    (collect (result)
             (define lisp-regexp #rx"(?:[.])(rkt|scm|ss|lisp|lisp-expr|asd)$")
             (let* ((dir (expand-user-path (or dir (current-directory))))
                    (dir (and (directory-exists? dir) dir)))
               (for ((path (in-directory dir)))
                 (when (regexp-match? lisp-regexp path)
                   (let-values (((base file base-p) (split-path path)))
                     (parameterize ((current-directory base)
                                    (current-dialect (string->symbol (second (regexp-match lisp-regexp path)))))
                       (result (cons path 
                                     (values->list (scan-file path)))))))))))
  
  (define (scan-file path)
    (collect ((tops 0 add1)
              (deepest 0 max)
              errors)
             (call-with-input-file path
               (lambda (in)
                 (with-handlers ((exn:fail? (lambda (exn) (errors exn))))
                   (do ((form (read-toplevel-form in path) (read-toplevel-form in path)))
                     ((eof-object? form))
                     (let-values (((deep) 
                                   (scan-toplevel-form form)))
                       (deepest deep)
                       (tops))))))))
  
  (define (scan-toplevel-form form)
    (letrec ((d (lambda (l)
                  (cond ((null? l) 1)
                        ((and (not (pair? (car l)))
                              (not (null? (car l))))
                         (d (cdr l)))
                        (else
                         (let ((car-depth (+ 1 (d (car l))))
                               (cdr-depth (d (cdr l))))
                           (cond ((> cdr-depth car-depth) cdr-depth)
                                 (else car-depth)))))))
             (scan-subform (lambda (l)
                             (cond ((and (not (pair? l))
                                         (not (null? l)))
                                    0)
                                   (else
                                    (d l))))))
      (values (scan-subform form))))
  
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