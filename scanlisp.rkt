#lang racket

(require "utils.rkt")

(define (scanlisp (dir #f))
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
      result))
  
  (define (scan-file path)
    (collect ((tops 0 add1)
              (deeps 0 max)
              (defuns 0 +)
              (defmacros 0 +)
              (defines 0 +)
              (lambdas 0 +)
              errors)
      (call-with-input-file path
        (lambda (in)
          (with-handlers ((exn:fail? (lambda (exn) (errors exn))))
            (do ((form (read-toplevel-form in path) 
                       (read-toplevel-form in path)))
              ((eof-object? form))
              (let ((result (scan-toplevel-form form)))
                (tops)
                (deeps (assoc-value 'deeps result))
                (defuns (assoc-value 'defuns result))
                (defmacros (assoc-value 'defmacros result))
                (defines (assoc-value 'defines result))
                (lambdas (assoc-value 'lambdas result)))))))))
  
  (define (scan-toplevel-form form)
    (define (defuns-add1 acc form)
      (match-case form
        ((`(defun ,name (,args ...) ,body ...)
          `(defun ,name nil ,body ...))
         (add1 acc))
        (else acc)))
    (define (defmacros-add1 acc form)
      (match-case form
        ((`(defmacro ,name (,args ...) ,body ...)
          `(defmacro ,name nil ,body ...))
         (add1 acc))
        (else acc)))
    (define (defines-add1 acc form)
      (match-case form
        ((`(define ,name ,body ...)
          `(define (,name-and-args ...) ,body ...))
         (add1 acc))
        (else acc)))
    (define (lambdas-add1 acc form)
      (match-case form
        ((`(lambda (,args ...) ,body ...)
          `(lambda ,args ,body ...))
         (add1 acc))
        (else acc)))
    (collect ((deeps 0 +)
              (defuns 0 defuns-add1)
              (defmacros 0 defmacros-add1)
              (defines 0 defines-add1)
              (lambdas 0 lambdas-add1)
              (:group (defuns defmacros defines lambdas :as coldefs)))
      (letrec ((d (lambda (l)
                    (cond ((null? l) 1)
                          ((atom? (car l)) (d (cdr l)))
                          (else
                           (coldefs (car l))
                           (let ((cad (+ 1 (d (car l))))
                                 (cdd (d (cdr l))))
                             (cond ((> cdd cad) cdd)
                                   (else cad)))))))
               (scan-subform (lambda (l)
                               (cond ((atom? l) 0)
                                     (else (coldefs (car l))
                                           (d l))))))
        (deeps (scan-subform form)))))
  
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