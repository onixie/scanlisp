#lang racket

(require "utils.rkt")
(require "defdefs.rkt")
(provide (for-syntax counters
                     sum-counters))

(in-defdefs (counters sum-counters)
  (define-file-counter paths
    ("File" "" (lambda (cv path) (path->string path))))
  
  (define-summary-counter (total  paths)
    ("Total Scan" 0 (lambda (cv v) (add1 cv))))
  
  (define-summary-counter (fail errors)
    ("Fail" 0 (lambda (cv v) (+ cv (if (zero? (string-length v)) 0 1)))))
  
  (define-toplevel-counter tops 
    ("Count of Top-Level Forms" 0 (lambda (cv form) (add1 cv))))
  
  (define-summary-counter tops
    ("Count of Top-Level Forms" 0 +))
  
  (define-toplevel-counter deeps 
    ("Max Depth of Top-Level Forms" 0 (lambda (cv form) (max cv (depth form)))))
  
  (define-summary-counter deeps 
    ("Max Depth of Top-Level Forms" 0 max))
  
  (define-subform-counter defuns 
    ("Count of `defun' Forms" 
     0 
     (lambda (cv form)
       (match-case form
         ((`(defun ,name ,args ,body (... ...)))
          (cond ((or (pair? args) (equal? args 'nil)) (add1 cv))
                (else cv)))
         (else cv)))))
  
  (define-summary-counter defuns
    ("Count of `defun' Forms" 0 +))
  
  (define-subform-counter defmacros 
    ("Count of `defmacro' Forms"
     0 
     (lambda (cv form)
       (match-case form
         ((`(defmacro ,name ,args ,body (... ...)))
          (cond ((pair? args) (add1 cv))
                (else cv)))
         (else cv)))))
  
  (define-summary-counter defmacros 
    ("Count of `defmacro' Forms" 0 +))
  
  (define-subform-counter defines 
    ("Count of `define' Forms"
     0
     (lambda (cv form)
       (match-case form
         ((`(define ,name ,body (... ...)))
          (add1 cv))
         (else cv)))))
  (define-summary-counter defines 
    ("Count of `define' Forms" 0 +))
  
  (define-subform-counter lambdas
    ("Count of `lambda' Forms" 
     0
     (lambda (cv form)
       (match-case form
         ((`(lambda ,args ,body (... ...)))
          (add1 cv))
         (else cv)))))
  
  (define-summary-counter lambdas
    ("Count of `lambda' Forms" 0 +))
  
  (define-subform-counter lets
    ("Count of `let' Forms" 
     0
     (lambda (cv form)
       (match form
         (`(let ,bindings ,body (... ...)) 
          (if (pair? bindings)
              (add1 cv)
              cv))
         (else (match form
                 (`(let ,name ,bindings ,body (... ...)) 
                  (if (and (atom? name) (pair? bindings))
                      (add1 cv)
                      cv))
                 (else cv)))))))
  
  (define-summary-counter lets
    ("Count of `let' Forms" 0 +))
  
  (define-subform-counter valuesmax 
    ("Max Length of `values' Form" 
     0 
     (lambda (cv form)
       (match form
         (`(values ,value (... ...)) (max (length value) cv))
         (else cv)))))
  
  (define-summary-counter valuesmax 
    ("Max Length of `values' Form" 0 max))
  
  (define-error-counter errors 
    ("Error" "" (lambda (cv exn) (format "~a" (exn-message exn))))))