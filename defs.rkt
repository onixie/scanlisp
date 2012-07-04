#lang racket

(require "utils.rkt")
(require "defdefs.rkt")
(provide collect-counters
         summary-counters
         (for-syntax
          counters
          summary
          summary-max
          summary-min)
         get-value
         get-description
         print-value
         print-description)

(define-counters (counters (:with-summary summary summary-max summary-min))
  (define-file-counter paths
    ("File" "" (lambda (cv path) (path->string path))))
  
  (define-toplevel-counter tops 
    ("Top-Levels" 0 (lambda (cv form) (add1 cv))))
  
  (define-toplevel-counter deeps 
    ("Top-Level Depth" 0 (lambda (cv form) (max cv (depth form)))))
  
  (define-summary (total  paths)
    ("Total" 0 (lambda (cv v) (add1 cv))))
  
  (define-summary (fail errors)
    ("Fail" 0 (lambda (cv v) (+ cv (if (zero? (string-length v)) 0 1)))))
  
  (define-summary tops
    ("Top-Levels" 0 +))
  
  (define-summary-max tops
    ("Top-Levels" 0 max))
  
  (define-summary-min tops
    ("Top-Levels" +inf.0 min))
  
  (define-summary-max deeps
    ("Top-Level Depth" 0 max))
  
  (define-subform-counter defuns 
    ("`defun' Forms" 
     0 
     (lambda (cv form)
       (match-case form
         ((`(defun ,name ,args ,body (... ...)))
          (cond ((or (pair? args) (equal? args 'nil)) (add1 cv))
                (else cv)))
         (else cv)))))
  
  (define-summary defuns
    ("`defun' Forms" 0 +))
  
  (define-summary-max defuns
    ("`defun' Forms" 0 max))
    
  (define-subform-counter defmacros 
    ("`defmacro' Forms"
     0 
     (lambda (cv form)
       (match-case form
         ((`(defmacro ,name ,args ,body (... ...)))
          (cond ((pair? args) (add1 cv))
                (else cv)))
         (else cv)))))
  
  (define-summary defmacros 
    ("`defmacro' Forms" 0 +))
    
  (define-summary-max defmacros 
    ("`defmacro' Forms" 0 max))
  
  (define-subform-counter defines 
    ("`define' Forms"
     0
     (lambda (cv form)
       (match-case form
         ((`(define ,name ,body (... ...)))
          (add1 cv))
         (else cv)))))
  
  (define-summary defines 
    ("`define' Forms" 0 +))
  
  (define-summary-max defines 
    ("`define' Forms" 0 max))
  
  (define-subform-counter lambdas
    ("`lambda' Forms" 
     0
     (lambda (cv form)
       (match-case form
         ((`(lambda ,args ,body (... ...)))
          (add1 cv))
         (else cv)))))
  
  (define-summary lambdas
    ("`lambda' Forms" 0 +))
  
  (define-summary-max lambdas
    ("`lambda' Forms" 0 max))
  
  (define-subform-counter lets
    ("`let' Forms" 
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
  
  (define-summary lets
    ("`let' Forms" 0 +))
  
  (define-summary-max lets
    ("`let' Forms" 0 max))
  
  (define-subform-counter valuesmax 
    ("`values' Length" 
     0 
     (lambda (cv form)
       (match form
         (`(values ,value (... ...)) (max (length value) cv))
         (else cv)))))
  
  (define-summary-max valuesmax 
    ("`values' Length" 0 max))
  
  (define-error-counter errors 
    ("Error" "" (lambda (cv exn) (format "~a" (exn-message exn))))))