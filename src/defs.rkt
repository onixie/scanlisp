#lang racket

(require "utils.rkt")
(require "defdefs.rkt")

(provide collect-counters
         summary-counters
         summary-counters-only-project
         summary-counters-only-total
         summary-counters-only-maximum
         summary-counters-only-minimum
         (for-syntax counters total maximum minimum project))

(provide get-value-of
         get-values-of
         get-description-of
         print-value
         print-description)

(define-counters (counters (:with-summary project total maximum minimum))
  (define-file-counter file-path
    ("Path" "" (lambda (cv path) (path->string path))))
  (define-summary-project file-path
    ("Path" (void) (lambda (cv path) (if (void? cv)
                                         path
                                         (string-trim (same-part2 cv path) #rx"[^/]+" #:left? #f)))))
  
  (define-toplevel-counter toplevels 
    ("Top-Levels" 0 (lambda (cv form) (add1 cv))))
  (define-summary-project toplevels 
    ("Top-Levels" 0 +))
  
  (define-toplevel-counter form-depth 
    ("Top-Level Depth" 0 (lambda (cv form) (max cv (depth form)))))
  (define-summary-project form-depth 
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
  (define-summary-project defuns
    ("`defun' Forms" 0 +))
  
  (define-subform-counter defmacros
    ("`defmacro' Forms"
     0 
     (lambda (cv form)
       (match-case form
         ((`(defmacro ,name ,args ,body (... ...)))
          (cond ((pair? args) (add1 cv))
                (else cv)))
         (else cv)))))
  (define-summary-project defmacros
    ("`defmacro' Forms" 0 +))
  
  (define-subform-counter defines
    ("`define' Forms"
     0
     (lambda (cv form)
       (match-case form
         ((`(define ,name ,body (... ...)))
          (add1 cv))
         (else cv)))))
  (define-summary-project defines
    ("`define' Forms" 0 +))
  
  (define-subform-counter lambdas
    ("`lambda' Forms" 
     0
     (lambda (cv form)
       (match-case form
         ((`(lambda ,args ,body (... ...)))
          (add1 cv))
         (else cv)))))
  (define-summary-project lambdas
    ("`lambda' Forms" 0 +))
  
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
  (define-summary-project lets
    ("`let' Forms" 0 +))
  
  (define-subform-counter values-length
    ("`values' Length" 
     0 
     (lambda (cv form)
       (match form
         (`(values ,value (... ...)) (max (length value) cv))
         (else cv)))))
  (define-summary-project values-length
    ("`values' Length" 0 max))
  
  (define-error-counter errors
    ("Error" "" (lambda (cv exn) (format "~a" (exn-message exn)))))
  (define-summary-project errors
    ("Error" "" string-append))
  
  (define-summary-total (total file-path)
    ("Total" 0 (lambda (cv v) (add1 cv))))
  
  (define-summary-total (fail errors)
    ("Fail" 0 (lambda (cv v) (+ cv (if (zero? (string-length v)) 0 1)))))
  
  (define-summary-total (total-toplevels toplevels)
    ("Top-Levels" 0 +))
  
  (define-summary-maximum (max-toplevels toplevels)
    ("Top-Levels" 0 max))
  
  (define-summary-minimum (min-toplevels toplevels)
    ("Top-Levels" +inf.0 (compose inexact->exact min)))
  
  (define-summary-maximum (max-form-depth form-depth)
    ("Top-Level Depth" 0 max))
  
  (define-summary-minimum (min-form-depth form-depth)
    ("Top-Level Depth" +inf.0 (compose inexact->exact min)))
  
  (define-summary-total (total-defuns defuns)
    ("`defun' Forms" 0 +))
  
  (define-summary-maximum (max-defuns defuns)
    ("`defun' Forms" 0 max))
  
  (define-summary-total (total-defmacros defmacros)
    ("`defmacro' Forms" 0 +))
  
  (define-summary-maximum (max-defmacros defmacros)
    ("`defmacro' Forms" 0 max))
  
  (define-summary-total (total-defines defines)
    ("`define' Forms" 0 +))
  
  (define-summary-maximum (max-defines defines)
    ("`define' Forms" 0 max))
  
  (define-summary-total (total-lambdas lambdas)
    ("`lambda' Forms" 0 +))
  
  (define-summary-maximum (max-lambdas lambdas)
    ("`lambda' Forms" 0 max))
  
  (define-summary-total (total-lets lets)
    ("`let' Forms" 0 +))
  
  (define-summary-maximum (max-lets lets)
    ("`let' Forms" 0 max))
  
  (define-summary-maximum (max-values-length values-length)
    ("`values' Length" 0 max))
  
  (define-summary-minimum (min-values-length values-length)
    ("`values' Length" +inf.0 (compose inexact->exact min))))