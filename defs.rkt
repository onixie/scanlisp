#lang racket

(require "utils.rkt")
(require "defdefs.rkt")
(provide (for-syntax counters
                     sum-counters))

(define-counters counters (count-pth count-top count-sub count-err))
(define-counters sum-counters (sum-prj))

(define-define-counter define-file-counter counters count-pth)
(define-define-counter define-toplevel-counter counters count-top)
(define-define-counter define-subform-counter counters count-sub)
(define-define-counter define-error-counter counters count-err)
(define-define-counter define-summary-counter sum-counters sum-prj)

(define-syntax define-simple-summary-counter
  (syntax-rules ()
    ((define-simple-summary-counter (name fname) (init op))
     (define-summary-counter name (init (lambda (c alist) (op c (assoc-value 'fname alist))))))
    ((define-simple-summary-counter name (init op))
     (define-simple-summary-counter (name name) (init op)))
    ((define-simple-summary-counter (name) (init op))
     (define-simple-summary-counter (name name) (init op)))))

(define-file-counter paths 
  ("" (lambda (c path) (path->string path))))

(define-summary-counter total 
  (0 (lambda (c alist) (add1 c))))

(define-simple-summary-counter (fail errors) 
  (0 (lambda (c v) (+ c (if (zero? (string-length v)) 0 1)))))

(define-toplevel-counter tops 
  (0 (lambda (c form) (add1 c))))

(define-simple-summary-counter tops 
  (0 +))

(define-toplevel-counter deeps 
  (0 (lambda (c form) (max c (depth form)))))

(define-simple-summary-counter deeps 
  (0 max))

(define-subform-counter defuns 
  (0 (lambda (acc form)
       (match-case form
         ((`(defun ,name ,args ,body (... ...)))
          (cond ((or (pair? args) (equal? args 'nil)) (add1 acc))
                (else acc)))
         (else acc)))))

(define-simple-summary-counter defuns
  (0 +))

(define-subform-counter defmacros 
  (0 (lambda (acc form)
       (match-case form
         ((`(defmacro ,name ,args ,body (... ...)))
          (cond ((pair? args) (add1 acc))
                (else acc)))
         (else acc)))))

(define-simple-summary-counter defmacros 
  (0 +))

(define-subform-counter defines 
  (0 (lambda (acc form)
       (match-case form
         ((`(define ,name ,body (... ...)))
          (add1 acc))
         (else acc)))))
(define-simple-summary-counter defines 
  (0 +))

(define-subform-counter lambdas
  (0 (lambda (acc form)
       (match-case form
         ((`(lambda ,args ,body (... ...)))
          (add1 acc))
         (else acc)))))

(define-simple-summary-counter lambdas
  (0 +))

(define-subform-counter lets
  (0 (lambda (acc form)
       (match form
         (`(let ,bindings ,body (... ...)) (if (pair? bindings)
                                         (add1 acc)
                                         acc))
         (else (match form
                 (`(let ,name ,bindings ,body (... ...)) (if (and (atom? name) (pair? bindings))
                                                       (add1 acc)
                                                       acc))
                 (else acc)))))))

(define-simple-summary-counter lets
  (0 +))

(define-subform-counter valuesmax 
  (0 (lambda (acc form)
       (match form
         (`(values ,value (... ...)) (max (length value) acc))
         (else acc)))))

(define-simple-summary-counter valuesmax 
  (0 max))

(define-error-counter errors 
  ("" (lambda (acc exn) (format "~a" (exn-message exn)))))
