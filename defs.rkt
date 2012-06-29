#lang racket

(require "utils.rkt")
(require "defdefs.rkt")
(provide (for-syntax counters
                     summary))

(define-counters counters (count-pth count-top count-sub count-err))
(define-counters summary (sum-prj))

(define-counters-adder add-to-counter counters)
(define-counters-adder add-to-summary summary)
(define-counters-adder add-to-pth-counter counters count-pth)
(define-counters-adder add-to-top-counter counters count-top)
(define-counters-adder add-to-sub-counter counters count-sub)
(define-counters-adder add-to-err-counter counters count-err)
(define-counters-adder add-to-prj-summary summary sum-prj)

(define-syntax-rule (add-simple-summary name init op)
  (add-to-prj-summary name init (lambda (c alist) (op c (assoc-value 'name alist)))))

(add-to-pth-counter paths "" (lambda (c path) (path->string path)))
(add-to-prj-summary total 0 (lambda (c alist) (add1 c)))

(add-to-prj-summary fail 0 (lambda (c alist) 
                             (+ c (if (zero? (string-length (assoc-value 'errors alist)))
                                      0 1))))

(add-to-top-counter tops 0 (lambda (c form) (add1 c)))
(add-simple-summary tops 0 +)

(add-to-top-counter deeps 0 (lambda (c form) (max c (depth form))))
(add-simple-summary deeps 0 max)

(add-to-sub-counter defuns 0 (lambda (acc form)
                               (match-case form
                                 ((`(defun ,name ,args ,body ...))
                                  (cond ((or (pair? args) (equal? args 'nil)) (add1 acc))
                                        (else acc)))
                                 (else acc))))
(add-simple-summary defuns 0 +)

(add-to-sub-counter defmacros 0 (lambda (acc form)
                                  (match-case form
                                    ((`(defmacro ,name ,args ,body ...))
                                     (cond ((pair? args) (add1 acc))
                                           (else acc)))
                                    (else acc))))
(add-simple-summary defmacros 0 +)

(add-to-sub-counter defines 0 (lambda (acc form)
                                (match-case form
                                  ((`(define ,name ,body ...))
                                   (add1 acc))
                                  (else acc))))
(add-simple-summary defines 0 +)

(add-to-sub-counter lambdas 0 (lambda (acc form)
                                (match-case form
                                  ((`(lambda ,args ,body ...))
                                   (add1 acc))
                                  (else acc))))
(add-simple-summary lambdas 0 +)

(add-to-sub-counter lets 0 (lambda (acc form)
                             (match form
                               (`(let ,bindings ,body ...) (if (pair? bindings)
                                                               (add1 acc)
                                                               acc))
                               (else (match form
                                       (`(let ,name ,bindings ,body ...) (if (and (atom? name) (pair? bindings))
                                                                             (add1 acc)
                                                                             acc))
                                       (else acc))))))
(add-simple-summary lets 0 +)

(add-to-sub-counter valuesmax 0 (lambda (acc form)
                                  (match form
                                    (`(values ,value ...) (max (length value) acc))
                                    (else acc))))
(add-simple-summary valuesmax 0 max)

(add-to-err-counter errors "" (lambda (acc exn) (format "~a" (exn-message exn))))
