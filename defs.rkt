#lang racket

(require "utils.rkt")
(require "defdefs.rkt")
(provide (for-syntax counters))

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
  (add-to-prj-summary name init (lambda (c alist) (op c (assoc-value alist 'name)))))

(add-to-pth-counter paths "" (lambda (c path) (path->string path)))
(add-simple-summary paths "" same-part)

(add-to-top-counter tops 0 (lambda (c form) (add1 c)))
(add-simple-summary tops 0 add)

(add-to-top-counter deeps 0 (lambda (c form) (max c (depth form))))
(add-simple-summary deeps 0 max)

(add-to-sub-counter defuns 0 (lambda (acc form)
                               (match-case form
                                 ((`(defun ,name ,args ,body ...))
                                  (cond ((or (pair? args) (equal? args 'nil)) (add1 acc))
                                        (else acc)))
                                 (else acc))))
(add-simple-summary defuns 0 add)

(add-to-sub-counter defmacros 0 (lambda (acc form)
                                  (match-case form
                                    ((`(defmacro ,name ,args ,body ...))
                                     (cond ((pair? args) (add1 acc))
                                           (else acc)))
                                    (else acc))))
(add-simple-summary defmacros 0 add)

(add-to-sub-counter defines 0 (lambda (acc form)
                                (match-case form
                                  ((`(define ,name ,body ...))
                                   (add1 acc))
                                  (else acc))))
(add-simple-summary defines 0 add)

(add-to-sub-counter lambdas 0 (lambda (acc form)
                                (match-case form
                                  ((`(lambda ,args ,body ...))
                                   (add1 acc))
                                  (else acc))))
(add-simple-summary lambdas 0 add)

(add-to-err-counter errors)
(add-simple-summary errors empty append)