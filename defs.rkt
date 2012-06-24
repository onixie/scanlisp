#lang racket

(require "utils.rkt")
(provide (for-syntax counter))

(define-collect counter
  ()
  (:group (:as top-count)
          (:as sub-count)
          (:as error-count)))

(add-collect! counter
  ((tops 0 (lambda (acc form)
             (add1 acc)))
   (deeps 0 (lambda (acc form)
              (max acc (depth form)))))
  (:group (tops deeps :into top-count)))

(add-collect! counter
  ((defuns 0 (lambda (acc form)
               (match-case form
                 ((`(defun ,name ,args ,body ...))
                  (if (or (pair? args) (equal? args 'nil))
                      (add1 acc)
                      acc))
                 (else acc))))
   (defmacros 0 (lambda (acc form)
                  (match-case form
                    ((`(defmacro ,name ,args ,body ...))
                     (if (pair? args)
                         (add1 acc)
                         acc))
                    (else acc))))
   (defines 0 (lambda (acc form)
                (match-case form
                  ((`(define ,name ,body ...))
                   (add1 acc))
                  (else acc))))
   (lambdas 0 (lambda (acc form)
               (match-case form
                 ((`(lambda ,args ,body ...))
                  (add1 acc))
                 (else acc)))))
  (:group (defuns defmacros defines lambdas :into sub-count)))

(add-collect! counter
  (errors)
  (:group (errors :into error-count)))