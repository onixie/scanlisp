#lang racket

(require "utils.rkt")
(provide in-defdefs)

(define-syntax-rule (define-counters collect (groups ...))
  (define-collect collect
    ()
    (:group (:as groups) ...)))
  
(define-syntax-rule (define-define-counter def-name collect groups ...)
  (define-syntax-rule (def-name name (init&acc (... ...)))
    (add-collect! collect
      ((name init&acc (... ...)))
      (:group (name :into groups) ...))))

(define-syntax (in-defdefs stx)
  (syntax-case stx ()
    ((_ (counters sum-counters) body ...)
     (with-syntax ((define-file-counter (unhygienize 'define-file-counter #'counters))
                   (define-toplevel-counter (unhygienize 'define-toplevel-counter #'counters))
                   (define-subform-counter (unhygienize 'define-subform-counter #'counters))
                   (define-error-counter (unhygienize 'define-error-counter #'counters))
                   (define-summary-counter (unhygienize 'define-summary-counter #'sum-counters))
                   (define-simple-summary-counter (unhygienize 'define-simple-summary-counter #'sum-counters)))
       #'(begin
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
           
           body ...)))))