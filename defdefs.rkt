#lang racket

(require "utils.rkt")
(provide in-defdefs
         print-value
         get-value
         print-description
         get-description)

(struct counter (desc value printer) #:mutable)

(define (make-counter-printer what)
  (lambda (counter)
    (let-values (((desc value)
                  ((counter-printer counter) counter)))
      (case what
        ('description desc)
        ('value value)
        (else value)))))

(define (make-counter-in-collect-getter field-proc)
  (lambda (key counters-collect)
    (field-proc (assoc-value key counters-collect))))

(define print-value (make-counter-printer 'value))
(define print-description (make-counter-printer 'description))
(define get-value (make-counter-in-collect-getter counter-value))
(define get-description (make-counter-in-collect-getter counter-desc))

(define-syntax-rule (define-counters-collect collect (groups ...))
  (define-collect collect
    ()
    (:group (:as groups) ...)))

(define-syntax-rule (define-define-counter def-name collect groups ...)
  (define-syntax def-name
    (syntax-rules ()
      ((def-name name (desc value accumulator printer))
       (add-collect! collect
         ((name 
           (counter desc value (lambda (cv) 
                                 (printer (counter-desc cv) 
                                          (counter-value cv)))) 
           (lambda (cv v)
             (set-counter-value! cv 
                                 (accumulator (counter-value cv) v)) 
             cv)))
         (:group (name :into groups) ...)))
      ((def-name name (desc value accumulator))
       (def-name name (desc value accumulator (lambda (d v) (values d v)))))
      ((def-name name (desc))
       (def-name name (desc null rcons)))
      ((def-name name ())
       (def-name name ("")))
      ((def-name name)
       (def-name name ())))))

(define-syntax (in-defdefs stx)
  (syntax-case stx ()
    ((_ (counters sum-counters) body ...)
     (with-syntax ((define-file-counter (unhygienize 'define-file-counter #'counters))
                   (define-toplevel-counter (unhygienize 'define-toplevel-counter #'counters))
                   (define-subform-counter (unhygienize 'define-subform-counter #'counters))
                   (define-error-counter (unhygienize 'define-error-counter #'counters))
                   (define-summary-counter (unhygienize 'define-summary-counter #'sum-counters)))
       #'(begin
           (define-counters-collect counters (count-pth count-top count-sub count-err))
           (define-counters-collect sum-counters (sum-prj))
           
           (define-define-counter define-file-counter counters count-pth)
           (define-define-counter define-toplevel-counter counters count-top)
           (define-define-counter define-subform-counter counters count-sub)
           (define-define-counter define-error-counter counters count-err)
           
           (define-define-counter define-internal-summary-counter sum-counters sum-prj)
           (define-syntax define-summary-counter
             (syntax-rules ()
               ((define-summary-counter (name fname) (desc value accumulator printer))
                (define-internal-summary-counter name (desc value 
                                                            (lambda (cv alist)
                                                              (accumulator cv
                                                                           (counter-value (assoc-value 'fname alist))))
                                                            printer)))
               ((define-summary-counter (name fname) (desc value accumulator))
                (define-summary-counter (name fname) (desc value accumulator values)))
               ((define-summary-counter (name fname) (desc))
                (define-summary-counter (name fname) (desc null rcons)))
               ((define-summary-counter (name fname) ())
                (define-summary-counter (name fname) ("")))
               ((define-summary-counter (name fname))
                (define-summary-counter (name fname) ()))
               ((define-summary-counter name (desc value accumulator printer))
                (define-summary-counter (name name) (desc value accumulator printer)))
               ((define-summary-counter name (desc value accumulator))
                (define-summary-counter (name name) (desc value accumulator)))
               ((define-summary-counter name (desc))
                (define-summary-counter (name name) (desc)))
               ((define-summary-counter name ())
                (define-summary-counter (name name) ()))
               ((define-summary-counter name)
                (define-summary-counter (name name)))
               ((define-summary-counter (name) (desc value accumulator printer))
                (define-summary-counter name (desc value accumulator printer)))
               ((define-summary-counter (name) (desc value accumulator))
                (define-summary-counter name (desc value accumulator)))
               ((define-summary-counter (name) (desc))
                (define-summary-counter name (desc)))
               ((define-summary-counter (name) ())
                (define-summary-counter name ()))
               ((define-summary-counter (name))
                (define-summary-counter name))))
           
           body ...)))))