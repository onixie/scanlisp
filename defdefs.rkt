#lang racket

(require "utils.rkt")
(require (for-syntax racket/syntax))

(provide define-counters
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

(define-syntax-rule (define-define-counter def-name (collect groups ...))
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

(define-syntax (define-counters stx)
  (syntax-case stx (:with-summary)
    ((_ (counters (:with-summary summaries ...)) body ...)
     (with-syntax ((define-file-counter (unhygienize 'define-file-counter #'counters))
                   (define-toplevel-counter (unhygienize 'define-toplevel-counter #'counters))
                   (define-subform-counter (unhygienize 'define-subform-counter #'counters))
                   (define-error-counter (unhygienize 'define-error-counter #'counters))
                   ((define-summary ...) (map (lambda (sum)
                                                (unhygienize (format-symbol "define-~a" sum) #'counters))
                                              (syntax->datum #'(summaries ...))))
                   ((define-internal-summary ...) (generate-temporaries #'(summaries ...))))
       #'(begin
           (define-counters-collect counters 
             (count-files count-toplevels count-subforms count-errors))
           
           (define-define-counter define-file-counter 
             (counters count-files))
           (define-define-counter define-toplevel-counter
             (counters count-toplevels))
           (define-define-counter define-subform-counter
             (counters count-subforms))
           (define-define-counter define-error-counter
             (counters count-errors))
           
           (define-counters-collect summaries
             (summary-details)) ...
                                
           (define-define-counter define-internal-summary
             (summaries summary-details)) ...
                                     
           (define-syntax define-summary
             (syntax-rules ()
               ((define-summary (name fname) (desc value accumulator printer))
                (define-internal-summary name (desc value 
                                                    (lambda (cv alist)
                                                      (accumulator cv
                                                                   (counter-value (assoc-value 'fname alist))))
                                                    printer)))
               ((define-summary (name fname) (desc value accumulator))
                (define-summary (name fname) (desc value accumulator values)))
               ((define-summary (name fname) (desc))
                (define-summary (name fname) (desc null rcons)))
               ((define-summary (name fname) ())
                (define-summary (name fname) ("")))
               ((define-summary (name fname))
                (define-summary (name fname) ()))
               ((define-summary name (desc value accumulator printer))
                (define-summary (name name) (desc value accumulator printer)))
               ((define-summary name (desc value accumulator))
                (define-summary (name name) (desc value accumulator)))
               ((define-summary name (desc))
                (define-summary (name name) (desc)))
               ((define-summary name ())
                (define-summary (name name) ()))
               ((define-summary name)
                (define-summary (name name)))
               ((define-summary (name) (desc value accumulator printer))
                (define-summary name (desc value accumulator printer)))
               ((define-summary (name) (desc value accumulator))
                (define-summary name (desc value accumulator)))
               ((define-summary (name) (desc))
                (define-summary name (desc)))
               ((define-summary (name) ())
                (define-summary name ()))
               ((define-summary (name))
                (define-summary name)))) ...
           
           body ...
           (void))))
    ((_ (counters) body ...)
     #'(define-counters (counters (:with-summary)) body ...))
    ((_ counters body ...)
     #'(define-counters (counters) body ...))))