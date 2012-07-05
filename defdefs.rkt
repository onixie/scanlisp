#lang racket

(require "utils.rkt")
(require (for-syntax racket/syntax))

(provide define-counters
         print-value
         get-value-of
         get-values-of
         print-description
         get-description-of)

(struct counter (desc value printer)
  #:mutable 
  #:property prop:custom-write (lambda (stt p m) 
                                 (fprintf p "~a:~a" 
                                          (counter-desc stt) 
                                          (counter-value stt))))

(define (make-counter-printer what)
  (lambda (counter)
    (let-values (((desc value)
                  ((counter-printer counter) counter)))
      (case what
        ('description desc)
        ('value value)
        (else value)))))

(define (make-counters-getter field-proc)
  (lambda (key counters)
    (let ((it (assoc-value key counters)))
      (when it
        (field-proc it)))))

(define (make-multiple-counters-getter field-proc)
  (lambda (key counters-lst)
    (let ((them (map (lambda (counters) ((make-counters-getter field-proc) key counters))
                     counters-lst)))
      (when (not (andmap void? them))
        them))))

(define print-value (make-counter-printer 'value))
(define print-description (make-counter-printer 'description))
(define get-value-of (make-counters-getter counter-value))
(define get-description-of (make-counters-getter counter-desc))
(define get-values-of (make-multiple-counters-getter counter-value))

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
                                                (unhygienize (format-symbol "define-summary-~a" sum) #'counters))
                                              (syntax->datum #'(summaries ...))))
                   (collect-counters (unhygienize (format-symbol "collect-~a" (syntax->datum #'counters)) #'counters))
                   (summary-counters (unhygienize (format-symbol "summary-~a" (syntax->datum #'counters)) #'counters))
                   ((summary-counters-only ...) (map (lambda (sum)
                                                       (unhygienize (format-symbol "summary-~a-only-~a" (syntax->datum #'counters) sum) #'counters))
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
           
           (define-syntax (collect-counters stx)
             (syntax-case stx (:into)
               ((_ (:into vals (... ...)) body1 (... ...))
                (if (not (has #'(body1 (... ...))))
                    #''()
                    (with-syntax ((counters (unhygienize 'counters #'(body1 (... ...)))))
                      #`(generate-collect counters (:into-alist vals (... ...))
                          body1 (... ...)))))
               ((_ () body1 (... ...))
                #'(collect-counters (:into) body1 (... ...)))))
           
           (define-counters-collect summaries
             (summary-details)) ...
                                
           (define-define-counter define-internal-summary
             (summaries summary-details)) ...
                                     
           (define-syntax define-summary
             (syntax-rules ()
               ((define-summary (name fname) (desc value accumulator printer))
                (define-internal-summary name (desc value
                                                    (lambda (cv alist)
                                                      (let ((it (get-value-of 'fname alist)))
                                                        (if (void? it)
                                                            cv
                                                            (accumulator cv it))))
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

           (define-syntax (summary-counters stx)
             (syntax-case stx ()
               ((_ details)
                (with-syntax (((summaries ...)
                               (list (unhygienize (syntax->datum #'summaries) #'details) ...))
                              (summary-details (unhygienize 'summary-details #'details)))
                  #'(let ((ds details))
                      (list
                       (generate-collect summaries (:into-alist)
                         (for ((detail (in-list ds)))
                           (summary-details detail))) ...))))))
           
           (define-syntax (summary-counters-only stx)
             (syntax-case stx ()
               ((_ details)
                (with-syntax ((summaries (unhygienize (syntax->datum #'summaries) #'details))
                              (summary-details (unhygienize 'summary-details #'details)))
                  #'(generate-collect summaries (:into-alist)
                      (for ((detail (in-list details)))
                        (summary-details detail))))))) ...
           body ...
           (void))))
    ((_ (counters) body ...)
     #'(define-counters (counters (:with-summary)) body ...))
    ((_ counters body ...)
     #'(define-counters (counters) body ...))))