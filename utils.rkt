(module utils racket
  (require (for-syntax racket/syntax))
  (require racket/performance-hint)
  
  (provide collect
           same-part
           values->list
           rcons)

  (define-syntax-rule (values->list thing)
      (call-with-values (lambda () thing) list))
  
  (begin-encourage-inline
    (define (rcons d a)
      (cons a d)))
  
  (define-syntax (collect stx)
    (define (make-default-binds binds)
      (map (lambda (bind)
             (cond ((pair? (syntax-e bind))
                    (let* ((bind (syntax->list bind))
                          (len (length bind)))
                      (cond ((= 3 len) bind)
                            ((= 2 len) (append bind (list #'rcons)))
                            ((= 1 len) (append bind (list #'null #'rcons)))
                            (else (error "Invalid collect binds")))))
                   ((not (null? bind))
                    (list bind #'null #'rcons))
                   (else (error "Invalid collect binds"))))
           binds))
    (syntax-case stx ()
      ((collect (binds ...) body ...)
       (with-syntax* 
        ((((fvar init acc) ...) (make-default-binds (syntax->list #'(binds ...))))
         ((var ...) (generate-temporaries (syntax->list #'(fvar ...)))))
        #'(let ((var init) ...)
            (letrec ((fvar
                      (lambda rest-vars
                        (set! var (apply acc var rest-vars))
                        var)) ...)
              body ...
              (values var ...)))))))

  (define (same-part strs)
    (define (sp s1 s2)
      (let/cc hop
      (let ((pos 0))
        (for ((c1 (in-string s1))
              (c2 (in-string s2)))
          (unless (eq? c1 c2)
            (hop (substring s1 0 pos)))
          (set! pos (+ 1 pos)))
        (substring s1 0 pos))))
    (cond 
      ((null? strs) "")
      ((null? (cdr strs)) (car strs))
      (else
       (let ((s1 (car strs))
             (s2 (cadr strs)))
         (same-part (cons (sp s1 s2) (cddr strs)))))))
  )
