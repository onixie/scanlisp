(module utils racket
  (require (for-syntax racket/syntax))
  (require racket/performance-hint)
  
  (provide collect
           same-part
           values->list
           rcons
           match-case
           assoc-key
           assoc-value
           atom?)
  
  (define-syntax-rule (values->list thing)
    (call-with-values (lambda () thing) list))
  
  (begin-encourage-inline
    (define (rcons d a)
      (cons a d))
    (define (assoc-key key alist)
      (car (assoc key alist)))
    (define (assoc-value key alist)
      (cdr (assoc key alist)))
    (define (atom? thing)
      (and (not (pair? thing))
           (not (null? thing)))))
  
  (define-syntax (match-case stx)
    (define (split-cases cases-clauses)
      (apply append
             (map (lambda (cases-clause)
                    (let* ((clause-list (syntax->list cases-clause))
                           (cases-list (syntax->list (car clause-list)))
                           (clause (cdr clause-list)))
                      (map (lambda (case)
                             (cons case clause))
                           cases-list)))
                  (syntax->list cases-clauses))))
    (syntax-case stx (else)
      ((_ expr (cases cases-body ...) ... (else else-body ...))
       (with-syntax ((((case case-body ...) ...)
                      (split-cases #'((cases cases-body ...) ...))))
         #'(match expr
             (case case-body ...) ...
             (_ else-body ...))))))
  
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
    (syntax-case stx (:group :as)
      ((collect (binds ... (:group (fvars ... :as gfvar) ...)) body ...)
       #'(collect (binds ...)
           (letrec ((gfvar
                     (lambda rest-vars
                       (apply fvars rest-vars) ...)) ...)
             body ...)))
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
              (list (cons 'fvar var) ...)))))))
  
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
         (same-part (cons (sp s1 s2) (cddr strs))))))))
