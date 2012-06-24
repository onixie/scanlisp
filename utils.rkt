(module utils racket
  (require (for-syntax racket/syntax 
                       racket/list))
  (require racket/performance-hint)
  
  (provide collect
           define-collect
           add-collect!
           generate-collect
           same-part
           values->list
           rcons
           match-case
           assoc-key
           assoc-value
           atom?
           depth)
  
  (define-syntax-rule (values->list thing)
    (call-with-values (lambda () thing) list))
  
  (begin-encourage-inline
    (define (rcons d a)
      (cons a d))
    (define (assoc-key key alist)
      (let ((thing (assoc key alist)))
        (and thing (car thing))))
    (define (assoc-value key alist)
      (let ((thing (assoc key alist)))
        (and thing (cdr thing))))
    (define (atom? thing)
      (and (not (pair? thing))
           (not (null? thing)))))
  
  (begin-for-syntax
    (define (merge-assoc left right)
      (cond ((null? left) right)
            (else 
             (merge-assoc (cdr left)
                          (let* ((li (car left))
                                 (li-key (car li))
                                 (ri (assoc li-key right)))
                            (cond (ri (cons (list li-key 
                                                  (remove-duplicates (append (cadr li) (cadr ri))))
                                            (remove ri right)))
                                  (else (cons li right)))))))))
    
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
                       (apply fvars rest-vars) ...
                       (void))) ...)
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
  
  (define-syntax (define-collect stx)
    (syntax-case stx (:group :as)
      ((_ what (clauses ...))
       #'(begin-for-syntax
           (define what '((clauses ...) ()))))
      ((_ what (clauses ...) (:group (names ... :as gnames) ...))
       #'(begin-for-syntax
           (define what '((clauses ...)
                          ((gnames (names ...)) ...)))))))
  
  (define-syntax (add-collect! stx)
    (syntax-case stx (:group :into)
      ((_ what (clauses ...))
       #'(begin-for-syntax
           (set! what (list (list* 'clauses ... (car what))
                            (cadr what)))))
      ((_ what (clauses ...) (:group (names ... :into gnames) ...))
       (with-syntax ((grps #''((gnames (names ...)) ...)))
         #'(begin-for-syntax
             (set! what (list (list* 'clauses ... (car what))
                              (merge-assoc grps (cadr what)))))))))
  
  (define-syntax (generate-collect stx)
    (syntax-case stx ()
    ((_ what body ...)
     (with-syntax ((((clauses ...) groups ...)
                    (let ((collect (eval-syntax #'what)))
                      ;(display collect)
                      (cons
                       (let/cc return
                         (let loop-unhygiene ((hys (car collect)) (unhys null))
                           (cond ((null? hys) (return unhys))
                                 (else
                                  (let* ((hy (car hys))
                                         (unhy (cond ((pair? hy) 
                                                      (cons
                                                       (format-id #'what "~a" (car hy))
                                                       (cdr hy)))
                                                     (else (format-id #'what "~a" hy)))))
                                    (loop-unhygiene (cdr hys) (cons unhy unhys)))))))
                       (let/cc return
                         (let loop-group ((raws (cadr collect)) (grps null))
                           (cond ((null? raws) (return grps))
                                 (else
                                  (let* ((grp (car raws))
                                         (gname (format-id #'what "~a" (car grp)))
                                         (names (map (lambda (name)
                                                       (format-id #'what "~a" name))
                                                     (cadr grp)))
                                         (grp `(:group (,@names :as ,gname))))
                                    (loop-group (cdr raws) (cons grp grps)))))))))))
       #'(collect (clauses ... groups ...)
           body ...)))))
  
  
  (define (depth l)
    (letrec ((d (lambda (l)
                  (cond ((null? l) 1)
                        ((atom? (car l)) (d (cdr l)))
                        (else
                         (let ((cad (+ 1 (d (car l))))
                               (cdd (d (cdr l))))
                           (cond ((> cdd cad) cdd)
                                 (else cad))))))))
      (cond ((atom? l) 0)
            (else (d l)))))
  
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
