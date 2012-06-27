#lang racket

(require "utils.rkt")
(require "defs.rkt")

(require xml)
(require net/sendurl)

(define (row-sorter column (op >))
  (lambda (results)
    (sort results op #:key (lambda (row)
                             (assoc-value column row)))))

(define (column-sorter (op string>?))
  (lambda (results)
    (map (lambda (result)
           (sort result op #:key (lambda (cell)
                                   (symbol->string (car cell)))))
         results)))

(define (html-reporter results)
  (send-url/contents
   (xexpr->string
    `(html
      (body
       (h1 "ScanLisp Report")
       (hr)
       (table
        ,@(let/cc ret
            (let loop ((row (car results))
                       (rest (cdr results))
                       (lines null))
              (define (tr-format (type cdr))
                `(tr ,@(map (lambda (p)
                              `(td ,(format "~a" (type p))))
                            row)))
              (let ((tr (tr-format)))
                (when (null? rest)
                  (ret (cons (tr-format car) (cons tr lines))))
                (loop (car rest)
                      (cdr rest) 
                      (cons tr lines)))))))))))

(define (scanlisp (dir #f) #:report (report identity) #:row-sort (row-sort identity) #:column-sort (column-sort identity) #:summary? (summary? #f))
  
  (define current-dialect (make-parameter empty))
  (define lisp-regexp #rx"(?:[.])(rkt|scm|ss|lisp|lisp-expr|asd)$")
  (define (dialect-type path)
    (string->symbol
     (second (regexp-match lisp-regexp 
                           path))))
  
  (define (scan-project (dir #f))
    (let ((dir (expand-user-path (or dir (current-directory)))))
      (unless (directory-exists? dir)
        (error "Project directory inexists."))
      (let ((results null)
            (summary null)
            (ths null))
        (collect (ths (:into-list ths))
          (for ((path (in-directory dir)))
            (when (regexp-match? lisp-regexp path)
              (let-values (((base file base-p) (split-path path)))
                (parameterize ((current-directory base)
                               (current-dialect (dialect-type path)))
                  (ths (thread (lambda () 
                                 (set! results
                                       (cons (scan-file path) results))))))))))
        (thread-wait-all ths)
        (report (row-sort (column-sort results))))))
  
  (define (scan-file path)
    (generate-collect counters
      (letrec ((scan-form (lambda (form)
                            (cond ((not (pair? form)) form)
                                  ((pair? (car form))
                                   (count-sub (car form))
                                   (scan-form (car form))
                                   (scan-form (cdr form)))
                                  (else
                                   (scan-form (cdr form)))))))
        (count-pth path)
        (call-with-input-file path
          (lambda (in)
            (with-handlers ((exn:fail? (lambda (exn) (count-err exn))))
              (do ((form (read-toplevel-form in path)
                         (read-toplevel-form in path)))
                ((eof-object? form))
                (count-top form)
                (count-sub form)
                (scan-form form))))))))
  
  (define (read-toplevel-form in dialect)
    (case (current-dialect)
      ((lisp lisp-expr asd)
       (parameterize ((current-readtable 
                       (make-readtable #f 
                                       #\# #\ #f
                                       #\. #\ #f
                                       #\' #\ #f
                                       #\` #\ #f
                                       #\, #\ #f
                                       #\" 'terminating-macro read-unescape-string)))
         (read in)))
      ((rkt scm ss)
       (parameterize ((read-accept-lang #t)
                      (read-accept-reader #t))
         (read in)))
      (else
       (error "Unsupport dialect" (current-dialect)))))
  
  (define read-unescape-string
    (case-lambda
      ((ch in)
       (define string-regexp #rx"(\\\\.|[^\\\"])*\"")
       (let* ((str (bytes->string/utf-8 (car (regexp-match string-regexp in))))
              (len (string-length str)))
         (substring str 0 (- len 1))))
      ((ch in src line col pos)
       (datum->syntax #f (read-unescape-string ch in)))))
  
  (scan-project dir))
