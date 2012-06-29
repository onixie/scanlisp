#lang racket

(require "utils.rkt")
(require "defs.rkt")
(require xml)
(require net/sendurl)

(provide row-sorter
         column-sorter
         html-reporter)

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
  (let ((dir (third results))
        (summary (second results))
        (detail (first results))
        (page-name "ScanLisp Report"))
    (send-url/contents
     (format "<!DOCTYPE html PUBLIC \"-//W3C//DTD XHTML 1.0 Transitional//EN\" \"http://www.w3.org/TR/xhtml1/DTD/xhtml1-transitional.dtd\">~a"
             (xexpr->string
              `(html
                (head 
                 (title ,page-name)
                 (link ((rel "stylesheet")
                        (href ,(format "~ascanlisp.css" (current-directory))))))
                (body
                 (div ((class "Banner"))
                      (h1 ,page-name))
                 ,@(if summary
                       `((div ((class "Summary"))
                              ,(if dir
                                   `(div ((class "SumProj"))
                                         ,(format "Project directory: ~a" dir))
                                   "")
                              (table ((class "SumTab"))
                                     (tr ,@(map (lambda (p) `(td ((class "SumHead")
                                                                  (id ,(format "Sum~a" (car p))))
                                                                 ,(string-titlecase (format "~a" (car p))))) summary))
                                     (tr ,@(map (lambda (p) `(td ((class "SumContent")
                                                                  (id ,(format "Sum~a" (car p))))
                                                                 ,(format "~a" (cdr p)))) summary)))))
                       '(""))
                 
                 (div ((class "Detail"))
                      (table ((class "DetTab"))
                             ,@(let/cc ret
                                 (let loop ((row (car detail))
                                            (rest (cdr detail))
                                            (lines null)
                                            (ind 1))
                                   (define (tr-format (type cdr) (title identity))
                                     `(tr ,@(map (lambda (p)
                                                   `(td ((class ,@(type `(("DetHead") ,(if (even? ind) "DetEven" "DetOdd"))))
                                                         (id ,(format "Det~a" (car p))))
                                                        ,(title (format "~a" (type p)))))
                                                 row)))
                                   (let ((tr (tr-format)))
                                     (when (null? rest)
                                       (ret (cons (tr-format car string-titlecase) (cons tr lines))))
                                     (loop (car rest)
                                           (cdr rest)
                                           (cons tr lines)
                                           (add1 ind))))))))))))
    (void)))