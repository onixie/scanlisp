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
        (detail (first results)))
    (send-url/contents
     (xexpr->string
      `(html
        (body
         (h1 "ScanLisp Report")
         ,@(if summary
               `((hr)
                 ,(if dir
                      (format "Project directory: ~a" dir)
                      "")
                 (table
                  (tr ,@(map (lambda (p) `(td ,(format "~a" (car p)))) summary))
                  (tr ,@(map (lambda (p) `(td ,(format "~a" (cdr p)))) summary))))
               '(""))
         (hr)
         (table
          ,@(let/cc ret
              (let loop ((row (car detail))
                         (rest (cdr detail))
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
                        (cons tr lines))))))))))
    (void)))