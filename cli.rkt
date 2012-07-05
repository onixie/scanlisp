#! /usr/local/bin/racket
#lang racket

(require racket/cmdline)
(require "scanlisp.rkt")
(require plot)

(module+ main
  (define report? (make-parameter #t))
  (define plot? (make-parameter #f))
  (define compare? (make-parameter #f))
  (define field (make-parameter null))
  
  (define args (command-line
                #:once-any
                (("-r" "--report") "Generate Report"
                                   (report? #t)
                                   (plot? #f))
                #:multi
                (("-p" "--plot") f
                                 "Generate Histogram"
                                 (report? #f)
                                 (plot? #t)
                                 (plot-new-window? #t)
                                 (field (cons (string->symbol f) (field))))
                #:once-each
                (("-c" "--compare") "Compare Mode"
                                    (compare? #t))
                #:args args
                
                args))
  (if (> (length args) 0)
      (if (compare?)
          (cond ((report?) (report (apply compare (map (lambda (path) (scanlisp path)) args))))
                ((plot?) (histogram (apply compare (map (lambda (path) (scanlisp path)) args)) #:by (field)))
                (else (exit)))
          (cond ((report?) (report (scanlisp (car args))))
                ((plot?) (histogram (scanlisp (car args)) #:by (field)))
                (else (exit))))
      (exit)))