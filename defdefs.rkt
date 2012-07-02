#lang racket

(require "utils.rkt")
(provide define-counters
         define-define-counter)

(define-syntax-rule (define-counters collect (groups ...))
  (define-collect collect
    ()
    (:group (:as groups) ...)))
  
(define-syntax-rule (define-define-counter def-name collect groups ...)
  (define-syntax-rule (def-name name (init&acc (... ...)))
    (add-collect! collect
      ((name init&acc (... ...)))
      (:group (name :into groups) ...))))