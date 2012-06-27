#lang racket

(require "utils.rkt")
(provide define-counters
         define-counters-adder)

(define-syntax-rule (define-counters collect (groups ...))
  (define-collect collect
    ()
    (:group (:as groups) ...)))
  
(define-syntax-rule (define-counters-adder adder-name collect groups ...)
  (define-syntax-rule (adder-name name init&acc (... ...))
    (add-collect! collect
      ((name init&acc (... ...)))
      (:group (name :into groups) ...))))