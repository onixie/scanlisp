#lang racket

(require plot)
(provide make-hist
         class-hist
         plot-hist)

(struct class (low high) 
  #:property prop:custom-write (lambda (stt p m) 
                                 (fprintf p "~a~~~a" 
                                          (class-low stt) 
                                          (class-high stt))))

(struct class-0s (low high)
  #:property prop:custom-write (lambda (stt p m)
                                 (fprintf p "...")))

(define (make-hist low high (step 1))
  (reverse (map
            (lambda (class-high)
              (vector (class (if (< (- class-high step) 0) 0 (- class-high step)) class-high) 0))
            (cons high (sequence->list (in-range (- high step) low (- step)))))))
  
(define (class-hist hist val)
  (map (lambda (class)
         (let ((low (class-low (vector-ref class 0)))
               (high (class-high (vector-ref class 0))))
           (when (or (and (> val low)
                          (<= val high))
                     (= val low high))
             (vector-set! class 1 (add1 (vector-ref class 1))))))
       hist)
  hist)

(define (optimize-hist hist)
  (let find-zero ((h hist) (s 0) (e 0) (zs null))
    (cond ((null? h) (if (= s e) zs (cons (list s (sub1 e)) zs)))
          ((zero? (vector-ref (car h) 1)) (find-zero (rest h) s (add1 e) zs))
          (else (find-zero (rest h) (+ e 2) (+ e 2) (if (= s e) zs (cons (list s e) zs)))))))
  
(define (plot-hist hist #:width-ratio (width-ratio 1) #:title (title #f) #:x-label (x-label #f) #:y-label (y-label #f))
  (plot (discrete-histogram hist) 
        #:title title 
        #:width (if (< width-ratio 1) (plot-width) (* (inexact->exact (round width-ratio)) (plot-width))) 
        #:x-label x-label 
        #:y-label y-label))