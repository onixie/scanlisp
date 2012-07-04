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

(define (plot-hist hist #:width-ratio (width-ratio 1) #:title (title #f) #:x-label (x-label #f) #:y-label (y-label #f))
  (plot (discrete-histogram hist) 
        #:title title 
        #:width (if (< width-ratio 1) (plot-width) (* (inexact->exact (round width-ratio)) (plot-width))) 
        #:x-label x-label 
        #:y-label y-label))