#lang racket

(require
 "wrappers.rkt"
 "utils.rkt")

(provide (all-defined-out))

;; The collection of sample data sets to be analyzed
(define *sets+* '())

;; The collection of statistical operations to be
;; performed on sample data sets
(define *ops+* '())

(define (setup ops sets)
  (set!
   *ops+*
   (map
    (λ (op)
      (if (list? op)
          (keyword-apply
           make-stat-operation
           (cadr op)
           (caddr op)
           (list (car op)))
          (apply 
           make-stat-operation 
           (list op))))
    ops))
  (set!
   *sets+*
   (map
    (λ (set)
      (make-stat-dataset set))
    sets)))

(define (analyze)
  (for-each
   (λ (set+)
     (show-line)
     (show (set+ 'nums))
     (show-line)
     ((set+ 'run) *ops+*))
   *sets+*))