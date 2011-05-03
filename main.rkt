#lang racket

(require
 (except-in racket sort)
 "wrappers.rkt"
 "std-ops.rkt"
 "utils.rkt")

(define *sets+* '())
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

(define rain '(69.8 72.4 59.3 67.6 72.4))
(define rain- (cons 73.2 rain))
(define mice '(6.3 5.9 7.0 6.9 5.9))
(define mice- (cons 10.2 mice))
(define mosquitoes '(1.43 1.16 1.51))
(define mosquitoes-
  (map 
   (λ (x)
     (* (- x 1) 100))
   mosquitoes))

(setup
 (list 
  mean median variance mode mean-ad std-dev
  (list mad '(#:sort?) '(#f))
  first-quartile third-quartile sample-min sample-max)
 (list
  rain
  rain-
  mice
  mice-
  mosquitoes 
  mosquitoes-
  ))

(analyze)