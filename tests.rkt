#lang racket

(require
 "std-ops.rkt"
 "main.rkt")

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

;; Khan Academy Exercises on Statistics
;; Mean, Median, and Mode:
;; http://www.khanacademy.org/exercises?exid=mean_median_and_mode
(define khan-stat-1-1 '(5 9 3 6 6 8))
(define khan-stat-1-2 '(4 3 9 7 7 9 9 8 10))
(define khan-stat-1-3 '(9 5 8 3 5 2 6 5 9 6))
(define khan-stat-1-4 '(8 4 2 10 10 10 9))
(define khan-stat-1-5 '(10 6 3 7 3 10 1 1 3))
(define khan-stat-1-6 '(7 10 4 5 7 8))
(define khan-stat-1-7 '(1 9 4 1 5 8))
(define khan-stat-1-8 '(1 2 4 6 7 7 9 10))
(define khan-stat-1-9 '(7 5 9 9 9 9 10))
(define khan-stat-1-10 '(6 1 1 1 5 1 7 8))
(define khan-stat-1-11 '(4 8 8 8 6 2))
(define khan-stat-1-12 '(2 8 3 4 5 9 3 8 3))

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
  khan-stat-1-1
  khan-stat-1-2
  khan-stat-1-3
  khan-stat-1-4
  khan-stat-1-5
  khan-stat-1-6
  khan-stat-1-7
  khan-stat-1-8
  khan-stat-1-9
  khan-stat-1-10
  khan-stat-1-11
  khan-stat-1-12
  ))

(analyze)