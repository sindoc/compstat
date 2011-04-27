#lang racket

(require
 "utils.rkt"
 (rename-in racket (sort r:sort)))

(define *std-ops* '())

(define (sum nums . op)
  (apply 
   + 
   (map 
    (if (null? op) id (car op)) 
    nums)))

(define (mean nums)
  (/ (sum nums) 
     (length nums)))

(define (variance nums)
  (define mean-val (mean nums))
  (define n (- (length nums) 1))
  (/
   (sum 
    nums
    (λ (x)
      (sq
       (- x mean-val))))
   n))

(define (quartile nums k)
  (define len (length nums))
  (define idx (/ (* k (+ len 1)) 4))
  (/
   (+ (list-ref nums (- (floor idx) 1))
      (list-ref nums (- (ceiling idx) 1)))
   2))

(define (median nums)
  (quartile nums 2))

(define (old-median nums)
  (define len (length nums))
  (define mid (/ (+ 1 len) 2))
  (cond
    ((odd? len)
     (list-ref nums (- mid 1)))
    (else
     (mean
      (list 
       (list-ref nums (- (floor mid) 1))
       (list-ref nums (floor mid)))))))

(define (ad nums proc)
  (define med (median nums))
  (proc
   (map
    (λ (x)
      (abs (- x med)))
    nums)))

(define (mad nums)
  (ad nums median))

(define (mean-ad nums)
  (ad nums mean))

(define (sample-min nums)
  (car nums))

(define (sample-max nums)
  (last nums))

(define (first-quartile nums)
  (quartile nums 1))

(define (third-quartile nums)
  (quartile nums 3))

(define (std-dev nums)
  (sqrt (variance nums)))

(define (mode nums)
  (define prev-c 0)
  (define prev-x 0)
  (define max-count 0)
  (define max #f)
  (map
   (λ (x c)
     (define val
       (cond 
         ((= x prev-x)
          (let ((new-count (+ prev-c 1)))
            (when (> new-count max-count)
              (set! max-count new-count)
              (set! max x))
            new-count))
         (else 1)))
     (set! prev-x x)
     (set! prev-c val)
     val)
   nums
   (build-list 
    (length nums)
    (λ (_) 1)))
  max)

(define (sort l)
  (r:sort l <))

(define (analyze nums ops)
  (show-line)
  (show nums)
  (show-line)
  (for-each
   (λ (op)
     (show-keyval (object-name op) (op nums)))
   ops))

(define (analyze+ sets ops)
  (for-each 
   (λ (set) 
     (analyze (sort set) ops))
   sets))

(define (setup)
  (set! 
   *std-ops* 
   (list mean median variance mode mad mean-ad std-dev)))

(define rain '(69.8 72.4 59.3 67.6 72.4))
(define rain- (cons 73.2 rain))
(define mosquitoes '(1.43 1.16 1.51))
(define mosquitoes-
  (map 
   (λ (x)
     (* (- x 1) 100))
   mosquitoes))

(define sets
  (list
   rain rain- mosquitoes mosquitoes-))

(setup)
(analyze+ sets *std-ops*)
 