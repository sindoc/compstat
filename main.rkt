#lang racket

(require
 (except-in racket sort)
 "std-ops.rkt"
 "utils.rkt")

(define (make-stat-dataset nums)
  (define self null)
  (define cache null)
  (define sorted-nums null)
  (define (init)
    (when (or (null? nums)
              (null? (cdr nums)))
       (error "Data set must at least contain two elements:" set))
    (set! self dispatch-stat-dataset)
    (set! cache (make-hash))
    (set! sorted-nums (sort nums))
    self)
  
  (define hash-fail default-hash-fail)
  
  (define (cache-ref op)
    (hash-ref cache (op 'name) hash-fail))
  (define (cache-set! op val)
    (hash-set! cache (op 'name) val))
  
  (define (run-op op)
    (define cache-val (cache-ref op))
    (define (determine-nums)
      (if (op 'sort-required?) sorted-nums nums))
    (define val
      (cond 
        ((false? cache-val)
         (let ((computed-val 
                (apply (op 'compute) 
                       (list (determine-nums)))))
           (cache-set! op computed-val)
           computed-val))
        (else
         cache-val)))
    (show-keyval (op 'name) val))
  
  (define (run ops)
    (for-each run-op ops))

  (define (dispatch-stat-dataset msg)
    (case msg
      ((nums) nums)
      ((run) run)
      (else
       (error "Unknown message" msg))))
  (init))

(define (make-stat-operation computer #:sort? [sort? #t])
  (define self null)
  (define (init)
    (set! self operation-dispatcher)
    self)
  
  (define (compute nums)
    (computer nums))
  
  (define (operation-dispatcher msg)
    (case msg
      ((compute) compute)
      ((sort-required?) sort?)
      ((name) (object-name computer))
      (else
       (error "Unknown message" msg))))
  (init))

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