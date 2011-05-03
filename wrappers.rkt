#lang racket

(require
 "utils.rkt")

(provide (all-defined-out))

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
  
  (define (hash-fail) #f)
  
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