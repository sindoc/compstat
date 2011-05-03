#lang racket

(require
 (rename-in racket (sort r:sort)))

(provide (all-defined-out))

(define (sort l)
  (r:sort l <))

(define (default-hash-fail) #f)

(define show
  (λ stuff
    (map
     (λ (x)
       (display x)
       (display " "))
     stuff)
    (newline)))

(define (show-keyval key val)
  (show key ": " val))

(define (show-line)
  (show "====================="))

(define (id x) x)

(define (sq x) (* x x))