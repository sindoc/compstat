#lang racket

(provide (all-defined-out))

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