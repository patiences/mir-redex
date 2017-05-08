#lang racket
(require redex rackunit)
(require "mir.rkt")

(define (rv-eval-tests)
  (test--> run
           (term (+ (1 i32) (2 i32))) (term (3 i32)))
  (test--> run (term (! #t)) (term #f))
  (test-results))

(rv-eval-tests)