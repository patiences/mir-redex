#lang racket
(require redex
         rackunit)
(require "mir.rkt")

(check-not-false (redex-match mir-e const (term 2)))
(check-not-false (redex-match mir-e rv (term (+ 1 2))))
(test-->> reduce (term (+ 1 2)) (term 3))

(test-->> reduce (term (* 5 6)) (term 30))

(test-results)