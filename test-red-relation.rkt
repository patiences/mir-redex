#lang racket
(require redex
         rackunit)
(require "mir.rkt")

;; Tests for metafunctions

;; Constants for testing
(define V0 (term [(x 15)
                  (y 16)
                  (z 17)]))
(check-not-false (redex-match mir-machine V V0))

(define H0 (term [(13 1)
                  (14 2)
                  (15 (ptr 14))
                  (16 void)
                  (17 (ptr 13))
                  (18 15)]))
(check-not-false (redex-match mir-machine H H0))

;; =========================================================
(define (rv-eval-tests)  
  (test-->> run (term ((use y) ,H0 ,V0))
            (term (void ,H0 ,V0)))
  (test-->> run (term ((use z) ,H0 ,V0))
            (term ((ptr 13) ,H0 ,V0)))
  (test-->> run (term ((& mut x) ,H0 ,V0))
            (term (15 ,H0 ,V0)))
  (test-->> run (term ((+ 1 2) ,H0 ,V0)) (term (3 ,H0 ,V0)))
  (test-->> run (term ((- 4 -20) ,H0 ,V0)) (term (24 ,H0 ,V0)))
  (test-->> run (term ((* 5 6) ,H0 ,V0)) (term (30 ,H0 ,V0)))
  (test-->> run (term ((/ 6 3) ,H0 ,V0)) (term (2 ,H0 ,V0)))
  (test-->> run (term ((< 1 2) ,H0 ,V0)) (term (#t ,H0 ,V0)))
  (test-->> run (term ((% -10 3) ,H0 ,V0)) (term (-1 ,H0 ,V0)))
  (test-->> run
            (term ((+ ((& mut x) ,H0 ,V0) 1) ,H0 ,V0)) ; x + 1
            (term ((16 ,H0 ,V0) ,H0 ,V0)))
  (test-->> run
            (term ((+ 1 ((& mut x) ,H0 ,V0)) ,H0 ,V0)) ; 1 + x
            (term ((16 ,H0 ,V0) ,H0 ,V0)))
  #; ;;FIXME why doesn't this match 
  (test-->> run
            (term ((+ ((& mut x) ,H0 ,V0)        ; x + x 
                      ((& mut x) ,H0 ,V0)
                      ,H0 ,V0)))
            (term ((30 ,H0 ,V0) ,H0 ,V0)))
  (test-->> run (term ((<< 8 2) ,H0 ,V0)) (term (32 ,H0 ,V0)))
  (test-->> run (term ((>> 8 2) ,H0 ,V0)) (term (2 ,H0 ,V0)))
  (test-->> run (term ((== 1 2) ,H0 ,V0)) (term (#f ,H0 ,V0)))
  (test-->> run (term ((!= 1 2) ,H0 ,V0)) (term (#t ,H0 ,V0)))
  (test-->> run (term ((- 15) ,H0 ,V0)) (term (-15 ,H0 ,V0)))
  (test-->> run (term ((! #t) ,H0 ,V0)) (term (#f ,H0 ,V0)))
  (test-->> run (term ((< 1 2) ,H0 ,V0)) (term (#t ,H0 ,V0)))
  #; ;; FIXME
  (test-->> run (term ((! (< 1 2)) ,H0 ,V0)) (term (#f ,H0 ,V0)))
  #; ;; FIXME 
  (test-->> run
            (term ((((+ 1 2) ,H0 ,V0)
                    ((+ 4 5) ,H0 ,V0))
                   ,H0 ,V0))
            (term ((3 9) ,H0 ,V0)))
  (test-results))

(rv-eval-tests)
;; =========================================================
;; get : ((any any) ...) any -> any or #f
(test-equal (term (get () x)) #f)
(test-equal (term (get ((a 1) (b 2) (c 3)) c)) 3)
(test-equal (term (get ((1 111) (2 222) (3 333)) 3)) 333)
(test-equal (term (get ((x 0) (y 0)) a)) #f)

;; =========================================================
;; extend : H α -> H
(test-equal (term (extend () 1)) (term [(1 void)]))
(test-equal (term (extend ,H0 19)) (term [(19 void)
                                          (13 1)
                                          (14 2)
                                          (15 (ptr 14))
                                          (16 void)
                                          (17 (ptr 13))
                                          (18 15)]))

;; =========================================================
;; malloc : H -> (H α)
(test-equal (term (malloc ())) (term ([(0 void)]
                                      0)))
(test-equal (term (malloc ,H0))
            (term ([(19 void)
                    (13 1)
                    (14 2)
                    (15 (ptr 14))
                    (16 void)
                    (17 (ptr 13))
                    (18 15)]
                   19)))

;; =========================================================
;; deref : H V x -> hv
(test-equal (term (deref ,H0 ,V0 x)) (term (ptr 14)))
(test-equal (term (deref ,H0 ,V0 y)) (term void))

;; =========================================================
;; put : H V x hv -> H
(test-equal (term (put ,H0 ,V0 x 1)) ; x is at address 15
            (term [(13 1)
                   (14 2)
                   (15 1)
                   (16 void)
                   (17 (ptr 13))
                   (18 15)]))

(test-results)