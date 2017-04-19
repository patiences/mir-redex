#lang racket
(require redex
         rackunit)
(require "mir.rkt")

;; Tests for metafunctions

;; Constants for testing
(define ENV0 (term [env 
                    (a 14)
                    (x 15)
                    (y 16)
                    (z 17)]))
(check-not-false (redex-match mir-machine ρ ENV0))

(define HEAP0 (term [store
                     (13 1)
                     (14 2)
                     (15 (ptr 14))
                     (16 void)
                     (17 (ptr 13))
                     (18 15)]))
(check-not-false (redex-match mir-machine σ HEAP0))

;; =========================================================
(define (rv-eval-tests)  
  (test-->> run (term ((use y) ,HEAP0 ,ENV0))
            (term (void ,HEAP0 ,ENV0)))
  (test-->> run (term ((use z) ,HEAP0 ,ENV0))
            (term ((ptr 13) ,HEAP0 ,ENV0)))
  (test-->> run (term ((& mut x) ,HEAP0 ,ENV0))
            (term (15 ,HEAP0 ,ENV0)))
  (test-->> run (term ((+ 1 2) ,HEAP0 ,ENV0)) (term (3 ,HEAP0 ,ENV0)))
  (test-->> run (term ((- 4 -20) ,HEAP0 ,ENV0)) (term (24 ,HEAP0 ,ENV0)))
  (test-->> run (term ((* 5 6) ,HEAP0 ,ENV0)) (term (30 ,HEAP0 ,ENV0)))
  (test-->> run (term ((/ 6 3) ,HEAP0 ,ENV0)) (term (2 ,HEAP0 ,ENV0)))
  (test-->> run (term ((< 1 2) ,HEAP0 ,ENV0)) (term (#t ,HEAP0 ,ENV0)))
  (test-->> run (term ((% -10 3) ,HEAP0 ,ENV0)) (term (-1 ,HEAP0 ,ENV0)))
  (test-->> run
            (term ((+ (use a) 1) ,HEAP0 ,ENV0)) ; a + 1
            (term (3 ,HEAP0 ,ENV0)))
  (test-->> run
            (term ((+ 1 (use a)) ,HEAP0 ,ENV0)) ; 1 + a
            (term (3 ,HEAP0 ,ENV0)))
  (test-->> run
            (term ((+ (use a) (use a)) ,HEAP0 ,ENV0))
            (term (4 ,HEAP0 ,ENV0)))
  (test-->> run (term ((<< 8 2) ,HEAP0 ,ENV0)) (term (32 ,HEAP0 ,ENV0)))
  (test-->> run (term ((>> 8 2) ,HEAP0 ,ENV0)) (term (2 ,HEAP0 ,ENV0)))
  (test-->> run (term ((== 1 2) ,HEAP0 ,ENV0)) (term (#f ,HEAP0 ,ENV0)))
  (test-->> run (term ((!= 1 2) ,HEAP0 ,ENV0)) (term (#t ,HEAP0 ,ENV0)))
  (test-->> run (term ((- 15) ,HEAP0 ,ENV0)) (term (-15 ,HEAP0 ,ENV0)))
  (test-->> run (term ((! #t) ,HEAP0 ,ENV0)) (term (#f ,HEAP0 ,ENV0)))
  (test-->> run (term ((< 1 2) ,HEAP0 ,ENV0)) (term (#t ,HEAP0 ,ENV0)))
  (test-->> run (term ((cast misc a as float) ,HEAP0 ,ENV0)) (term (2 ,HEAP0 ,ENV0)))
  (test-results))

(rv-eval-tests)
;; =========================================================
;; env-lookup : ρ x -> α
(check-exn exn:fail? (λ () (term (env-lookup (env) x))) "not found in environment: x")
(check-exn exn:fail? (λ () (term (env-lookup (env (a 1)) c))) "not found in environment: c")
(test-equal (term (env-lookup (env (a 1) (b 2) (c 3)) c)) 3)

;; =========================================================
;; store-lookup : σ α -> v
(check-exn exn:fail? (λ () (term (store-lookup (store) 1))) "not found in store: 1")
(check-exn exn:fail? (λ () (term (store-lookup (store (1 1) 0)))) "not found in store: 0")
(test-equal (term (store-lookup (store (1 111) (2 222) (3 333)) 3)) 333)

;; =========================================================
;; extend : σ α len -> σ
(test-equal (term (extend (store) 1 0)) (term (store)))
(test-equal (term (extend (store) 1 1)) (term [store (1 void)]))
(test-equal (term (extend ,HEAP0 19 1)) (term [store
                                               (19 void)
                                               (13 1)
                                               (14 2)
                                               (15 (ptr 14))
                                               (16 void)
                                               (17 (ptr 13))
                                               (18 15)]))
(test-equal (term (extend ,HEAP0 19 5)) (term [store
                                               (23 void)
                                               (22 void)
                                               (21 void)
                                               (20 void)
                                               (19 void)
                                               (13 1)
                                               (14 2)
                                               (15 (ptr 14))
                                               (16 void)
                                               (17 (ptr 13))
                                               (18 15)]))

;; =========================================================
;; malloc : σ len -> (σ α)
(test-equal (term (malloc (store) 0)) (term ((store) 0)))
(test-equal (term (malloc (store) 1)) (term ([store (0 void)]
                                        0)))
(test-equal (term (malloc ,HEAP0 1))
            (term ([store
                    (19 void)
                    (13 1)
                    (14 2)
                    (15 (ptr 14))
                    (16 void)
                    (17 (ptr 13))
                    (18 15)]
                   19)))
(test-equal (term (malloc ,HEAP0 2))
            (term ([store
                    (20 void)
                    (19 void)
                    (13 1)
                    (14 2)
                    (15 (ptr 14))
                    (16 void)
                    (17 (ptr 13))
                    (18 15)]
                   19)))

;; =========================================================
;; deref : σ ρ x -> v
(test-equal (term (deref ,HEAP0 ,ENV0 x)) (term (ptr 14)))
(test-equal (term (deref ,HEAP0 ,ENV0 y)) (term void))

;; =========================================================
;; put : σ ρ x v -> σ
(test-equal (term (put ,HEAP0 ,ENV0 x 1)) ; x is at address 15
            (term [store
                   (13 1)
                   (14 2)
                   (15 1)
                   (16 void)
                   (17 (ptr 13))
                   (18 15)]))

(test-results)