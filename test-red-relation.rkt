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

;; =========================================================
;; remove : σ α len -> σ
(test-equal (term (remove (store (3 void)
                                 (2 void)
                                 (1 void)
                                 (0 void))
                          0 4))
            (term (store)))
(test-equal (term (remove (store (3 void)
                                 (2 void)
                                 (1 void)
                                 (0 void))
                          1 2))
            (term (store (3 void)
                         (0 void))))
(check-exn exn:fail? (λ () (term (remove (store) 0 1))) "address not found in store: 1")

;; =========================================================
;; copy : σ α α len -> σ
(test-equal (term (copy (store (1 void)
                               (0 void))
                        0 1 0))
            (term (store (1 void)
                         (0 void))))
(test-equal (term (copy (store (100 void)
                               (5 5))
                        5 100 1))
            (term (store (100 5)
                         (5 5))))
(test-equal (term (copy (store (100 void)
                               (5 5))
                        100 5 1))
            (term (store (100 void)
                         (5 void))))
(test-equal (term (copy (store (104 void)
                               (103 void)
                               (102 void)
                               (101 void)
                               (4 4)
                               (3 3)
                               (2 2)
                               (1 1))
                        1 101 4))
            (term (store (104 4)
                         (103 3)
                         (102 2)
                         (101 1)
                         (4 4)
                         (3 3)
                         (2 2)
                         (1 1))))
(check-exn exn:fail? (λ () (term (copy (store) 100 101 1)))
           "copy: attempted read from address not found in store: 100")
(check-exn exn:fail? (λ () (term (copy (store (100 100)) 100 101 1)))
           "copy: attempted write to address not found in store: 101")

;; =========================================================
;; vec-new : σ -> (σ (vec α len cap))
(test-equal (term (vec-new (store))) (term ((store) (vec 0 0 0))))
(test-equal (term (vec-new ,HEAP0))
            (term (,HEAP0 (vec 19 0 0))))

;; vec-with-capacity : σ cap -> (σ (vec α len cap))
(test-equal (term (vec-with-capacity (store) 0)) (term ((store) (vec 0 0 0))))
(test-equal (term (vec-with-capacity (store) 4))
            (term ((store (3 void)
                          (2 void)
                          (1 void)
                          (0 void))
                   (vec 0 0 4))))

;; vec-push : σ (vec α len cap) v -> (σ (vec α len cap))
(define HEAP-WITH-VEC-8 (term (store (7 void)
                                     (6 void)
                                     (5 void)
                                     (4 void)
                                     (3 3)
                                     (2 2)
                                     (1 1)
                                     (0 0))))
(test-equal (term (vec-push ,HEAP-WITH-VEC-8 (vec 0 4 8) 4))
            (term ((store (7 void)
                          (6 void)
                          (5 void)
                          (4 4)
                          (3 3)
                          (2 2)
                          (1 1)
                          (0 0))
                   (vec 0 5 8))))
;; unallocated vector -> vec with capacity 4 
(test-equal (term (vec-push (store) (vec 0 0 0) 5))
            (term ((store (3 void)
                          (2 void)
                          (1 void)
                          (0 5))
                   (vec 0 1 4))))
;; push onto a vec filled to capacity, needs resizing
(test-equal (term (vec-push (store (3 3)
                                   (2 2)
                                   (1 1)
                                   (0 0))
                            (vec 0 4 4)
                            4)) 
            (term ((store (11 void)
                          (10 void)
                          (9 void)
                          (8 4)
                          (7 3)
                          (6 2)
                          (5 1)
                          (4 0))
                   (vec 4 5 8))))
(test-equal (term (vec-resize (store (3 3)
                                     (2 2)
                                     (1 1)
                                     (0 0))
                              (vec 0 4 4)))
            (term ((store (11 void)
                          (10 void)
                          (9 void)
                          (8 void)
                          (7 3)
                          (6 2)
                          (5 1)
                          (4 0))
                   (vec 4 4 8))))
(test-equal (term (vec-resize (store (1 1)
                                     (0 0))
                              (vec 0 2 2))) 
            (term ((store (5 void)
                          (4 void)
                          (3 1)
                          (2 0))
                   (vec 2 2 4))))


(test-results)