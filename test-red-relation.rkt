#lang racket
(require redex rackunit)
(require "mir.rkt")

;; Constants for testing
;; =========================================================

;; Addresses 
(define a0 (term (ptr ,(gensym))))
(define a1 (term (ptr ,(gensym))))
(define a2 (term (ptr ,(gensym))))
(define a3 (term (ptr ,(gensym))))

(define MT-ENV (term (env)))
(define ENV0 (term [env 
                    (a ,a0)
                    (x ,a1)
                    (y ,a2)
                    (z ,a3)]))
(check-not-false (redex-match mir-machine ρ ENV0))

(define MT-STORE (term (store)))
(define STORE0 (term (store [,a0 void]
                            [,a1 ,a2]
                            [,a2 (5 i32)]
                            [,a3 void])))
(check-not-false (redex-match mir-machine σ STORE0))

;; Reduction tests
;; =========================================================
(define (function-eval-tests)
  (test-results))

(function-eval-tests)

(define (statement-eval-tests)
  (test-->> run (term ((= a (1 i32)) ,STORE0 ,ENV0))
            (term (void (store [,a0 (1 i32)]
                               [,a1 ,a2]
                               [,a2 (5 i32)]
                               [,a3 void])
                        ,ENV0)))
  (test-->> run (term ((= a (+ (1 i32) (1 i32))) ,STORE0 ,ENV0))
            (term (void (store [,a0 (2 i32)]
                               [,a1 ,a2]
                               [,a2 (5 i32)]
                               [,a3 void])
                        ,ENV0)))
  (test-->> run (term ((* x) ,STORE0 ,ENV0))
            (term (,a2 ,STORE0 ,ENV0)))
  (test-->> run (term ((= (* x) (1 i32)) ,STORE0 ,ENV0)) ; *x = 1 
            (term (void (store [,a0 void]
                               [,a1 ,a2]
                               [,a2 (1 i32)] ; *x
                               [,a3 void])
                        ,ENV0)))
  #;(test-->> run (term ((let-vars ([= a (1 i32)] ; run multiple statements
                                  [= (* x) (2 i32)]))
                       ,STORE0 ,ENV0))
            (term (void (store [,a0 (1 i32)]
                               [,a1 ,a2]
                               [,a2 (2 i32)] ; *x
                               [,a3 void])
                        ,ENV0)))
  (test-results))

(statement-eval-tests)

(define (rv-eval-tests)
  (test-->> run (term ((use y) ,STORE0 ,ENV0))
            (term ((5 i32) ,STORE0 ,ENV0)))
  (test-->> run (term ((use z) ,STORE0 ,ENV0))
            (term (void ,STORE0 ,ENV0)))
  (test-->> run (term ((& mut x) ,STORE0 ,ENV0))
            (term (,a1 ,STORE0 ,ENV0)))
  (test-->> run (term ((+ (1 i32) (2 i32)) ,STORE0 ,ENV0)) (term ((3 i32) ,STORE0 ,ENV0)))
  (test-->> run (term ((- (4 i32) (-20 i32)) ,STORE0 ,ENV0)) (term ((24 i32) ,STORE0 ,ENV0)))
  (test-->> run (term ((* (5 i32) (6 i32)) ,STORE0 ,ENV0)) (term ((30 i32) ,STORE0 ,ENV0)))
  (test-->> run (term ((< (1 i32) (2 i32)) ,STORE0 ,ENV0)) (term (#t ,STORE0 ,ENV0)))
  (test-->> run (term ((% (-10 i32) (3 i32)) ,STORE0 ,ENV0)) (term ((-1 i32) ,STORE0 ,ENV0)))
  (test--> run (term ((! #t) ,STORE0 ,ENV0)) (term (#f ,STORE0 ,ENV0)))
  (test-->> run
            (term ((+ (use y) (1 i32)) ,STORE0 ,ENV0)) ; y + 1
            (term ((6 i32) ,STORE0 ,ENV0)))
  (test-->> run
            (term ((+ (1 i32) (use y)) ,STORE0 ,ENV0)) ; 1 + y
            (term ((6 i32) ,STORE0 ,ENV0)))
  (test-->> run
            (term ((+ (use y) (use y)) ,STORE0 ,ENV0))
            (term ((10 i32) ,STORE0 ,ENV0)))
  (test-results))

(rv-eval-tests)

;; Metafunction tests
;; =========================================================
;; deref : σ ρ x -> v
(test-equal (term (deref ,STORE0 ,ENV0 x)) (term ,a2))
(test-equal (term (deref ,STORE0 ,ENV0 y)) (term (5 i32)))

;; =========================================================
;; env-lookup : ρ x -> α
(check-exn exn:fail? (λ () (term (env-lookup (env) x))) "env-lookup: variable not found in environment: x")
(check-exn exn:fail? (λ () (term (env-lookup (env (x ,a1)) ,a2))) "env-lookup: address not found in environment: c")
(test-equal (term (env-lookup ,ENV0 z)) (term ,a3))

;; =========================================================
;; store-lookup : σ α -> v
(check-exn exn:fail? (λ () (term (store-lookup (store) 1))) "store-lookup: address not found in store: 1")
(check-exn exn:fail? (λ () (term (store-lookup ,STORE0 (gensym)))) "store-lookup: address not found in store: 0")
(test-equal (term (store-lookup ,STORE0 ,a2)) (term (5 i32)))

(test-results)