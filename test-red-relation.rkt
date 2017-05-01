#lang racket
(require redex
         rackunit)
(require "mir.rkt")

;; Tests for metafunctions

;; Constants for testing
(define MT-ENV (term (env)))
(define ENV0 (term [env 
                    (a 14)
                    (x 15)
                    (y 16)
                    (z 17)]))
(check-not-false (redex-match mir-machine ρ ENV0))

(define MT-HEAP (term (store)))
(define HEAP0 (term [store
                     (13 1)
                     (14 2)
                     (15 (ptr 14))
                     (16 void)
                     (17 (ptr 13))
                     (18 15)]))
(check-not-false (redex-match mir-machine σ HEAP0))

(define MT-TENV (term (tenv)))
(check-not-false (redex-match mir-machine Γ MT-TENV))

;; =========================================================
(define (fn-eval-tests)
  (test-->> run (term ((x : int) ,MT-HEAP ,MT-ENV (tenv))) ;; a single integer vdecl gets 1 block of memory
            (term (void (store (0 void)) (env (x 0)) (tenv (x int)))))
  (test-->> run (term ((x : (vec int 4)) ,MT-HEAP ,MT-ENV ,MT-TENV)) ; vec w/ capacity = 4 gets 4 blocks
            (term (void (store (3 void) (2 void) (1 void) (0 void))
                        (env (x 0)) (tenv (x (vec int 4))))))
  (test-->> run (term ((x : (int float int)) ,MT-HEAP ,MT-ENV ,MT-TENV)) ; triple of numbers 
            (term (void (store (2 void) (1 void) (0 void))
                        (env (x 0)) (tenv (x (int float int))))))
  (test-->> run ;; no decls 
            (term ((-> main () unit-ty
                       void
                       (bb bb0 () return))
                   ,MT-HEAP,MT-ENV ,MT-TENV))
            (term ((-> main () unit-ty
                       void
                       (bb bb0 () return))
                   ,MT-HEAP ,MT-ENV ,MT-TENV))) 
  (test-->> run ;; single vdecl inside a fn 
            (term ((-> main () unit-ty
                       (_0 : int)
                       (bb bb0 () return))
                   ,MT-HEAP ,MT-ENV ,MT-TENV))
            (term ((-> main () unit-ty
                       void
                       (bb bb0 () return))
                   (store (0 void)) (env (_0 0)) (tenv (_0 int)))))
  (test-->> run ;; multiple vdecls inside a fn 
            (term ((-> main () unit-ty
                       (a : int)
                       (b : float)
                       (c : unit-ty)
                       (mut d : int)
                       (bb bb0 () return))
                   ,MT-HEAP ,MT-ENV ,MT-TENV))
            (term ((-> main () unit-ty
                       void void void void
                       (bb bb0 () return))
                   (store (3 void) (2 void) (1 void) (0 void))
                   (env (d 3) (c 2) (b 1) (a 0))
                   (tenv (d int) (c unit-ty) (b float) (a int)))))
  (test-results))

(fn-eval-tests)

(define (bb-eval-tests)
  (test-->> run
            (term ((bb bb0 () return) ,MT-HEAP ,MT-ENV ,MT-TENV))
            (term ((bb bb0 () return) ,MT-HEAP ,MT-ENV ,MT-TENV)))
  (test-->> run
            (term ((bb bb0
                       ((= a 1)
                        (= b 2))
                       return)
                   (store (1 void) (2 void)) ; statements expect variables to already be heap-initialized 
                   (env (a 1) (b 2))
                   (tenv (a int) (b int))))
            (term ((bb bb0 (void void) return)
                   (store (1 1) (2 2))
                   (env (a 1) (b 2))
                   (tenv (a int) (b int)))))
  (test-results))

(bb-eval-tests)

(define (statement-eval-tests)
  (test-->> run
            (term ((= a 3) ,HEAP0 ,ENV0 ,MT-TENV))
            (term (void
                   [store
                    (13 1)
                    (14 3) ; a
                    (15 (ptr 14))
                    (16 void)
                    (17 (ptr 13))
                    (18 15)]
                   ,ENV0 ,MT-TENV)))
  (test-->> run
            (term ((= a (+ 1 2)) ,HEAP0 ,ENV0 ,MT-TENV))
            (term (void
                   [store
                    (13 1)
                    (14 3) ; a
                    (15 (ptr 14))
                    (16 void)
                    (17 (ptr 13))
                    (18 15)]
                   ,ENV0 ,MT-TENV)))
  (test-->> run (term ((* x) ,HEAP0 ,ENV0 ,MT-TENV))
            (term ((ptr 14) ,HEAP0 ,ENV0 ,MT-TENV)))
  (test-->> run
            (term ((= (* x) 3) ,HEAP0 ,ENV0 ,MT-TENV))
            (term (void
                   [store
                    (13 1)
                    (14 3) ; a
                    (15 (ptr 14))
                    (16 void)
                    (17 (ptr 13))
                    (18 15)]
                   ,ENV0 ,MT-TENV)))
  ;; projection to an invalid address. No bounds checks here? 
  (check-exn exn:fail? (λ () (apply-reduction-relation* run (term ((= (· a 100) 123) ,HEAP0 ,ENV0 ,MT-TENV))))
             "store-update: address not found in store: 114")
  (test-->> run
            (term ((· a 0) ,HEAP0 ,ENV0 ,MT-TENV))
            (term ((ptr 14) ,HEAP0 ,ENV0 ,MT-TENV)))
  (test-->> run (term ((= (· a 2) 100) ,HEAP0 ,ENV0 ,MT-TENV))
            (term (void
                   [store
                    (13 1)
                    (14 2) ; a
                    (15 (ptr 14))
                    (16 100) ;a.2
                    (17 (ptr 13))
                    (18 15)]
                   ,ENV0 ,MT-TENV)))
  (test-->> run (term (((= (· a 0) 1)
                        (= (· a 1) 2)
                        (= (· a 2) 3))
                       ,HEAP0 ,ENV0 ,MT-TENV))
            (term ((void void void)
                   [store
                    (13 1)
                    (14 1) ; a.0
                    (15 2) ; a.1
                    (16 3)
                    (17 (ptr 13))
                    (18 15)]
                   ,ENV0 ,MT-TENV)))
  (test-results))

(statement-eval-tests)

(define (rv-eval-tests)  
  (test-->> run (term ((use y) ,HEAP0 ,ENV0 ,MT-TENV))
            (term (void ,HEAP0 ,ENV0 ,MT-TENV)))
  (test-->> run (term ((use z) ,HEAP0 ,ENV0 ,MT-TENV))
            (term ((ptr 13) ,HEAP0 ,ENV0 ,MT-TENV)))
  (test-->> run (term ((& mut x) ,HEAP0 ,ENV0 ,MT-TENV))
            (term (15 ,HEAP0 ,ENV0 ,MT-TENV)))
  (test-->> run (term ((+ 1 2) ,HEAP0 ,ENV0 ,MT-TENV)) (term (3 ,HEAP0 ,ENV0 ,MT-TENV)))
  (test-->> run (term ((- 4 -20) ,HEAP0 ,ENV0 ,MT-TENV)) (term (24 ,HEAP0 ,ENV0 ,MT-TENV)))
  (test-->> run (term ((* 5 6) ,HEAP0 ,ENV0 ,MT-TENV)) (term (30 ,HEAP0 ,ENV0 ,MT-TENV)))
  (test-->> run (term ((/ 6 3) ,HEAP0 ,ENV0 ,MT-TENV)) (term (2 ,HEAP0 ,ENV0 ,MT-TENV)))
  (test-->> run (term ((< 1 2) ,HEAP0 ,ENV0 ,MT-TENV)) (term (#t ,HEAP0 ,ENV0 ,MT-TENV)))
  (test-->> run (term ((% -10 3) ,HEAP0 ,ENV0 ,MT-TENV)) (term (-1 ,HEAP0 ,ENV0 ,MT-TENV)))
  (test-->> run
            (term ((+ (use a) 1) ,HEAP0 ,ENV0 ,MT-TENV)) ; a + 1
            (term (3 ,HEAP0 ,ENV0 ,MT-TENV)))
  (test-->> run
            (term ((+ 1 (use a)) ,HEAP0 ,ENV0 ,MT-TENV)) ; 1 + a
            (term (3 ,HEAP0 ,ENV0 ,MT-TENV)))
  (test-->> run
            (term ((+ (use a) (use a)) ,HEAP0 ,ENV0 ,MT-TENV))
            (term (4 ,HEAP0 ,ENV0 ,MT-TENV)))
  (test-->> run (term ((<< 8 2) ,HEAP0 ,ENV0 ,MT-TENV)) (term (32 ,HEAP0 ,ENV0 ,MT-TENV)))
  (test-->> run (term ((>> 8 2) ,HEAP0 ,ENV0 ,MT-TENV)) (term (2 ,HEAP0 ,ENV0 ,MT-TENV)))
  (test-->> run (term ((== 1 2) ,HEAP0 ,ENV0 ,MT-TENV)) (term (#f ,HEAP0 ,ENV0 ,MT-TENV)))
  (test-->> run (term ((!= 1 2) ,HEAP0 ,ENV0 ,MT-TENV)) (term (#t ,HEAP0 ,ENV0 ,MT-TENV)))
  (test-->> run (term ((- 15) ,HEAP0 ,ENV0 ,MT-TENV)) (term (-15 ,HEAP0 ,ENV0 ,MT-TENV)))
  (test-->> run (term ((! #t) ,HEAP0 ,ENV0 ,MT-TENV)) (term (#f ,HEAP0 ,ENV0 ,MT-TENV)))
  (test-->> run (term ((< 1 2) ,HEAP0 ,ENV0 ,MT-TENV)) (term (#t ,HEAP0 ,ENV0 ,MT-TENV)))
  (test-->> run (term ((cast misc a as float) ,HEAP0 ,ENV0 ,MT-TENV)) (term (2 ,HEAP0 ,ENV0 ,MT-TENV)))
  (test-results))

(rv-eval-tests)
;; =========================================================
;; env-lookup : ρ x -> α
(check-exn exn:fail? (λ () (term (env-lookup (env) x))) "env-lookup: variable not found in environment: x")
(check-exn exn:fail? (λ () (term (env-lookup (env (a 1)) c))) "env-lookup: address not found in environment: c")
(test-equal (term (env-lookup (env (a 1) (b 2) (c 3)) c)) 3)

;; =========================================================
;; store-lookup : σ α -> v
(check-exn exn:fail? (λ () (term (store-lookup (store) 1))) "store-lookup: address not found in store: 1")
(check-exn exn:fail? (λ () (term (store-lookup (store (1 1) 0)))) "store-lookup: address not found in store: 0")
(test-equal (term (store-lookup (store (1 111) (2 222) (3 333)) 3)) 333)

;; =========================================================
;; store-extend : σ α len -> σ
(test-equal (term (store-extend (store) 1 0)) (term (store)))
(test-equal (term (store-extend (store) 1 1)) (term [store (1 void)]))
(test-equal (term (store-extend ,HEAP0 19 1)) (term [store
                                                     (19 void)
                                                     (13 1)
                                                     (14 2)
                                                     (15 (ptr 14))
                                                     (16 void)
                                                     (17 (ptr 13))
                                                     (18 15)]))
(test-equal (term (store-extend ,HEAP0 19 5)) (term [store
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
;; sizeof : ty -> int 
(test-equal (term (sizeof (int float int))) (term 3))
(test-equal (term (sizeof (vec int 4))) (term 4))

;; =========================================================
;; deref : σ ρ x -> v
(test-equal (term (deref ,HEAP0 ,ENV0 x)) (term (ptr 14)))
(test-equal (term (deref ,HEAP0 ,ENV0 y)) (term void))

;; =========================================================
;; store-update : σ ρ x v -> σ
(test-equal (term (store-update ,HEAP0 ,ENV0 x 1)) ; x is at address 15
            (term [store
                   (13 1)
                   (14 2)
                   (15 1)
                   (16 void)
                   (17 (ptr 13))
                   (18 15)]))

;; =========================================================
;; store-remove : σ α len -> σ
(test-equal (term (store-remove (store (3 void)
                                       (2 void)
                                       (1 void)
                                       (0 void))
                                0 4))
            (term (store)))
(test-equal (term (store-remove (store (3 void)
                                       (2 void)
                                       (1 void)
                                       (0 void))
                                1 2))
            (term (store (3 void)
                         (0 void))))
(check-exn exn:fail? (λ () (term (store-remove (store) 0 1))) "store-remove: address not found in store: 1")

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
;; tenv-lookup : Γ x -> ty
(test-equal (term (tenv-lookup (tenv (a int) (b float)) a))
            (term int))
(check-exn exn:fail? (λ () (term (tenv-lookup (tenv) a)))
           "tenv-lookup: variable not found in type environment: a")

;; =========================================================
;; tenv-update : Γ x ty -> Γ
(test-equal (term (tenv-update (tenv (a int)) a float))
            (term (tenv (a float))))
(test-equal (term (tenv-update (tenv) a int))
            (term (tenv (a int))))

;; =========================================================
;; tenv-add : Γ x ty -> Γ
(test-equal (term (tenv-add (tenv) a int))
            (term (tenv (a int))))
; this function does not check that the variable has not been defined 
(test-equal (term (tenv-add (tenv (a int) (b int) (c int)) a float))
            (term (tenv (a float) (a int) (b int) (c int))))

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