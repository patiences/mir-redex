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
(define a4 (term (ptr ,(gensym))))

(define PROG0 (term ([main () (let-bbs ([bb 0 (let-vars ()) return])) 0])))
(check-not-false (redex-match mir-machine prog PROG0))

(define MT-ENV (term (env)))
(check-not-false (redex-match mir-machine ρ MT-ENV))

(define MT-STORE (term (store)))
(define STORE0 (term (store [,a0 void]
                            [,a1 ,a2]
                            [,a2 (5 i32)]
                            [,a3 void]
                            [,a4 (,a0 ,a2)])))
(check-not-false (redex-match mir-machine σ STORE0))

(define MT-FRM (term (frm)))
(define FRM0 (term [frm 
                    (a ,a0)
                    (x ,a1)
                    (y ,a2)
                    (z ,a3)
                    (xyz ,a4)]))
(check-not-false (redex-match mir-machine frame MT-FRM))

(define MT-STK (term (stk)))
(define STK0 (term (stk ,FRM0)))
(check-not-false (redex-match mir-machine stack STK0))

;; Reduction tests
;; =========================================================
(define (function-call-tests)
  (test-->> run PROG0
        (term (,PROG0 (callfn main ()) ,MT-STORE ,MT-ENV ,MT-STK)))
  (test-results))

(function-call-tests)

(define (statement-eval-tests)
  (test-->> run (term ((main ()
                             (let-bbs ([bb 0 (let-vars ([= a (1 i32)])) return]))
                             0)
                       ,STORE0 ,MT-ENV ,STK0))
            (term (void
                   (store [,a0 (1 i32)]
                          [,a1 ,a2]
                          [,a2 (5 i32)]
                          [,a3 void]
                          [,a4 (,a0 ,a2)])
                   ,MT-ENV ,STK0)))
  (test-->> run (term ((bb 0 (let-vars [(= a (1 i32))]) return) ,STORE0 ,MT-ENV ,STK0))
            (term (void
                   (store [,a0 (1 i32)]
                          [,a1 ,a2]
                          [,a2 (5 i32)]
                          [,a3 void]
                          [,a4 (,a0 ,a2)])
                   ,MT-ENV ,STK0)))
  (test-->> run (term ((= a (1 i32)) ,STORE0 ,MT-ENV ,STK0))
            (term (void (store [,a0 (1 i32)]
                               [,a1 ,a2]
                               [,a2 (5 i32)]
                               [,a3 void]
                               [,a4 (,a0 ,a2)])
                        ,MT-ENV ,STK0)))
  (test-->> run (term ((= a (+ (1 i32) (1 i32))) ,STORE0 ,MT-ENV ,STK0))
            (term (void (store [,a0 (2 i32)]
                               [,a1 ,a2]
                               [,a2 (5 i32)]
                               [,a3 void]
                               [,a4 (,a0 ,a2)])
                        ,MT-ENV ,STK0)))
  (test-->> run (term ((* x) ,STORE0 ,MT-ENV ,STK0))
            (term (,a2 ,STORE0 ,MT-ENV ,STK0)))
  (test-->> run (term ((· xyz 1) ,STORE0 ,MT-ENV ,STK0))
            (term ((5 i32) ,STORE0 ,MT-ENV ,STK0)))
  (test-->> run (term ((= (* x) (1 i32)) ,STORE0 ,MT-ENV ,STK0)) ; *x = 1 
            (term (void (store [,a0 void]
                               [,a1 ,a2]
                               [,a2 (1 i32)] ; *x
                               [,a3 void]
                               [,a4 (,a0 ,a2)])
                        ,MT-ENV ,STK0)))
  (test-->> run (term ((let-vars ([= a (1 i32)] ; run multiple statements
                                  [= (* x) (2 i32)]))
                       ,STORE0 ,MT-ENV ,STK0))
            (term (void (store [,a0 (1 i32)]
                               [,a1 ,a2]
                               [,a2 (2 i32)] ; *x
                               [,a3 void]
                               [,a4 (,a0 ,a2)])
                        ,MT-ENV ,STK0)))
  (test-results))

(statement-eval-tests)

(define (rv-eval-tests)
  (test-->> run (term ((use y) ,STORE0 ,MT-ENV ,STK0))
            (term ((5 i32) ,STORE0 ,MT-ENV ,STK0)))
  (test-->> run (term ((use z) ,STORE0 ,MT-ENV ,STK0))
            (term (void ,STORE0 ,MT-ENV ,STK0)))
  (test-->> run (term ((& mut x) ,STORE0 ,MT-ENV ,STK0))
            (term (,a1 ,STORE0 ,MT-ENV ,STK0)))
  (test-->> run (term ((+ (1 i32) (2 i32)) ,STORE0 ,MT-ENV ,STK0)) (term ((3 i32) ,STORE0 ,MT-ENV ,STK0)))
  (test-->> run (term ((- (4 i32) (-20 i32)) ,STORE0 ,MT-ENV ,STK0)) (term ((24 i32) ,STORE0 ,MT-ENV ,STK0)))
  (test-->> run (term ((* (5 i32) (6 i32)) ,STORE0 ,MT-ENV ,STK0)) (term ((30 i32) ,STORE0 ,MT-ENV ,STK0)))
  (test-->> run (term ((< (1 i32) (2 i32)) ,STORE0 ,MT-ENV ,STK0)) (term (#t ,STORE0 ,MT-ENV ,STK0)))
  (test-->> run (term ((% (-10 i32) (3 i32)) ,STORE0 ,MT-ENV ,STK0)) (term ((-1 i32) ,STORE0 ,MT-ENV ,STK0)))
  (test--> run (term ((! #t) ,STORE0 ,MT-ENV ,STK0)) (term (#f ,STORE0 ,MT-ENV ,STK0)))
  (test-->> run
            (term ((+ (use y) (1 i32)) ,STORE0 ,MT-ENV ,STK0)) ; y + 1
            (term ((6 i32) ,STORE0 ,MT-ENV ,STK0)))
  (test-->> run
            (term ((+ (1 i32) (use y)) ,STORE0 ,MT-ENV ,STK0)) ; 1 + y
            (term ((6 i32) ,STORE0 ,MT-ENV ,STK0)))
  (test-->> run
            (term ((+ (use y) (use y)) ,STORE0 ,MT-ENV ,STK0))
            (term ((10 i32) ,STORE0 ,MT-ENV ,STK0)))
  (test-results))

(rv-eval-tests)

;; Metafunction tests
;; =========================================================
;; deref : σ ρ x -> v
(test-equal (term (deref ,STORE0 ,STK0 x)) (term ,a2))
(test-equal (term (deref ,STORE0 ,STK0 y)) (term (5 i32)))
; dereferencing an aggregate value gets a list of addresses
(test-equal (term (deref ,STORE0 ,STK0 xyz)) (term (,a0 ,a2)))

;; =========================================================
;; deref-projection : σ frame x f -> v
(test-equal (term (deref-projection ,STORE0 ,STK0 xyz 1))
            (term (5 i32)))

;; =========================================================
;; frm-lookup : frame x -> frm-v
(check-exn exn:fail? (λ () (term (frm-lookup (frm) x))) "frm-lookup: variable not found in frame x")
(test-equal (term (frm-lookup ,FRM0 z)) (term ,a3))

;; =========================================================
;; store-lookup : σ α -> v
(check-exn exn:fail? (λ () (term (store-lookup (store) 1))) "store-lookup: address not found in store: 1")
(check-exn exn:fail? (λ () (term (store-lookup ,STORE0 (gensym)))) "store-lookup: address not found in store: 0")
(test-equal (term (store-lookup ,STORE0 ,a2)) (term (5 i32)))

;; =========================================================
;; list-ref : any idx -> any
(test-equal (term (list-ref (0 1 2) 0)) (term 0))
(test-equal (term (list-ref ((a 0) (b 0) (c 0) (d 0)) 3)) (term (d 0)))
(check-exn exn:fail? (λ () (term (list-ref () 1))) "list-ref: index not in range: 1")

;; =========================================================
;; lookup-bb : bbs idx -> blk
(test-equal (term (lookup-bb (let-bbs ([bb 0 (let-vars ()) return])) 0))
            (term [bb 0 (let-vars ()) return]))
(test-equal (term (lookup-bb (let-bbs ([bb 0 (let-vars ()) return]
                                       [bb 1 (let-vars ()) return]
                                       [bb 2 (let-vars ()) return]))
                             2))
            (term [bb 2 (let-vars ()) return]))
(check-exn exn:fail? (λ () (term (lookup-bb (let-bbs ()) 1))) "lookup-bb: basic block with index not found: 1")


(test-results)