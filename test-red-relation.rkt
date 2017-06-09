#lang racket
(require redex rackunit)
(require "mir.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Constants for testing 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Addresses
(define return-address (term (ptr ,(gensym))))
(define a0 (term (ptr ,(gensym))))
(define a1 (term (ptr ,(gensym))))
(define a2 (term (ptr ,(gensym))))
(define a3 (term (ptr ,(gensym))))
(define a4 (term (ptr ,(gensym))))

;; Programs and functions 
(define MT-MAIN (term [main () (let-bbs ([bb 0 (let-vars ()) return])) 0]))
(define PROG0 (term (,MT-MAIN)))

(define PROG1 (term ([main () (let-bbs ([bb 0 (let-vars ([= x (1 i32)]
                                                         [= y (use x)]
                                                         [= z ((100 i32) (200 i32) (300 i32))]))
                                            return]))
                           0])))

(define PROG2 (term ([main () (let-bbs ([bb 0 (let-vars ([= x (1 i32)]
                                                         [= y (use x)]
                                                         [= z ((100 i32) (200 i32) (300 i32))]))
                                            (goto 1)]
                                        [bb 1 (let-vars ([= foo (use x)]
                                                         [= bar (42 i32)]))
                                            return]))
                           0])))

(check-not-false (redex-match mir-machine prog PROG0))
(check-not-false (redex-match mir-machine prog PROG1))
(check-not-false (redex-match mir-machine prog PROG2))

;; Environments 
(define MT-ENV (term (env)))
(check-not-false (redex-match mir-machine ρ MT-ENV))

;; Stores 
(define MT-STORE (term (store)))
(define MT-STORE-WITH-RETURN (term (store [,return-address void])))
(define STORE0-ALLOC-ONLY (term (store [,return-address void]
                                       [,a0 void]
                                       [,a1 void]
                                       [,a2 void]
                                       [,a3 void]
                                       [,a4 void])))
(define STORE0 (term (store [,return-address void]
                            [,a0 void]
                            [,a1 ,a2]
                            [,a2 (5 i32)]
                            [,a3 void]
                            [,a4 (,a0 ,a2)])))
(check-not-false (redex-match mir-machine σ STORE0-ALLOC-ONLY))
(check-not-false (redex-match mir-machine σ STORE0))

;; Stack frames 
(define MT-FRM (term (frm)))
(define MT-FRM-WITH-RETURN (term (frm [return-ptr ,return-address])))
(define FRM0-ALLOC-ONLY (term (frm [return-ptr ,return-address]
                                   [a ,a0]
                                   [x ,a1]
                                   [y ,a2]
                                   [z ,a3]
                                   [xyz ,a4])))
(define FRM0 (term (frm
                    [return-ptr ,return-address]
                    [a ,a0]
                    [x ,a1]
                    [y ,a2]
                    [z ,a3]
                    [xyz ,a4])))
(check-not-false (redex-match mir-machine frame MT-FRM))
(check-not-false (redex-match mir-machine frame FRM0-ALLOC-ONLY))
(check-not-false (redex-match mir-machine frame FRM0))

;; Stacks 
(define MT-STK (term (stk)))
(define STK0-ALLOC-ONLY (term (stk ,FRM0-ALLOC-ONLY)))
(define STK0 (term (stk ,FRM0)))
(check-not-false (redex-match mir-machine δ STK0-ALLOC-ONLY))
(check-not-false (redex-match mir-machine δ STK0))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Reduction tests 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (function-call-tests)
  ;; These tests deal with function call executions. 
  (test--> run PROG0
           (term (,PROG0 (callfn main ()) ,MT-STORE ,MT-ENV ,MT-STK)))
  
  (define result_0 (car (apply-reduction-relation* run PROG0)))
  (define result (term (get-return-value-from-reduction ,result_0)))
  (test-equal result (term void))
  
  (define result_1 (car (apply-reduction-relation* run PROG1))) ; unwrap outer list
  (define stack_1 (term (get-stack ,result_1)))
  (define frame_1 (term (list-ref ,stack_1 1)))
  (define store_1 (term (get-store ,result_1)))
  (test-equal (term (size ,stack_1)) 1) ;; 1 frame created
  (test-equal (term (size ,frame_1)) 4) ;; 3 variables in the frame + return pointer
  (test-equal (term (size ,store_1)) 7) ;; 2 + (1 + 3) blocks of memory allocated + return value 
  (test-equal (term (is-allocated x ,store_1 ,frame_1)) #t)
  (test-equal (term (is-allocated y ,store_1 ,frame_1)) #t)
  (test-equal (term (is-allocated z ,store_1 ,frame_1)) #t)
  
  (define result_2 (car (apply-reduction-relation* run PROG2)))
  (define stack_2 (term (get-stack ,result_2)))
  (define frame_2 (term (list-ref ,stack_2 1)))
  (define store_2 (term (get-store ,result_2)))
  (test-equal (term (size ,stack_2)) 1)
  (test-equal (term (size ,frame_2)) 6)
  
  (test-results))

(define (bb-eval-tests)
  ;; These tests deal with basic block execution and control flow within a function call.
  
  ;; For tests that use STORE0-ALLOC-ONLY & MT-ENV & STK0-ALLOC-ONLY
  (define (wrap-test test-exp)
    (term (,PROG0 (in-call ,MT-MAIN ,test-exp) ,STORE0-ALLOC-ONLY ,MT-ENV ,STK0-ALLOC-ONLY)))
  
  (define (wrap-result result-exp result-store)
    (term (,PROG0 ,result-exp ,result-store ,MT-ENV ,STK0-ALLOC-ONLY)))
  
  (test-->> run (wrap-test (term [bb 0 (let-vars ([= a (1 i32)])) return]))
            (wrap-result (term void)
                         (term (store [,return-address void]
                                      [,a0 (1 i32)]
                                      [,a1 void]
                                      [,a2 void]
                                      [,a3 void]
                                      [,a4 void]))))
  (test-results))

(define (statement-eval-tests)
  ;; These tests deal with single statement execution within a basic block.
  
  ;; For tests that use STORE0-ALLOC-ONLY & MT-ENV & STK0-ALLOC-ONLY
  (define (wrap-test test-exp)
    (term (,PROG0 (in-call ,MT-MAIN ,test-exp) ,STORE0-ALLOC-ONLY ,MT-ENV ,STK0-ALLOC-ONLY)))
  
  (define (wrap-result result-exp result-store)
    (term (,PROG0 (in-call ,MT-MAIN ,result-exp) ,result-store ,MT-ENV ,STK0-ALLOC-ONLY)))
  
  (test-->> run (wrap-test (term (bb 0 (let-vars [(= a (1 i32))]) return)))
            (term (,PROG0 void (store [,return-address void]
                                      [,a0 (1 i32)]
                                      [,a1 void]
                                      [,a2 void]
                                      [,a3 void]
                                      [,a4 void])
                          ,MT-ENV ,STK0-ALLOC-ONLY)))
  (test-->> run (wrap-test (term (= a (1 i32))))
            (wrap-result (term void)
                         (term (store [,return-address void]
                                      [,a0 (1 i32)]
                                      [,a1 void]
                                      [,a2 void]
                                      [,a3 void]
                                      [,a4 void]))))
  (test-->> run (wrap-test (term (= a (+ (1 i32) (1 i32)))))
            (wrap-result (term void)
                         (term (store [,return-address void]
                                      [,a0 (2 i32)]
                                      [,a1 void]
                                      [,a2 void]
                                      [,a3 void]
                                      [,a4 void]))))
  
  ;; For tests that use STORE0 & MT-ENV & STK0
  (define (wrap exp)
    (term (,PROG0 (in-call ,MT-MAIN ,exp) ,STORE0 ,MT-ENV ,STK0)))
  
  (test-->> run (wrap (term (* x)))
            (wrap (term,a2)))
  (test-->> run (wrap (term (· xyz 1)))
            (wrap (term (5 i32))))
  (test-->> run (wrap (term (= (* x) (1 i32))))       ; *x = 1 
            (term (,PROG0
                   (in-call ,MT-MAIN void)
                   (store [,return-address void]
                          [,a0 void]
                          [,a1 ,a2]
                          [,a2 (1 i32)] ; *x
                          [,a3 void]
                          [,a4 (,a0 ,a2)])
                   ,MT-ENV ,STK0)))
  (test-->> run (wrap (term (let-vars ([= a (1 i32)] ; run multiple statements
                                       [= (* x) (2 i32)]))))
            (term (,PROG0
                   (in-call ,MT-MAIN void)
                   (store [,return-address void]
                          [,a0 (1 i32)]
                          [,a1 ,a2]
                          [,a2 (2 i32)] ; *x
                          [,a3 void]
                          [,a4 (,a0 ,a2)])
                   ,MT-ENV ,STK0)))
  (test-results))

(define (rv-eval-tests)
  ;; These tests deal with rvalue evaluation.
  ;; Rvalue reduction doesn't touch the PROG, so it's okay to use a mocked empty PROG.
  (define (wrap exp)
    (term (,PROG0 (in-call ,MT-MAIN ,exp) ,STORE0 ,MT-ENV ,STK0)))
  
  (test-->> run (wrap (term (use y))) (wrap (term (5 i32))))
  (test-->> run (wrap (term (use z))) (wrap (term void)))
  (test-->> run (wrap (term (& mut x))) (wrap a1))
  (test-->> run (wrap (term (+ (1 i32) (2 i32))))
            (wrap (term (3 i32))))
  (test-->> run (wrap (term (- (4 i32) (-20 i32))))
            (wrap (term (24 i32))))
  (test-->> run (wrap (term (* (5 i32) (6 i32))))
            (wrap (term (30 i32))))
  (test-->> run (wrap (term (< (1 i32) (2 i32)))) (wrap #t))
  (test-->> run (wrap (term (% (-10 i32) (3 i32))))
            (wrap (term (-1 i32))))
  (test-->> run (wrap (term (! #t))) (wrap (term #f)))
  (test-->> run (wrap (term (+ (use y) (1 i32)))) 
            (wrap (term (6 i32))))
  (test-->> run (wrap (term (+ (1 i32) (use y))))
            (wrap (term (6 i32))))
  (test-->> run (wrap (term (+ (use y) (use y)))) (wrap (term (10 i32))))
  (test-results))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Metafunction tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; deref : σ ρ x -> v
;; =========================================================
(test-equal (term (deref ,STORE0 ,STK0 x)) (term ,a2))
(test-equal (term (deref ,STORE0 ,STK0 y)) (term (5 i32)))
; dereferencing an aggregate value gets a list of addresses
(test-equal (term (deref ,STORE0 ,STK0 xyz)) (term (,a0 ,a2)))

;; deref-projection : σ frame x f -> v
;; =========================================================
(test-equal (term (deref-projection ,STORE0 ,STK0 xyz 1))
            (term (5 i32)))

;; frm-lookup : frame x -> frm-v
;; =========================================================
(check-exn exn:fail? (λ () (term (frm-lookup (frm) x))) "frm-lookup: variable not found in frame x")
(test-equal (term (frm-lookup ,FRM0 z)) (term ,a3))

;; store-lookup : σ α -> v
;; =========================================================
(check-exn exn:fail? (λ () (term (store-lookup (store) 1))) "store-lookup: address not found in store: 1")
(check-exn exn:fail? (λ () (term (store-lookup ,STORE0 (gensym)))) "store-lookup: address not found in store: 0")
(test-equal (term (store-lookup ,STORE0 ,a2)) (term (5 i32)))

;; store-update-aggregate : σ δ α (v ...) -> σ
;; =========================================================
(define STORE-WITH-AGGREGATE-VALUE (term (store [,a0 (,a1 ,a2 ,a3)]
                                                [,a1 void]
                                                [,a2 void]
                                                [,a3 void])))
(define FRAME-WITH-AGGREGATE-VALUE (term (frm [x ,a0])))
(check-not-false (redex-match mir-machine σ STORE-WITH-AGGREGATE-VALUE))
(check-not-false (redex-match mir-machine frame FRAME-WITH-AGGREGATE-VALUE))

(test-equal (term (store-update-aggregate ,STORE-WITH-AGGREGATE-VALUE (stk ,FRAME-WITH-AGGREGATE-VALUE)
                                          x ((1 i32) (2 i32) (3 i32))))
            (term (store [,a0 (,a1 ,a2 ,a3)]
                         [,a1 (1 i32)]
                         [,a2 (2 i32)]
                         [,a3 (3 i32)])))

;; list-ref : any idx -> any
;; =========================================================
(test-equal (term (list-ref (0 1 2) 0)) (term 0))
(test-equal (term (list-ref ((a 0) (b 0) (c 0) (d 0)) 3)) (term (d 0)))
(check-exn exn:fail? (λ () (term (list-ref () 1))) "list-ref: index not in range: 1")

;; lookup-bb : bbs idx -> blk
;; =========================================================
(test-equal (term (lookup-bb (let-bbs ([bb 0 (let-vars ()) return])) 0))
            (term [bb 0 (let-vars ()) return]))
(test-equal (term (lookup-bb (let-bbs ([bb 0 (let-vars ()) return]
                                       [bb 1 (let-vars ()) return]
                                       [bb 2 (let-vars ()) return]))
                             2))
            (term [bb 2 (let-vars ()) return]))
(check-exn exn:fail? (λ () (term (lookup-bb (let-bbs ()) 1))) "lookup-bb: basic block with index not found: 1")

;; lookup-fn : prog g -> fn
;; =========================================================
(test-equal (term (lookup-fn ,PROG0 main)) MT-MAIN)
(check-exn exn:fail? (λ () (term (lookup-fn () main))) "lookup-fn: function with name not found: main")

;; alloc-vars-in-fn : fn σ δ -> (σ δ)
;; =========================================================
(define alloc_vars (term (alloc-vars-in-fn (main () (let-bbs ([bb 0 (let-vars ([= x (1 i32)])) return]
                                                              [bb 1 (let-vars ([= foo (6 u64)]
                                                                               [= foo (7 u64)]))
                                                                  return]))
                                                 0)
                                           ,MT-STORE ,MT-STK)))
(define new_store (car alloc_vars))
(define new_stack (cadr alloc_vars))
(define new_frame (term (list-ref ,new_stack 1)))
(test-equal (term (is-allocated x ,new_store ,new_frame)) #t)
(test-equal (term (is-allocated foo ,new_store ,new_frame)) #t)
(test-equal (term (size ,new_stack)) 1)
(test-equal (term (size ,new_frame)) 3) ; 2 variables plus return pointer 
(test-equal (term (size ,new_store)) 3) ; 2 variables plus return value 

;; alloc-vars-in-fn-helper : fn σ frame -> (σ frame)
;; =========================================================
(define alloc_new_bbs (term (alloc-vars-in-fn-helper (main () (let-bbs ([bb 0 (let-vars ([= x (1 i32)])) return]
                                                                        [bb 1 (let-vars ([= foo (6 u64)]
                                                                                         [= bar (7 u64)]))
                                                                            return]))
                                                           0)
                                                     ,MT-STORE ,MT-FRM)))

(define σ_new (car alloc_new_bbs))
(define frm_new (cadr alloc_new_bbs))
(test-equal (term (is-allocated x ,σ_new ,frm_new)) #t)
(test-equal (term (is-allocated foo ,σ_new ,frm_new)) #t)
(test-equal (term (is-allocated bar ,σ_new ,frm_new)) #t)
(test-equal (term (size ,frm_new)) 3)

;; alloc-vars-in-bb : blk σ frame -> (σ frame)
;; =========================================================
(define alloc_new_vars (term (alloc-vars-in-bb [bb 0 (let-vars ([= x (1 i32)])) return]
                                               ,MT-STORE ,MT-FRM)))
(define store_new (car alloc_new_vars))
(define frame_new (cadr alloc_new_vars))
(test-equal (term (is-allocated x ,store_new ,frame_new)) #t)

(define alloc_more_new_vars (term (alloc-vars-in-bb [bb 1 (let-vars ([= foo (6 u64)]
                                                                     [= bar (7 u64)]))
                                                        return]
                                                    ,store_new ,frame_new)))
(define store_newer (car alloc_more_new_vars))
(define frame_newer (cadr alloc_more_new_vars))
(test-equal (term (is-allocated foo ,store_newer ,frame_newer)) #t)
(test-equal (term (is-allocated bar ,store_newer ,frame_newer)) #t)

;; alloc-var : lv σ frame -> (σ frame)
;; =========================================================
(define alloc_new (term (alloc-var new_variable (1 i32) ,MT-STORE ,MT-FRM)))
(test-equal (term (is-allocated new_variable ,(car alloc_new) ,(cadr alloc_new))) #t)

; variable already exists, don't allocate
(define do_not_alloc_new (term (alloc-var old_variable (1 i32) (store (,a1 void)) (frm [old_variable ,a1]))))
(test-equal do_not_alloc_new
            (term ((store (,a1 void)) (frm [old_variable ,a1]))))
(test-equal (term (is-allocated old_variable ,(car do_not_alloc_new) ,(cadr do_not_alloc_new))) #t)

; allocate enough space for a tuple
(define alloc_tuple (term (alloc-var my_tuple ((1 i32) (2 i32)) ,MT-STORE ,MT-FRM)))
(define new_store_with_tuple (car alloc_tuple))
(define new_frame_with_tuple (cadr alloc_tuple))
(test-equal (term (is-allocated my_tuple ,new_store_with_tuple ,new_frame_with_tuple)) #t)
(test-equal (term (size ,new_store_with_tuple)) 3)
(test-equal (term (size ,new_frame_with_tuple)) 1)

;; malloc : σ -> (σ α)
;; =========================================================
(define malloc_one (term (malloc ,MT-STORE 1)))
(define store_1 (car malloc_one))
(test-equal (term (size ,store_1)) 1)

(define malloc_three_more (term (malloc ,store_1 3)))
(define store_2 (car malloc_three_more))
(test-equal (term (size ,store_2)) 4)

;; is-allocated : x σ frame -> boolean
;; =========================================================
(test-equal (term (is-allocated x (store (,a1 void)) (frm [x ,a1]))) #t) 
(test-equal (term (is-allocated x (store) (frm))) #f)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Run tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(display "Metafunction unit tests: ")
(test-results)

(display "Function call tests: ")
(function-call-tests)

(display "Basic block tests: ")
(bb-eval-tests)

(display "Statement tests: ")
(statement-eval-tests)

(display "Rvalue tests: ")
(rv-eval-tests)
