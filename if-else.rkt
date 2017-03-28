#lang racket
(require redex)

(define-language mir
  (fns (fn fn ...))
  (fn (-> x((x: type) ...) type
          binding ...
          bb ...))
  (binding (let mut x type)
           (let x type)
           scope)
  (scope (binding ...))
  (bb (x
       statement ...
       terminator))
  (statement (= lv rv))
  (terminator return
              (switch lv (lv x) ...)
              (goto x))
  (lv x)
  (rv (lv ...)
      (const x) ; FIXME
      (x rv ...)) ; function call
  (type unit ; unit type
        i32
        bool)
  (x variable-not-otherwise-mentioned))

;; if else  
;; ===================================================================
(define _0-decl (term (let mut _0 unit)))
(define scope-1 (term ((let mut _1 i32))))
(define _2-decl (term (let mut _2 bool)));
(define _2-assignment (term (= _2 (Lt (const 3i32) (const 4i32)))))

(define bb0 (term (bb0
                   ;; StorageLive(_1)
                   ;; StorageLive(_2)
                   ,_2-assignment
                   (switch _2 (0u8 bb2) (otherwise bb1)))))                   
(define bb1 (term (bb1 (= _1 (const 1i32))
                       (= _0 ())
                       (goto bb3))))
(define bb2 (term (bb2 (= _1 (const 0i32))
                       (= _0 ())
                       (goto bb3))))
(define bb3 (term (bb3
                   ;; StorageDead(_2)
                   ;; StorageDead(_1)
                   return)))
(define main (term (-> main() unit
                       ,_0-decl
                       ,scope-1
                       ,_2-decl
                       ,bb0
                       ,bb1
                       ,bb2
                       ,bb3)))

(redex-match mir bb bb0)
(redex-match mir bb bb1)
(redex-match mir bb bb2)
(redex-match mir bb bb3)
(redex-match mir fn main)