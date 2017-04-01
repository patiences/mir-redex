#lang racket
(require redex)

(define-language mir
  (fns (fn fn ...))
  (fn (-> x((x : type) ...) type
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
  (rv (use lv)
      (lv ...)
      (const x) ; FIXME
      (x rv ...)) ; function call
  (type unit ; unit type
        i32
        bool)
  (x variable-not-otherwise-mentioned))

;; logical or  
;; ===================================================================
(define _0-decl (term (let mut _0 unit)))
(define _1-decl (term (let mut _1 bool)))
(define _1-assignment (term (= _1 (foo (const 0i32)
                                       (const 10i32)
                                       (const 5i32)))))
(define bb0 (term (bb0
                   ,_1-assignment
                   (goto bb1))))
(define bb1 (term (bb1 (= _0 ()) return)))

(redex-match mir bb bb0)
(redex-match mir bb bb1)

(define _0-decl-foo (term (let mut _0 bool)))
(define scope-1-foo (term ((let _4 i32)
                       (let _5 i32)
                       (let _6 i32))))
(define _7-decl-foo (term (let mut _7 bool)))
(define _8-decl-foo (term (let mut _8 i32)))
(define _9-decl-foo (term (let mut _9 i32)))
(define _10-decl-foo (term (let mut _10 bool)))
(define _11-decl-foo (term (let mut _11 i32)))
(define _12-decl-foo (term (let mut _12 i32)))
(define bb0-foo (term (bb0
                       ;; StorageLive(_4)
                       (= _4 (use _1))
                       ;; StorageLive(_5)
                       (= _5 (use _2))
                       ;; StorageLive(_6)
                       (= _6 (use _3))
                       ;; StorageLive(_7)
                       ;; StorageLive(_8)
                       (= _8 (use _4))
                       ;; StorageLive(_9)
                       (= _9 (use _6))
                       (= _7 (Lt (use _8) (use _9)))
                       ;; StorageDead(_9)
                       ;; StorageDead(_8)
                       (switch _7 (0u8 bb3) (otherwise bb1)))))
(define bb1-foo (term (bb1 (= _0 (const true)) (goto bb4))))
(define bb2-foo (term (bb2 (= _0 (const false)) (goto bb4))))
(define bb3-foo (term (bb3
                       ;; StorageLive(_10)
                       ;; StorageLive(_11)
                       (= _11 (use _6))
                       ;; StorageLive(_12)
                       (= _12 (use _5))
                       (= _10 (Lt (use _11) (use _12)))
                       ;; StorageDead(_12)
                       ;; StorageDead(_11)
                       (switch _10 (0u8 bb2) (otherwise bb1)))))
(define bb4-foo (term (bb4
                       ;; StorageDead(_10)
                       ;; StorageDead(_7)
                       ;; StorageDead(_6)
                       ;; StorageDead(_5)
                       ;; StorageDead(_4)
                       return)))

(redex-match mir bb bb0-foo)
(redex-match mir bb bb1-foo)
(redex-match mir bb bb2-foo)
(redex-match mir bb bb3-foo)
(redex-match mir bb bb4-foo)

(define main (term (-> main() unit
                       ,_0-decl
                       ,_1-decl
                       ,bb0
                       ,bb1)))
(define foo (term (-> foo((_1 : i32) (_2 : i32) (_3 : i32)) bool
                      ,_0-decl-foo
                      ,scope-1-foo
                      ,_7-decl-foo
                      ,_8-decl-foo
                      ,_9-decl-foo
                      ,_10-decl-foo
                      ,_11-decl-foo
                      ,_12-decl-foo
                      ,bb0-foo
                      ,bb1-foo
                      ,bb2-foo
                      ,bb3-foo
                      ,bb4-foo)))
(redex-match mir fn main)
(redex-match mir fn foo)

(redex-match mir fns (term (,main ,foo)))