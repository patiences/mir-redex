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
      (x rv ...) ; function call
      (rv ...) ;; aggregate values like tuples. This needs a better notation
      (. lv int)) ;; tuple/field access
  (type unit ; unit type
        i32
        bool
        ; aggregate types like tuples. Do they all need to be the same? 
        (type ...)) 
  (x variable-not-otherwise-mentioned))

;; tuples i.e. (1, 2).0  
;; ===================================================================
(define _0-decl (term (let mut _0 unit)))
(define scope-1 (term ((let _1 (i32 i32))
                       ((let _2 i32)) ;; scope 2
                       )))
(define _3-decl (term (let mut _3 i32)))
(define bb0 (term (bb0
                   ;; StorageLive(_1)
                   (= _1 ((const 1i32)  ;; tuple of 2 const i32s
                          (const 2i32)))
                   ;; StorageLive(_2)
                   ;; StorageLive(_3)
                   (= _3 (. _1 0)) ;; FIXME tuple access as an lv vs rv
                   ;; StorageDead(_3)
                   (= _0 ())
                   ;; StorageDead(_2)
                   ;; StorageDead(_1)
                   return)))
(define main (term (-> main() unit
                       ,_0-decl
                       ,scope-1
                       ,_3-decl
                       ,bb0)))

(redex-match mir scope scope-1)
(redex-match mir bb bb0)
(redex-match mir fn main)