#lang racket
(require redex)

(define-language mir
  (fns (fn fn ...))
  (fn (-> x((x: type) ...) type
          binding ...
          scope ...
          bb ...))
  (binding (let mut x type)
           (let x type))
  (scope (binding ...))
  (bb (x
       statement ...
       terminator))
  (statement (= lv rv))
  (terminator return)
  (lv x)
  (rv (lv ...)
      (const x)) ; FIXME
  (type unit ; unit type
        i32)
  (x variable-not-otherwise-mentioned))

;; simple if else 
;; ===================================================================
(define _0-decl (term (let mut _0 unit))) ; let mut _0: ();
(define scope-1 (term ((let mut _1 i32))))
(define bb0 (term (bb0
                   ;; StorageLive(_1)
                   (= _1 (const 1i32))
                   (= _0 ())
                   ;; StorageDead(_1)
                   return)))
(define main1 (term (-> main() unit
                        ,_0-decl
                        ,scope-1
                        ,bb0)))

(redex-match mir binding _0-decl)
(redex-match mir scope scope-1)
(redex-match mir bb bb0)
(redex-match mir fn main1)