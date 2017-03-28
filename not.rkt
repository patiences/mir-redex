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
      (const x) ; FIXME
      (x rv ...)) ; function call
  (type unit ; unit type
        i32
        bool)
  (x variable-not-otherwise-mentioned))

;; not 
;; ===================================================================
(define _0-assignment (term (= _0 ()))) ; _0 = ();
(define return (term return)) ; return;
(define _0-decl (term (let mut _0 unit))) ; let mut _0: ();
(define scope-1 (term ((let _1 bool)))) ; let _1: bool;
(define _1-assignment (term (= _1 (Not (const true)))))
(define bb0 (term (bb0
                   ;; StorageLive(_1)
                   ,_1-assignment
                   ,_0-assignment
                   ;; StorageDead(_1)
                   ,return)))
(define main (term (-> main() unit
                       ,_0-decl
                       ,scope-1
                       ,bb0)))

(redex-match mir statement _1-assignment)
(redex-match mir bb bb0)
(redex-match mir fn main)