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
      (x rv ...) ; Function call
      (struct x (x rv) ...))
  (type unit ; unit type
        i32
        bool
        x) ; struct types 
  (x variable-not-otherwise-mentioned))


;; struct creation 
;; ===================================================================
(define _0-decl (term (let mut _0 unit)))
(define scope-1 (term ((let _1 Point))))
(define bb0 (term (bb0
                  ; StorageLive(_1)
                  (= _1 (struct Point
                          (x (const 4i32))
                          (y (const 5i32))))
                  (= _0 ())
                  ;; StorageDead(_1)
                  return)))

(redex-match mir bb bb0)
                               