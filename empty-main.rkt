#lang racket
(require redex)

(define-language mir
  (fns (fn fn ...))
  (fn (-> x((x: type) ...) type
          binding ...
          bb ...))
  (binding (let mut x type)
           (let x type))
  (bb (x
       statement ...
       terminator))
  (statement (= lv rv))
  (terminator return)
  (lv x)
  (rv (lv ...))
  (type unit) ; unit type
  (x variable-not-otherwise-mentioned))

;; simple empty function(s) 
;; ===================================================================
(define _0-assignment (term (= _0 ()))) ; _0 = ();
(define return (term return)) ; return;
(define bb0 (term (bb0 ,_0-assignment ,return))) ; bb0: { _0 = (); return; }

(define _0-decl (term (let mut _0 unit))) ; let mut _0: ();

(define main0 (term (-> main() unit
                       ,_0-decl
                       ,bb0)))
(redex-match mir statement _0-assignment)
(redex-match mir terminator return)
(redex-match mir bb bb0)
(redex-match mir binding _0-decl)
(redex-match mir fn main0) ; single empty function 
(redex-match mir fns (term (,main0 ,main0 ,main0))) ; multiple empty functions