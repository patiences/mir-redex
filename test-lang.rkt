#lang racket
(require redex
         rackunit)
(require "mir.rkt")

;; Empty main function 
;; ======================================================
;fn main() -> () {
;    let mut _0: ();                      // return pointer
;
;    bb0: {
;        _0 = ();                         // scope 0 at <anon>:1:15: 3:2
;        return;                          // scope 0 at <anon>:3:2: 3:2
;    }
;}
(define empty-main
  (term (main () (let-bbs ([bb 0 (let-vars ([= _0 unit])) return])) 0)))

(check-not-false (redex-match mir fn empty-main))

;; Simple if-else function 
;; ======================================================
;fn main() -> () {
;    let mut _0: ();                      // return pointer
;    scope 1 {
;        let mut _1: i32;                 // "a" in scope 1 at <anon>:2:9: 2:14
;    }
;
;    bb0: {
;        StorageLive(_1);                 // scope 0 at <anon>:2:9: 2:14
;        _1 = const 1i32;                 // scope 1 at <anon>:4:9: 4:14
;        _0 = ();                         // scope 1 at <anon>:3:13: 5:6
;        StorageDead(_1);                 // scope 0 at <anon>:8:2: 8:2
;        return;                          // scope 0 at <anon>:8:2: 8:2
;    }
;}
(define if-else-simple
  (term (main () (let-bbs ([bb 0 (let-vars ([= _1 (1 i32)] [= _0 unit])) return])) 0))) 

(check-not-false (redex-match mir fn if-else-simple))