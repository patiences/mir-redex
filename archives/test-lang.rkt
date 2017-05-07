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
  (term (-> main () unit-ty
            (mut _0 : unit-ty)
            (bb 0
                [(= _0 unit)]
                return))))

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
  (term (-> main () unit-ty
            (mut _0 : unit-ty)
            (scope 1 (mut _1 : int))
            (bb 0
                [(= _1 1)
                 (= _0 unit)]
                return))))

(check-not-false (redex-match mir fn if-else-simple))

;; Simple struct creation 
;; ======================================================
;fn main() -> () {
;    let mut _0: ();                      // return pointer
;    scope 1 {
;        let _1: Point;                   // "a" in scope 1 at <anon>:7:9: 7:10
;    }
;
;    bb0: {
;        StorageLive(_1);                 // scope 0 at <anon>:7:9: 7:10
;        _1 = Point { x: const 4i32, y: const 5i32 }; // scope 0 at <anon>:7:13:7:31
;        _0 = ();                         // scope 1 at <anon>:6:11: 8:2
;        StorageDead(_1);                 // scope 0 at <anon>:8:2: 8:2
;        return;                          // scope 0 at <anon>:8:2: 8:2
;    }
;}
(define struct-creation
  (term (-> main () unit-ty
            (mut _0 : unit-ty)
            (scope 1 (_1 : (struct Point)))
            (bb 0
                [(= _1 (struct Point [(= x 4)
                                      (= y 5)]))
                 (= _0 unit)]
                return))))

(check-not-false (redex-match mir fn struct-creation))

;; Not function 
;; ======================================================
;fn main() -> () {
;    let mut _0: ();                      // return pointer
;    scope 1 {
;        let _1: bool;                    // "x" in scope 1 at <anon>:2:9: 2:10
;    }
;
;    bb0: {
;        StorageLive(_1);                 // scope 0 at <anon>:2:9: 2:10
;        _1 = Not(const true);            // scope 0 at <anon>:2:13: 2:18
;        _0 = ();                         // scope 1 at <anon>:1:11: 4:2
;        StorageDead(_1);                 // scope 0 at <anon>:4:2: 4:2
;        return;                          // scope 0 at <anon>:4:2: 4:2
;    }
;}

(define not
  (term (-> main () unit-ty
            (mut _0 : unit-ty)
            (scope 1 (_1 : bool))
            (bb 0
                [(= _1 (! #true)) 
                 (= _0 unit)]
                return))))

(check-not-false (redex-match mir fn not))

;; Logical or 
;; ======================================================
;fn main() -> () {
;    let mut _0: ();                      // return pointer
;    let mut _1: bool;
;
;    bb0: {
;        _1 = foo(const 0i32, const 10i32, const 5i32) -> bb1; // scope 0 at <anon>:6:5: 6:18
;    }
;
;    bb1: {
;        _0 = ();                         // scope 0 at <anon>:5:15: 7:2
;        return;                          // scope 0 at <anon>:7:2: 7:2
;    }
;}
;
;fn foo(_1: i32, _2: i32, _3: i32) -> bool {
;    let mut _0: bool;                    // return pointer
;    scope 1 {
;        let _4: i32;                     // "a" in scope 1 at <anon>:1:8: 1:9
;        let _5: i32;                     // "b" in scope 1 at <anon>:1:16: 1:17
;        let _6: i32;                     // "x" in scope 1 at <anon>:1:24: 1:25
;    }
;    let mut _7: bool;
;    let mut _8: i32;
;    let mut _9: i32;
;    let mut _10: bool;
;    let mut _11: i32;
;    let mut _12: i32;
;
;    bb0: {
;        StorageLive(_4);                 // scope 0 at <anon>:1:8: 1:9
;        _4 = _1;                         // scope 0 at <anon>:1:8: 1:9
;        StorageLive(_5);                 // scope 0 at <anon>:1:16: 1:17
;        _5 = _2;                         // scope 0 at <anon>:1:16: 1:17
;        StorageLive(_6);                 // scope 0 at <anon>:1:24: 1:25
;        _6 = _3;                         // scope 0 at <anon>:1:24: 1:25
;        StorageLive(_7);                 // scope 1 at <anon>:2:5: 2:10
;        StorageLive(_8);                 // scope 1 at <anon>:2:5: 2:6
;        _8 = _4;                         // scope 1 at <anon>:2:5: 2:6
;        StorageLive(_9);                 // scope 1 at <anon>:2:9: 2:10
;        _9 = _6;                         // scope 1 at <anon>:2:9: 2:10
;        _7 = Lt(_8, _9);                 // scope 1 at <anon>:2:5: 2:10
;        StorageDead(_9);                 // scope 1 at <anon>:2:10: 2:10
;        StorageDead(_8);                 // scope 1 at <anon>:2:10: 2:10
;        switchInt(_7) -> [0u8: bb3, otherwise: bb1]; // scope 1 at <anon>:2:5: 2:19
;    }
;
;    bb1: {
;        _0 = const true;                 // scope 1 at <anon>:2:5: 2:19
;        goto -> bb4;                     // scope 1 at <anon>:2:5: 2:19
;    }
;
;    bb2: {
;        _0 = const false;                // scope 1 at <anon>:2:5: 2:19
;        goto -> bb4;                     // scope 1 at <anon>:2:5: 2:19
;    }
;
;    bb3: {
;        StorageLive(_10);                // scope 1 at <anon>:2:14: 2:19
;        StorageLive(_11);                // scope 1 at <anon>:2:14: 2:15
;        _11 = _6;                        // scope 1 at <anon>:2:14: 2:15
;        StorageLive(_12);                // scope 1 at <anon>:2:18: 2:19
;        _12 = _5;                        // scope 1 at <anon>:2:18: 2:19
;        _10 = Lt(_11, _12);              // scope 1 at <anon>:2:14: 2:19
;        StorageDead(_12);                // scope 1 at <anon>:2:19: 2:19
;        StorageDead(_11);                // scope 1 at <anon>:2:19: 2:19
;        switchInt(_10) -> [0u8: bb2, otherwise: bb1]; // scope 1 at <anon>:2:5: 2:19
;    }
;
;    bb4: {
;        StorageDead(_10);                // scope 1 at <anon>:2:19: 2:19
;        StorageDead(_7);                 // scope 1 at <anon>:2:19: 2:19
;        StorageDead(_6);                 // scope 0 at <anon>:3:2: 3:2
;        StorageDead(_5);                 // scope 0 at <anon>:3:2: 3:2
;        StorageDead(_4);                 // scope 0 at <anon>:3:2: 3:2
;        return;                          // scope 1 at <anon>:3:2: 3:2
;    }
;}
;        _1 = foo(const 0i32, const 10i32, const 5i32) -> bb1; // scope 0 at <anon>:6:5: 6:18

(define logical-or
  (term [(-> main () unit-ty
             (mut _0 : unit-ty)
             (mut _1 : bool)
             (bb 0 [] (call _1
                              foo [0 10 5]
                              1))
             (bb 1 [(= _0 unit)] return))
         (-> foo [(_1 : int) (_2 : int) (_3 : int)] bool
             (mut _0 : unit-ty)
             (scope 1
                    (_4 : int)
                    (_5 : int)
                    (_6 : int))
             (mut _7 : bool)
             (mut _8 : int)
             (mut _9 : bool)
             (mut _10 : int)
             (mut _11 : int)
             (mut _12 : int)
             (bb 0
                 [(= _4 (use _1))
                  (= _5 (use _2))
                  (= _6 (use _3))
                  (= _8 (use _4))
                  (= _9 (use _6))
                  (= _7 (< (use _8) (use _9)))]
                 (switchInt _10 (0 3) (otherwise 1)))
             (bb 1 [(= _1 #true)] (goto 4))
             (bb 2 [(= _1 #false)] (goto 4))
             (bb 3 [(= _11 (use _6))
                      (= _12 (use _5))
                      (= _10 (< (use _11) (use _12)))]
                 (switchInt _10 (0 3) (otherwise 1)))
             (bb 4 [] return))]))

(check-not-false (redex-match mir prog logical-or))

;; Tuples 
;; ======================================================
;fn main() -> () {
;    let mut _0: ();                      // return pointer
;    scope 1 {
;        let _1: (i32, i32);              // "tup" in scope 1 at <anon>:2:9: 2:12
;        scope 2 {
;            let _2: i32;                 // "first" in scope 2 at <anon>:3:9:3:14
;        }
;    }
;    let mut _3: i32;
;
;    bb0: {
;        StorageLive(_1);                 // scope 0 at <anon>:2:9: 2:12
;        _1 = (const 1i32, const 2i32);   // scope 0 at <anon>:2:15: 2:21
;        StorageLive(_2);                 // scope 1 at <anon>:3:9: 3:14
;        StorageLive(_3);                 // scope 1 at <anon>:3:17: 3:22
;        _3 = (_1.0: i32);                // scope 1 at <anon>:3:17: 3:22
;        _2 = _3;                         // scope 1 at <anon>:3:17: 3:22
;        StorageDead(_3);                 // scope 1 at <anon>:3:22: 3:22
;        _0 = ();                         // scope 2 at <anon>:1:11: 4:2
;        StorageDead(_2);                 // scope 1 at <anon>:4:2: 4:2
;        StorageDead(_1);                 // scope 0 at <anon>:4:2: 4:2
;        return;                          // scope 0 at <anon>:4:2: 4:2
;    }
;}

(define tuple
  (term (-> main () unit-ty
            (mut _0 : unit-ty)
            (scope 1
                   (_1 : (int int))
                   (scope 2 (_2 : int)))
            (mut _3 : int)
            (bb 0
                [(= _1 (1 2))
                 (= _3 (use (· _1  0)))
                 (= _2 (use _3))
                 (= _0 unit)]
                return))))

(check-not-false (redex-match mir fn tuple))

;; Downcasting to integer
;; ======================================================
;fn main() -> () {
;    let mut _0: ();                      // return pointer
;    scope 1 {
;        let _1: f64;                     // "x_float" in scope 1 at <anon>:2:9:2:16
;        scope 2 {
;            let _2: i32;                 // "x_int" in scope 2 at <anon>:3:9:3:14
;        }
;    }
;    let mut _3: f64;
;
;    bb0: {
;        StorageLive(_1);                 // scope 0 at <anon>:2:9: 2:16
;        _1 = const F64(1);               // scope 0 at <anon>:2:19: 2:22
;        StorageLive(_2);                 // scope 1 at <anon>:3:9: 3:14
;        StorageLive(_3);                 // scope 1 at <anon>:3:17: 3:24
;        _3 = _1;                         // scope 1 at <anon>:3:17: 3:24
;        _2 = _3 as i32 (Misc);           // scope 1 at <anon>:3:17: 3:31
;        StorageDead(_3);                 // scope 1 at <anon>:3:31: 3:31
;        _0 = ();                         // scope 2 at <anon>:1:11: 4:2
;        StorageDead(_2);                 // scope 1 at <anon>:4:2: 4:2
;        StorageDead(_1);                 // scope 0 at <anon>:4:2: 4:2
;        return;                          // scope 0 at <anon>:4:2: 4:2
;    }
;}

(define downcast
  (term (-> main () unit-ty
            (mut _0 : unit-ty)
            (scope 1
                   (_1 : float)
                   (scope 2 (_2 : int)))
            (mut _3 : float)
            (bb 0
                [(= _1 1)
                 (= _3 (use _1))
                 (= _2 (cast misc _3 as int))
                 (= _3 unit)]
                return))))

(check-not-false (redex-match mir fn downcast))

;; BinOp (Unchecked Addition)  
;; ======================================================
;fn main() -> () {
;    let mut _0: ();                      // return pointer
;    scope 1 {
;        let _1: i32;                     // "a" in scope 1 at <anon>:2:9: 2:10
;    }
;    let mut _2: (i32, bool);
;
;    bb0: {
;        StorageLive(_1);                 // scope 0 at <anon>:2:9: 2:10
;        _2 = CheckedAdd(const 1i32, const 2i32); // scope 0 at <anon>:2:13: 2:18
;        assert(!(_2.1: bool), "attempt to add with overflow") -> bb1; // scope 0 at <anon>:2:13: 2:18
;    }
;
;    bb1: {
;        _1 = (_2.0: i32);                // scope 0 at <anon>:2:13: 2:18
;        _0 = ();                         // scope 1 at <anon>:1:11: 3:2
;        StorageDead(_1);                 // scope 0 at <anon>:3:2: 3:2
;        return;                          // scope 0 at <anon>:3:2: 3:2
;    }
;}

(define add
  (term (-> main () unit-ty
            (mut _0 : unit-ty)
            (scope 1 (_1 : int))
            (_2 : (int bool))
            (bb 0
                [(= _2 (+ 1 2))]
                (assert (! (use (· _2 1)))
                        1
                        "attempt to add with overflow")))))

(check-not-false (redex-match mir fn add))


;; Vector initialization 
;; ======================================================
;fn main() -> () {
;    let mut _0: ();                      // return pointer
;    scope 1 {
;        let mut _1: std::vec::Vec<i32>;  // "vec" in scope 1 at <anon>:2:9: 2:16
;    }
;    let mut _2: ();
;    let mut _3: ();
;    let mut _4: &mut std::vec::Vec<i32>;
;
;    bb0: {
;        StorageLive(_1);                 // scope 0 at <anon>:2:9: 2:16
;        _1 = <std::vec::Vec<T>><i32>::new() -> bb1; // scope 0 at <anon>:2:30:2:40
;    }
;
;    bb1: {
;        StorageLive(_4);                 // scope 1 at <anon>:3:5: 3:8
;        _4 = &mut _1;                    // scope 1 at <anon>:3:5: 3:8
;        _3 = <std::vec::Vec<T>><i32>::push(_4, const 1i32) -> [return: bb4, unwind: bb3]; // scope 1 at <anon>:3:5: 3:16
;    }
;
;    bb2: {
;        resume;                          // scope 0 at <anon>:1:1: 4:2
;    }
;
;    bb3: {
;        drop(_1) -> bb2;                 // scope 0 at <anon>:4:2: 4:2
;    }
;
;    bb4: {
;        StorageDead(_4);                 // scope 1 at <anon>:3:16: 3:16
;        _0 = ();                         // scope 1 at <anon>:1:11: 4:2
;        drop(_1) -> bb5;                 // scope 0 at <anon>:4:2: 4:2
;    }
;
;    bb5: {
;        StorageDead(_1);                 // scope 0 at <anon>:4:2: 4:2
;        return;                          // scope 0 at <anon>:4:2: 4:2
;    }
;}

(define vec
  (term (-> main () unit-ty
            (mut _0 : unit-ty)
            (scope 1 (mut _1 : (vec int 0)))
            (mut _2 : unit-ty)
            (mut _3 : unit-ty)
            (mut _4 : (& mut (vec int 0)))
                                ;; FIXME: how to make this call during reduction only
            (bb 0 [] (call _1 _vec-new () 1))            ;; _1 = (vec α 0 0)
            (bb 1 [(= _4 (& mut _1))] (call _3
                                              _vec-push
                                              [(use _4)
                                               1]           ;; _3 = &mut (vec α 1 4)
                                              4))
            (bb 2 [] resume)
            (bb 3 [] (drop _1 2))
            (bb 4 [(= _0 unit)] (drop _1 5))
            (bb 5 [] return))))

(check-not-false (redex-match mir fn vec))

;; Match statements
;; ======================================================
;fn main() -> () {
;    let mut _0: ();                      // return pointer
;    scope 1 {
;        let mut _1: i32;                 // "result" in scope 1 at <anon>:2:9: 2:19
;        scope 2 {
;            let _2: i32;                 // "transform" in scope 2 at <anon>:3:9: 3:18
;            scope 3 {
;                let _3: i32;             // "result" in scope 3 at <anon>:5:9: 5:15
;            }
;        }
;    }
;    let mut _4: i32;
;    let mut _5: (i32, bool);
;
;    bb0: {
;        StorageLive(_1);                 // scope 0 at <anon>:2:9: 2:19
;        _1 = const 17i32;                // scope 0 at <anon>:2:22: 2:24
;        StorageLive(_2);                 // scope 1 at <anon>:3:9: 3:18
;        _2 = const 2i32;                 // scope 1 at <anon>:3:21: 3:22
;        StorageLive(_3);                 // scope 2 at <anon>:5:9: 5:15
;        switchInt(_2) -> [1i32: bb1, 2i32: bb2, otherwise: bb3]; // scope 2 at <anon>:6:9: 6:10
;    }
;
;    bb1: {
;        _3 = const 111i32;               // scope 2 at <anon>:6:14: 6:17
;        goto -> bb4;                     // scope 2 at <anon>:5:18: 9:6
;    }
;
;    bb2: {
;        _3 = const 222i32;               // scope 2 at <anon>:7:14: 7:17
;        goto -> bb4;                     // scope 2 at <anon>:5:18: 9:6
;    }
;
;    bb3: {
;        StorageLive(_4);                 // scope 2 at <anon>:8:14: 8:20
;        _4 = _1;                         // scope 2 at <anon>:8:14: 8:20
;        _5 = CheckedShr(_4, const 2i32); // scope 2 at <anon>:8:14: 8:25
;        assert(!(_5.1: bool), "attempt to shift right with overflow") -> bb5; // scope 2 at <anon>:8:14: 8:25
;    }
;
;    bb4: {
;        _0 = ();                         // scope 3 at <anon>:1:11: 10:2
;        StorageDead(_3);                 // scope 2 at <anon>:10:2: 10:2
;        StorageDead(_2);                 // scope 1 at <anon>:10:2: 10:2
;        StorageDead(_1);                 // scope 0 at <anon>:10:2: 10:2
;        return;                          // scope 0 at <anon>:10:2: 10:2
;    }
;
;    bb5: {
;        _3 = (_5.0: i32);                // scope 2 at <anon>:8:14: 8:25
;        StorageDead(_4);                 // scope 2 at <anon>:8:25: 8:25
;        goto -> bb4;                     // scope 2 at <anon>:5:18: 9:6
;    }
;}

(define match
  (term (-> main () unit-ty
            (mut _0 : unit-ty)
            (scope 1 (_1 : int)
                   (scope 2 (_2 : int)
                          (scope 3 (_3 : int))))
            (mut _4 : int)
            (mut _5 : (int bool))
            (bb 0
                [(= _1 17)
                 (= _2 2)]
                (switchInt _2 (1 1) (2 2) (otherwise 3)))
            (bb 1 [(= _3 111)] (goto 4))
            (bb 2 [(= _3 222)] (goto 4))
            (bb 3 [(= _4 (use _1)) (= _5 (>> (use _4) 2))]
                (assert (! (use (· _5 1))) 5 "attempt to shift right with overflow"))
            (bb 4 [(= _0 unit)] return)
            (bb 5 [(= _3 (use (· _5 0)))] (goto 4)))))

(check-not-false (redex-match mir fn match))

;; Vector fun
;; ======================================================
;fn main() -> () {
;    let mut _0: ();                      // return pointer
;    scope 1 {
;        let mut _1: std::vec::Vec<i32>;  // "vec" in scope 1 at <anon>:2:9: 2:16
;        scope 2 {
;            let _5: i32;                 // "first" in scope 2 at <anon>:4:9: 4:14
;        }
;    }
;    let mut _2: ();
;    let mut _3: ();
;    let mut _4: &mut std::vec::Vec<i32>;
;    let mut _6: i32;
;    let mut _7: &i32;
;    let mut _8: &std::vec::Vec<i32>;
;
;    bb0: {
;        StorageLive(_1);                 // scope 0 at <anon>:2:9: 2:16
;        _1 = const <std::vec::Vec<T>>::new() -> bb1; // scope 0 at <anon>:2:30: 2:40
;    }
;
;    bb1: {
;        StorageLive(_4);                 // scope 1 at <anon>:3:5: 3:8
;        _4 = &mut _1;                    // scope 1 at <anon>:3:5: 3:8
;        _3 = const <std::vec::Vec<T>>::push(_4, const 1i32) -> [return: bb4, unwind: bb3]; // scope 1 at <anon>:3:5: 3:16
;    }
;
;    bb2: {
;        resume;                          // scope 0 at <anon>:1:1: 5:2
;    }
;
;    bb3: {
;        drop(_1) -> bb2;                 // scope 0 at <anon>:5:2: 5:2
;    }
;
;    bb4: {
;        StorageDead(_4);                 // scope 1 at <anon>:3:16: 3:16
;        StorageLive(_5);                 // scope 1 at <anon>:4:9: 4:14
;        StorageLive(_6);                 // scope 1 at <anon>:4:17: 4:23
;        StorageLive(_7);                 // scope 1 at <anon>:4:17: 4:23
;        StorageLive(_8);                 // scope 1 at <anon>:4:17: 4:20
;        _8 = &_1;                        // scope 1 at <anon>:4:17: 4:20
;        _7 = const std::ops::Index::index(_8, const 0usize) -> [return: bb5, unwind: bb3]; // scope 1 at <anon>:4:17: 4:23
;    }
;
;    bb5: {
;        _6 = (*_7);                      // scope 1 at <anon>:4:17: 4:23
;        _5 = _6;                         // scope 1 at <anon>:4:17: 4:23
;        StorageDead(_6);                 // scope 1 at <anon>:4:23: 4:23
;        StorageDead(_8);                 // scope 1 at <anon>:4:23: 4:23
;        StorageDead(_7);                 // scope 1 at <anon>:4:24: 4:24
;        _0 = ();                         // scope 2 at <anon>:1:11: 5:2
;        StorageDead(_5);                 // scope 1 at <anon>:5:2: 5:2
;        drop(_1) -> bb6;                 // scope 0 at <anon>:5:2: 5:2
;    }
;
;    bb6: {
;        StorageDead(_1);                 // scope 0 at <anon>:5:2: 5:2
;        return;                          // scope 0 at <anon>:5:2: 5:2
;    }
;}

(define vector-fun
  (term (-> main () unit-ty
            (mut _0 : unit-ty)
            (scope 1 (mut _1 : (vec int 0))
                   (scope 2 (_5 : int)))
            (mut _2 : unit-ty)
            (mut _3 : unit-ty)
            (mut _4 : (& mut (vec int 0)))
            (mut _6 : int)
            (mut _7 : (& imm int))
            (mut _8 : (& imm (vec int 0)))
            (bb 0 [] (call _1 _vec-new () 1)) 
            (bb 1 [(= _4 (& mut _1))] (call _3 _vec-push ((use _4) 1) 4 3))
            (bb 2 [] resume)
            (bb 3 [] (drop _1 2))
                                                    ;; TODO impl Index  
            (bb 4 [(= _8 (& unique _1))] (call _7 std::ops::Index::index ((use _8) 0) 5 3))))) ;; FIXME indexing

(check-not-false (redex-match mir fn vector-fun))

;; Box creation, raw pointer manipulation
;; ======================================================
;fn main() -> () {
;    let mut _0: ();                      // return pointer
;    scope 1 {
;        let _1: std::boxed::Box<i32>;    // "x" in scope 1 at <anon>:2:9: 2:10
;        scope 2 {
;            let _3: *mut i32;            // "ptr" in scope 2 at <anon>:3:9: 3:12
;            scope 3 {
;                let _5: std::boxed::Box<i32>; // "x" in scope 3 at <anon>:4:9: 4:10
;            }
;        }
;    }
;    let mut _2: ();
;    let mut _4: std::boxed::Box<i32>;
;    let mut _6: *mut i32;
;
;    bb0: {
;        StorageLive(_1);                 // scope 0 at <anon>:2:9: 2:10
;        _1 = const <std::boxed::Box<T>>::new(const 5i32) -> bb1; // scope 0 at <anon>:2:13: 2:24
;    }
;
;    bb1: {
;        StorageLive(_3);                 // scope 1 at <anon>:3:9: 3:12
;        StorageLive(_4);                 // scope 1 at <anon>:3:29: 3:30
;        _4 = _1;                         // scope 1 at <anon>:3:29: 3:30
;        _3 = const <std::boxed::Box<T>>::into_raw(_4) -> [return: bb5, unwind: bb4]; // scope 1 at <anon>:3:15: 3:31
;    }
;
;    bb2: {
;        resume;                          // scope 0 at <anon>:1:1: 6:2
;    }
;
;    bb3: {
;        drop(_1) -> bb2;                 // scope 0 at <anon>:6:2: 6:2
;    }
;
;    bb4: {
;        drop(_4) -> bb3;                 // scope 1 at <anon>:3:31: 3:31
;    }
;
;    bb5: {
;        drop(_4) -> [return: bb6, unwind: bb3]; // scope 1 at <anon>:3:31: 3:31
;    }
;
;    bb6: {
;        StorageDead(_4);                 // scope 1 at <anon>:3:31: 3:31
;        StorageLive(_5);                 // scope 2 at <anon>:4:9: 4:10
;        StorageLive(_6);                 // scope 2 at <anon>:4:36: 4:39
;        _6 = _3;                         // scope 2 at <anon>:4:36: 4:39
;        _5 = const <std::boxed::Box<T>>::from_raw(_6) -> [return: bb7, unwind: bb3]; // scope 2 at <anon>:4:22: 4:40
;    }
;
;    bb7: {
;        StorageDead(_6);                 // scope 2 at <anon>:4:40: 4:40
;        _0 = ();                         // scope 3 at <anon>:1:11: 6:2
;        drop(_5) -> [return: bb8, unwind: bb3]; // scope 2 at <anon>:6:2: 6:2
;    }
;
;    bb8: {
;        StorageDead(_5);                 // scope 2 at <anon>:6:2: 6:2
;        StorageDead(_3);                 // scope 1 at <anon>:6:2: 6:2
;        drop(_1) -> bb9;                 // scope 0 at <anon>:6:2: 6:2
;    }
;
;    bb9: {
;        StorageDead(_1);                 // scope 0 at <anon>:6:2: 6:2
;        return;                          // scope 0 at <anon>:6:2: 6:2
;    }
;}

(define box-fun
  (term (-> main () unit-ty
            (mut _0 : unit-ty)
            (scope 1 (mut _1 : (box int))
                   (scope 2 (_3 : (* mut int))
                          (scope 3 (_5 : (box int)))))
            (mut _3 : unit-ty)
            (mut _4 : (box int))
            (mut _6 : (* mut int))
            (bb 0 [(= _1 (box 5))] (goto 1))
                                               ;; TODO Impl Box 
            (bb 1 [(= _4 (use _1))] (call _3 <std::boxed::Box<T>>::into_raw ((use _4)) 5 4))
            (bb 2 [] resume)
            (bb 3 [] (drop _1 2))
            (bb 4 [] (drop _4 3))
            (bb 5 [] (drop _4 6 3))
            (bb 6 [(= _6 (use _3))] (call _5 <std::boxed::Box<T>>::from_raw ((use _6)) 7 3))
            (bb 7 [(= _0 unit)] (drop _5 8 3))
            (bb 8 [] (drop _1 9))
            (bb 9 [] return))))

(check-not-false (redex-match mir fn box-fun))

;fn main() -> () {
;    let mut _0: ();                      // return pointer
;    let mut _1: bool;
;    let mut _2: &bool;
;    let mut _3: &bool;
;    let mut _4: bool;
;    let mut _5: &i64;
;    let mut _6: &i64;
;    let mut _7: i64;
;    bb0: {
;        StorageLive(_2);                 // scope 0 at <anon>:26:25: 26:30
;        StorageLive(_3);                 // scope 0 at <anon>:26:25: 26:30
;        _3 = promoted1;                  // scope 0 at <anon>:26:25: 26:30
;        _2 = &(*_3);                     // scope 0 at <anon>:26:25: 26:30
;        StorageLive(_5);                 // scope 0 at <anon>:26:32: 26:39
;        StorageLive(_6);                 // scope 0 at <anon>:26:32: 26:39
;        _6 = promoted0;                  // scope 0 at <anon>:26:32: 26:39
;        _5 = &(*_6);                     // scope 0 at <anon>:26:32: 26:39
;        _1 = const are_positive_hashes(_2, _5) -> bb1; // scope 0 at <anon>:26:5: 26:40
;    }
;    bb1: {
;        StorageDead(_5);                 // scope 0 at <anon>:26:40: 26:40
;        StorageDead(_2);                 // scope 0 at <anon>:26:40: 26:40
;        StorageDead(_6);                 // scope 0 at <anon>:26:41: 26:41
;        StorageDead(_3);                 // scope 0 at <anon>:26:41: 26:41
;        _0 = ();                         // scope 0 at <anon>:25:15: 27:2
;        return;                          // scope 0 at <anon>:27:2: 27:2
;    }
;}
;promoted0 in main: &i64 = {
;    let mut _0: &i64;                    // return pointer
;    let mut _1: i64;
;    bb0: {
;        _1 = const 12i64;                // scope 0 at <anon>:26:33: 26:39
;        _0 = &_1;                        // scope 0 at <anon>:26:32: 26:39
;        return;                          // scope 0 at <anon>:26:32: 26:39
;    }
;}
;promoted1 in main: &bool = {
;    let mut _0: &bool;                   // return pointer
;    let mut _1: bool;
;    bb0: {
;        _1 = const true;                 // scope 0 at <anon>:26:26: 26:30
;        _0 = &_1;                        // scope 0 at <anon>:26:25: 26:30
;        return;                          // scope 0 at <anon>:26:25: 26:30
;    }
;}
;fn <i64 as Hash>::hash(_1: &i64) -> u64 {
;    let mut _0: u64;                     // return pointer
;    scope 1 {
;        let _2: &i64;                    // "self" in scope 1 at <anon>:12:13: 12:18
;    }
;    let mut _3: i64;
;    bb0: {
;        StorageLive(_2);                 // scope 0 at <anon>:12:13: 12:18
;        _2 = _1;                         // scope 0 at <anon>:12:13: 12:18
;        StorageLive(_3);                 // scope 1 at <anon>:13:9: 13:14
;        _3 = (*_2);                      // scope 1 at <anon>:13:9: 13:14
;        _0 = _3 as u64 (Misc);           // scope 1 at <anon>:13:9: 13:21
;        StorageDead(_3);                 // scope 1 at <anon>:13:21: 13:21
;        StorageDead(_2);                 // scope 0 at <anon>:14:6: 14:6
;        return;                          // scope 1 at <anon>:14:6: 14:6
;    }
;}
;fn are_positive_hashes(_1: &T, _2: &U) -> bool {
;    let mut _0: bool;                    // return pointer
;    scope 1 {
;        let _3: &T;                      // "t" in scope 1 at <anon>:21:42: 21:43
;        let _4: &U;                      // "u" in scope 1 at <anon>:21:49: 21:50
;    }
;    let mut _5: bool;
;    let mut _6: &T;
;    let mut _7: bool;
;    let mut _8: &U;
;    bb0: {
;        StorageLive(_3);                 // scope 0 at <anon>:21:42: 21:43
;        _3 = _1;                         // scope 0 at <anon>:21:42: 21:43
;        StorageLive(_4);                 // scope 0 at <anon>:21:49: 21:50
;        _4 = _2;                         // scope 0 at <anon>:21:49: 21:50
;        StorageLive(_5);                 // scope 1 at <anon>:22:5: 22:24
;        StorageLive(_6);                 // scope 1 at <anon>:22:22: 22:23
;        _6 = &(*_3);                     // scope 1 at <anon>:22:22: 22:23
;        _5 = const is_positive_hash(_6) -> bb5; // scope 1 at <anon>:22:5: 22:24
;    }
;    bb1: {
;        _0 = const true;                 // scope 1 at <anon>:22:5: 22:47
;        goto -> bb4;                     // scope 1 at <anon>:22:5: 22:47
;    }
;    bb2: {
;        _0 = const false;                // scope 1 at <anon>:22:5: 22:47
;        goto -> bb4;                     // scope 1 at <anon>:22:5: 22:47
;    }
;    bb3: {
;        StorageLive(_7);                 // scope 1 at <anon>:22:28: 22:47
;        StorageLive(_8);                 // scope 1 at <anon>:22:45: 22:46
;        _8 = &(*_4);                     // scope 1 at <anon>:22:45: 22:46
;        _7 = const is_positive_hash(_8) -> bb6; // scope 1 at <anon>:22:28: 22:47
;    }
;    bb4: {
;        StorageDead(_7);                 // scope 1 at <anon>:22:47: 22:47
;        StorageDead(_5);                 // scope 1 at <anon>:22:47: 22:47
;        StorageDead(_4);                 // scope 0 at <anon>:23:2: 23:2
;        StorageDead(_3);                 // scope 0 at <anon>:23:2: 23:2
;        return;                          // scope 1 at <anon>:23:2: 23:2
;    }
;    bb5: {
;        StorageDead(_6);                 // scope 1 at <anon>:22:24: 22:24
;        switchInt(_5) -> [0u8: bb2, otherwise: bb3]; // scope 1 at <anon>:22:5: 22:47
;    }
;    bb6: {
;        StorageDead(_8);                 // scope 1 at <anon>:22:47: 22:47
;        switchInt(_7) -> [0u8: bb2, otherwise: bb1]; // scope 1 at <anon>:22:5: 22:47
;    }
;}
;fn is_positive_hash(_1: &T) -> bool {
;    let mut _0: bool;                    // return pointer
;    scope 1 {
;        let _2: &T;                      // "t" in scope 1 at <anon>:17:30: 17:31
;    }
;    let mut _3: u64;
;    let mut _4: &T;
;    bb0: {
;        StorageLive(_2);                 // scope 0 at <anon>:17:30: 17:31
;        _2 = _1;                         // scope 0 at <anon>:17:30: 17:31
;        StorageLive(_3);                 // scope 1 at <anon>:18:5: 18:13
;        StorageLive(_4);                 // scope 1 at <anon>:18:5: 18:6
;        _4 = &(*_2);                     // scope 1 at <anon>:18:5: 18:6
;        _3 = const Hash::hash(_4) -> bb1; // scope 1 at <anon>:18:5: 18:13
;    }
;    bb1: {
;        StorageDead(_4);                 // scope 1 at <anon>:18:13: 18:13
;        _0 = Gt(_3, const 0u64);         // scope 1 at <anon>:18:5: 18:17
;        StorageDead(_3);                 // scope 1 at <anon>:18:17: 18:17
;        StorageDead(_2);                 // scope 0 at <anon>:19:2: 19:2
;        return;                          // scope 1 at <anon>:19:2: 19:2
;    }
;}
;fn <bool as Hash>::hash(_1: &bool) -> u64 {
;    let mut _0: u64;                     // return pointer
;    scope 1 {
;        let _2: &bool;                   // "self" in scope 1 at <anon>:6:13: 6:18
;    }
;    let mut _3: bool;
;    bb0: {
;        StorageLive(_2);                 // scope 0 at <anon>:6:13: 6:18
;        _2 = _1;                         // scope 0 at <anon>:6:13: 6:18
;        StorageLive(_3);                 // scope 1 at <anon>:7:12: 7:17
;        _3 = (*_2);                      // scope 1 at <anon>:7:12: 7:17
;        switchInt(_3) -> [0u8: bb2, otherwise: bb1]; // scope 1 at <anon>:7:9: 7:34
;    }
;    bb1: {
;        _0 = const 0u64;                 // scope 1 at <anon>:7:20: 7:21
;        goto -> bb3;                     // scope 1 at <anon>:7:9: 7:34
;    }
;    bb2: {
;        _0 = const 1u64;                 // scope 1 at <anon>:7:31: 7:32
;        goto -> bb3;                     // scope 1 at <anon>:7:9: 7:34
;    }
;    bb3: {
;        StorageDead(_3);                 // scope 1 at <anon>:7:34: 7:34
;        StorageDead(_2);                 // scope 0 at <anon>:8:6: 8:6
;        return;                          // scope 1 at <anon>:8:6: 8:6
;    }
;}

(define main
  (term (-> main () unit-ty
            (mut _0 : unit-ty)
            (mut _1 : bool)
            (mut _2 : (& imm bool))
            (mut _3 : (& imm bool))
            (mut _4 : bool)
            (mut _5 : (& imm int))
            (mut _6 : (& imm int))
            (mut _7 : int)
            (promoted 0 () (& imm int)
                (mut _0 : (& imm int))
                (mut _1 : int)
                (bb 0 [(= _1 12) (= _0 (& unique _1))] return))
            (promoted 1 () (& imm bool)
                (mut _0 : (& imm bool))
                (mut _1 : bool)
                (bb 0 [(= _1 #true) (= _0 (& unique _1))] return))
            (bb 0 [(= _3 promoted1) ;; FIXME 
                     (= _2 (& unique (* _3)))
                     (= _6 promoted0)
                     (= _5 (& unique (* _6)))]
                (call _1 are_positive_hashes ((use _2) (use _5)) 1))
            (bb 1 [(= _0 unit)] return))))

(define impl_hash_for_i64
  (term (-> impl_hash_for_i64 ((_1 : (& imm int))) uint
            (mut _0 : uint)
            (scope 1 (_2 : (& imm int)))
            (mut _3 :  int)
            (bb 0 [(= _2 (use _1))
                     (= _3 (use (* _2)))
                     (= _0 (cast misc _3 as uint))]
                return))))

(define impl_hash_for_bool
  (term (-> impl_hash_for_bool ((_1 : (& imm bool))) uint
            (mut _0 : uint)
            (scope 1 (_2 : (& imm bool)))
            (_3 : bool)
            (bb 0 [(= _2 (use _1))
                     (= _3 (use (* _2)))]
                     (switchInt _3 (0 2) (otherwise 1)))
            (bb 1 [(= _0 0)] (goto 3))
            (bb 2 [(= _0 1)] (goto 3))
            (bb 3 [] return))))

(define is_positive_hash
  (term (-> is_positive_hash ((_1 : (& imm T))) bool
            (mut _0 : bool)
            (scope 1 (_2 : (& imm T)))
            (mut _3 : uint)
            (mut _4 : (& imm T))
            (bb 0 [(= _2 (use _1))
                     (= _4 (& unique (* _2)))]
                (call _3 Hash::hash ((use _4)) 1))
            (bb 1 [(= _0 (> (use _3) 0))] return))))

(define are_positive_hashes
  (term (-> are_positive_hashes ((_1 : (& imm T)) (_2 : (& imm U))) bool
            (mut _0 : bool)
            (scope 1 (_3 : (& imm T)) (_4 : (& imm U)))
            (mut _5 : bool)
            (mut _6 : (& imm T))
            (mut _7 : bool)
            (mut _8 : (& imm U))
            (bb 0 [(= _3 (use _1))
                     (= _4 (use _2))
                     (= _6 (& unique (* _3)))]
                (call _5 is_positive_hash ((use _6)) 5))
            (bb 1 [(= _0 #true)] (goto 4))
            (bb 2 [(= _0 #false)] (goto 4))
            (bb 3 [(= _8 (& unique (* _4)))] (call _7 is_positive_hash ((use _8)) 6))
            (bb 4 [] return)
            (bb 5 [] (switchInt _5 (0 2) (otherwise 3)))
            (bb 6 [] (switchInt _7 (0 2) (otherwise 1))))))

(check-not-false (redex-match mir fn main))
(check-not-false (redex-match mir fn impl_hash_for_i64))
(check-not-false (redex-match mir fn impl_hash_for_bool))
(check-not-false (redex-match mir fn is_positive_hash))
(check-not-false (redex-match mir fn are_positive_hashes))
(check-not-false (redex-match mir prog
                              (term (,main ,impl_hash_for_i64 ,impl_hash_for_bool ,is_positive_hash ,are_positive_hashes))))