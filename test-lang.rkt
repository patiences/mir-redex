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
            (bb bb0
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
            (scope scope1 (mut _1 : int))
            (bb bb0
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
            (scope scope1 (_1 : (struct Point)))
            (bb bb0
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
            (scope scope1 (_1 : bool))
            (bb bb0
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
             (bb bb0 [] (call _1
                              foo [0 10 5]
                              bb1))
             (bb bb1 [(= _0 unit)] return))
         (-> foo [(_1 : int) (_2 : int) (_3 : int)] bool
             (mut _0 : unit-ty)
             (scope scope1
                    (_4 : int)
                    (_5 : int)
                    (_6 : int))
             (mut _7 : bool)
             (mut _8 : int)
             (mut _9 : bool)
             (mut _10 : int)
             (mut _11 : int)
             (mut _12 : int)
             (bb bb0
                 [(= _4 (use _1))
                  (= _5 (use _2))
                  (= _6 (use _3))
                  (= _8 (use _4))
                  (= _9 (use _6))
                  (= _7 (< (use _8) (use _9)))]
                 (switchInt _10 (0 bb3) (otherwise bb1)))
             (bb bb1 [(= _1 #true)] (goto bb4))
             (bb bb2 [(= _1 #false)] (goto bb4))
             (bb bb3 [(= _11 (use _6))
                      (= _12 (use _5))
                      (= _10 (< (use _11) (use _12)))]
                 (switchInt _10 (0 bb3) (otherwise bb1)))
             (bb bb4 [] return))]))

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
            (scope scope1
                   (_1 : (int int))
                   (scope scope2 (_2 : int)))
            (mut _3 : int)
            (bb bb0
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
            (scope scope1
                   (_1 : float)
                   (scope scope2 (_2 : int)))
            (mut _3 : float)
            (bb bb0
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
            (scope scope1 (_1 : int))
            (_2 : (int bool))
            (bb bb0
                [(= _2 (+ 1 2))]
                (assert (! (use (· _2 1)))
                        bb1
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
            (scope scope1 (mut _1 : (vec int 0)))
            (mut _2 : unit-ty)
            (mut _3 : unit-ty)
            (mut _4 : (& mut (vec int 0)))
            (bb bb0 [] (call _1 <std::vec::Vec<T>><int>::new () bb1))
            (bb bb1 [(= _4 (& mut _1))] (call _3
                                              <std::vec::Vec<T>><int>::push
                                              [(use _4)
                                               1]
                                              bb4))
            (bb bb2 [] resume)
            (bb bb3 [] (drop _1 bb2))
            (bb bb4 [(= _0 unit)] (drop _1 bb5))
            (bb bb5 [] return))))

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
            (scope scope1 (_1 : int)
                   (scope scope2 (_2 : int)
                          (scope scope3 (_3 : int))))
            (mut _4 : int)
            (mut _5 : (int bool))
            (bb bb0
                [(= _1 17)
                 (= _2 2)]
                (switchInt _2 (1 bb1) (2 bb2) (otherwise bb3)))
            (bb bb1 [(= _3 111)] (goto bb4))
            (bb bb2 [(= _3 222)] (goto bb4))
            (bb bb3 [(= _4 (use _1)) (= _5 (>> (use _4) 2))]
                (assert (! (use (· _5 1))) bb5 "attempt to shift right with overflow"))
            (bb bb4 [(= _0 unit)] return)
            (bb bb5 [(= _3 (use (· _5 0)))] (goto bb4)))))

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
            (scope scope1 (mut _1 : (vec int 0))
                   (scope scope2 (_5 : int)))
            (mut _2 : unit-ty)
            (mut _3 : unit-ty)
            (mut _4 : (& mut (vec int 0)))
            (mut _6 : int)
            (mut _7 : (& imm int))
            (mut _8 : (& imm (vec int 0)))
            (bb bb0 [] (call _1 <std::vec::Vec<T>>::new () bb1)) ;; FIXME const call?
            (bb bb1 [(= _4 (& mut _1))] (call _3 <std::vec::Vec<T>>::push ((use _4) 1) bb4 bb3))
            (bb bb2 [] resume)
            (bb bb3 [] (drop _1 bb2))
            (bb bb4 [(= _8 (& unique _1))] (call _7 std::ops::Index::index ((use _8) 0) bb5 bb3))))) ;; FIXME indexing

(check-not-false (redex-match mir fn vector-fun))