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
  (term (main () (let-bbs ([bb 0 (let-vars
                                  ([= _1 (struct Point ([= x (4 i32)] [= y (5 i32)]))]
                                   [= _0 unit])) return]))
              0)))

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
  (term (main () (let-bbs ([bb 0 (let-vars ([= _1 (! #t)] [= _0 unit])) return])) 0)))

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
  (term ((main () (let-bbs ([bb 0 (let-vars ()) (call _1 foo ((0 i32) (10 i32) (5 i32)) 1)]
                            [bb 1 (let-vars ([= _0 unit])) return])) 0)
         (foo (_1 _2 _3) (let-bbs ([bb 0 (let-vars ([= _4 (use _1)]
                                                    [= _5 (use _2)]
                                                    [= _6 (use _3)]
                                                    [= _8 (use _4)]
                                                    [= _9 (use _6)]
                                                    [= _7 (< (use _8) (use _9))]))
                                       (switchInt (use _7) ((0 u8) 3) (otherwise 1))]
                                   [bb 1 (let-vars ([= _0 #t])) (goto 4)]
                                   [bb 2 (let-vars ([= _0 #f])) (goto 4)]
                                   [bb 3 (let-vars ([= _11 (use _6)]
                                                    [= _12 (use _5)]
                                                    [= _10 (< (use _11) (use _12))]))
                                       (switchInt (use _10) ((0 u8) 2) (otherwise 1))]
                                   [bb 4 (let-vars ()) return]))
              0))))

(check-not-false (redex-match mir fns logical-or))

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
  (term (main () (let-bbs ([bb 0 (let-vars ([= _1 ((1 i32) (2 i32))]
                                            [= _3 (use (· _1 0))]
                                            [= _2 (use _3)]
                                            [= _0 unit])) return]))
              0)))

(check-not-false (redex-match mir fn tuple))

;; BinOp (Checked Addition)  
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
  (term (main () (let-bbs ([bb 0 (let-vars ([= _2 (+ (1 i32) (2 i32))]))
                               (assert (use (· _2 1)) #f 1 "attempt to add with overflow")]
                           [bb 1 (let-vars ([= _1 (use (· _2 0))] [= _0 unit])) return]))
              0)))

(check-not-false (redex-match mir fn add))

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
  (term (main () (let-bbs ([bb 0 (let-vars ([= _1 (17 i32)]
                                            [= _2 (2 i32)]))
                               (switchInt (use _2) ((1 i32) 1) ((2 i32) 2) (otherwise 3))]
                           [bb 1 (let-vars ([= _3 (111 i32)])) (goto 4)]
                           [bb 2 (let-vars ([= _3 (222 i32)])) (goto 4)]
                           [bb 3 (let-vars ([= _4 (use _1)] [= _5 (>> (use _4) (2 i32))]))
                               (assert (use (· _5 1)) #f 5 "attempt to shift right with overflow")]
                           [bb 4 (let-vars ([= _0 unit])) return]
                           [bb 5 (let-vars ([= _3 (use (· _5 0))])) (goto 4)]))
              0)))

(check-not-false (redex-match mir fn match))