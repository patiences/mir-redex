#lang racket
(require redex)
(provide (all-defined-out))

;; A Redex model for the intermediate representation (MIR) inside the
;; Rust compiler.

;; This is an implementation that discards most type information. 
;; Since we don't need type information to evaluate the program
;; in most cases, let's see what happens when we don't have it.
;; (An exception to this is type casting which can show up in an rvalue,
;; in that case maybe we can just use typed nums everywhere, i.e. 5i32
;; instead of 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-language mir
  ;; program
  ;;     fns: list of functions
  ;;     call: i.e. main, the entrance function call into the program
  (prog fns call)

  ;; functions
  (fns [fn ...])
  ;; function
  ;;     g: function name
  ;;     x: parameters
  ;;     vars: local variables
  ;;     bbs: basic blocks 
  ;;     idx: the basic block to run first 
  (fn (g (x_!_ ...) bbs idx))

  ;; variables with their assignments 
  (vars (let-vars ([= lv_!_ rv] ...)))

  ;; basic blocks, with a unique integer identifier, local variables and a terminator
  (bbs (let-bbs([bb idx_!_ vars terminator] ...)))

  ;; terminator
  (terminator return                              ;; return to caller
              resume                              ;; emitted by diverge call during unwinding 
              (switchInt lv                       ;; switch on an integer, branching to bb l 
                         (const idx) ...
                         (otherwise idx))
              (assert rv idx msg)                 ;; checked branch
              ;;    rv: condition
              ;;    idx: index of block to branch to, if rv evals to true
              ;;    msg: error message, if rv evals to false
              (assert rv idx idx msg)             ;; as above, with extra unwinding label 
              ;; TODO: Consider removing: calls are just an assignment + a goto. 
              (call lv g rvs idx)                 ;; lv gets the result of calling g(rvs), branch to l on return  
              (call lv g rvs idx idx)             ;; as above, with extra unwinding label  
              (goto idx)                          ;; goto l
              (drop lv idx)                       ;; drop lv, goto l 
              (drop lv idx idx))                  ;; as above, with extra unwinding label

  ;; lvalues
  (lv x
      (· lv f)                                    ;; projection (e.g. tuple access)
      (* lv))                                     ;; pointer deref

  ;; rvalue
  (rvs (rv ...))
  (rv (use lv)                                    ;; use lv  
      (& borrowkind lv)                           ;; references (borrows)
      const                                       ;; constants
      (binop operand operand)                     ;; binary operations 
      (unop operand)                              ;; unary operations
      (cast castkind lv as ty))                   ;; typecasts
      ;; TODO: handle remaining rvalues 

  ;; A subset of rvalues that can be used inside other rvalues 
  (operand (use lv)
           const)

  ;; A subset of rvalues that can be evaluated at compile time
  ;; http://manishearth.github.io/rust-internals-docs/rustc_typeck/middle/const_val/enum.ConstVal.html
  (const boolean
         (number numtype)
         unit)                                    ;; unit values

  ;; binary operation kinds
  (binop + - * / % ^ & \| << >> == < <= != >= >)
  
  ;; unary operation kinds
  (unop ! -)
  
  ;; cast kinds 
  (castkind misc
            reifyfp                               ;; ReifyFnPointer
            closurefp                             ;; ClosureFnPointer 
            unsafefp                              ;; UnsafeFnPointer
            unsize)                               ;; Unsize

  ;; types of numbers
  (numtype i32 i64 u8 u32 u64)
  
  ;; borrow kinds
  (borrowkind shared unique mut)

  (idx integer)                                   ;; indices 
  (x variable-not-otherwise-mentioned)            ;; variables 
  (f integer)                                     ;; field "names"
  (g variable-not-otherwise-mentioned))           ;; function names