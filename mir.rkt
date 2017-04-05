#lang racket
(require redex)
(provide (all-defined-out))

(define-language mir
  ;; program
  ;;    fns: list of functions 
  (prog fns)
  
  ;; functions 
  (fns (fn ...))
  ;; function
  ;;    g: function name
  ;;    vdecls: parameters
  ;;    type: return type
  ;;    decl ...: list of variable declarations and local scopes 
  ;;    blk ...: list of basic blocks
  (fn (-> g vdecls type decl ... blk ...))
  
  ;; declaration
  ;;    vdecl: single variable declaration
  ;;    scp: variable declarations inside a scope 
  (decl vdecl
        scp)
  
  ;; variable declarations 
  (vdecls (vdecl ...))
  ;; variable declaration, with mutability qualifier (immutable by default)
  (vdecl (x : type)
         (mq x : type))
  
  ;; mutability qualifier
  (mq mut
      imm)
  
  ;; scope
  ;;    l: label
  ;;    vdecls: list of (local) variable declarations
  ;;    scps: list of (nested) scopes 
  (scp (scope l decl ...))
  
  ;; basic block
  ;;    l: label
  ;;    sts: list of statements
  ;;    terminator: connection to subsequent basic blocks  
  (blk (bb l sts terminator))
  
  ;; statements 
  (sts (st ...))
  ;; statement
  (st  ;; single variable assignment 
   (= lv rv))
    
  ;; terminator 
  (terminator ;; return to caller
   return
   ;; emitted by diverge call during unwinding 
   resume
   ;; switch on an integer, branching to bb l 
   (switchInt lv (const l) ... (otherwise l))
   ;; checked branch
   ;;    rv: condition
   ;;    l: block to branch to, if rv evaluates to true
   ;;    msg: error message 
   (assert rv l msg)
   ;; assert with an unwinding label
   (assert rv l l msg)
   ;; lv gets the result of calling g with rvs as args
   ;; branch to bb l on return 
   (call lv g rvs l)
   ;; call with an unwinding label 
   (call lv g rvs l l)
   ;; go to bb l 
   (goto l)
   ;; drop the lv
   (drop lv l)
   ;; drop with an unwinding label 
   (drop lv l l))
  
  ;; lvalues
  (lvs (lv ...))
  ;; lvalue 
  (lv x
      ;; projection (e.g. tuple access)
      (· lv f))
  
  ;; TODO: Consider combining tuples & structs & vectors into single "aggregate" value? 
  ;;        See http://manishearth.github.io/rust-internals-docs///rustc/mir/enum.Rvalue.html
  
  ;; rvalues
  (rvs (rv ...))
  ;; rvalue 
  (rv ;; by-value use of lv 
   (use lv)
   ;; references (borrows) 
   (& borrowkind lv)
   ;; constants
   const
   ;; binary operations
   (binop rv rv)
   ;; unary operations
   (unop rv)
   ;; vectors
   (vec type len)
   ;; aggregate values
   (rv ...)
   ;; typecast 
   (cast castkind lv as type)
   ;; structs
   ;;    s: struct name
   ;;    sts: assignments to struct variables 
   (struct s sts))
  
  ;; constants (can be evaluated at compile time)
  (const boolean
         number
         ;; unit values 
         unit)
  
  ;; binary operation kinds
  (binop + - * / % ^ & \| << >> == < <= != >= >)
  
  ;; unary operation kinds
  (unop ! -)
  
  ;; cast kinds 
  (castkind misc)
  
  ;; borrow kinds
  (borrowkind mut shared unique)
    
  ;; type
  (type ;; unit, i.e. () 
   unit-type
   int
   uint
   float
   bool
   ;; struct types 
   (struct s)
   ;; aggregate types
   (type ...)
   ;; vector type
   (vec type len)
   ;; type with qualifier
   (& mq type)
   ;; box
   (box type))
  
  ;; error messages
  (msg string)
  
  ;; vector length
  (len integer)
  
  ;; variables 
  (x variable-not-otherwise-mentioned)
  ;; field names
  (f number)
  ;; label
  (l variable-not-otherwise-mentioned)
  ;; function names
  (g variable-not-otherwise-mentioned)
  ;; struct names
  (s variable-not-otherwise-mentioned))

;; Evaluation
;; ===========================================
(define-extended-language mir-e mir
  ;; Evaluation contexts 
  (C hole 
     ;; BinOps
     (+ const C) (+ C rv)
     (- const C) (- C rv)
     (* const C) (* C rv)
     (/ const C) (/ C rv)))

(define reduce
  (reduction-relation
   mir-e #:domain C 
   (--> (in-hole C (+ const_1 const_2))
        (in-hole C ,(+ (term const_1) (term const_2)))
        "+")
   
   (--> (in-hole C (- const_1 const_2))
        (in-hole C ,(- (term const_1) (term const_2)))
        "-")
   
   (--> (in-hole C (* const_1 const_2))
        (in-hole C ,(* (term const_1) (term const_2)))
        "*")
   
   (--> (in-hole C (/ const_1 const_2))
        (in-hole C ,(/ (term const_1) (term const_2)))
        "/")))
   