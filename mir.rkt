#lang racket
(require redex)
(provide mir)

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

  ;; TODO: add unwinding labels
  
  ;; terminator 
  (terminator ;; return to caller
              return
              ;; emitted by diverge call during unwinding 
              resume
              ;; switch on an integer, branching to bb l 
              (switchInt lv (const l) (otherwise l))
              ;; checked branch
              ;;    rv: condition
              ;;    l: block to branch to, if rv evaluates to true
              ;;    msg: error message 
              (assert rv l msg)
              ;; lv gets the result of calling g with rvs as args
              ;; branch to bb l on return 
              (call lv g rvs l)
              ;; go to bb l 
              (goto l)
              ;; drop the lv
              (drop lv l))

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
         ;; annotated numbers
         (number type)
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

  ;; TODO: can we drop num sizes?
  
  ;; type
  (type ;; unit, i.e. () 
        unit-type
        i32
        i64
        u08
        f64
        bool
        ;; struct types 
        (struct s)
        ;; aggregate types
        (type ...)
        ;; vector type
        (vec type len)
        ;; type with qualifier
        (& mq type))

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
                