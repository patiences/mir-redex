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
         (mut x : type))
  
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
              ;; switch on an integer, branching to bb l 
              (switchInt lv (const l) ... (otherwise l))
              ;; lv gets the result of calling g with rvs as args
              ;; branch to bb l on return 
              (call lv g rvs l)
              ;; go to bb l 
              (goto l))

  ;; lvalues
  (lvs (lv ...))
  ;; lvalue 
  (lv x
      ;; projection (e.g. tuple access)
      (· lv f))
  
  ;; rvalues
  (rvs (rv ...))
  ;; rvalue 
  (rv ;; by-value use of lv 
      (use lv)  
      ;; constants
      const
      ;; aggregate values
      (rv ...)
      ;; structs
      ;;    s: struct name
      ;;    sts: assignments to struct variables 
      (struct s sts)
      ;; struct constants
      (struct s (const ...))
      (struct s (rv ...))) ;; FIXME how many kinds of structs do we have...?

  ;; constants (can be evaluated at compile time)
  (const boolean
         ;; annotated numbers
         (number type)
         ;; unit values 
         unit)

  ;; type
  (type ;; unit, i.e. () 
        unit-type
        i32
        i64
        u08
        bool
        ;; struct types 
        (struct s)
        ;; aggregate types
        (type ...))

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
              
  
  

                 
                 
                 
                 
                 
                 