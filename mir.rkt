#lang racket
(require redex)
(provide (all-defined-out))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 

(define-language mir
  ;; program
  ;;    fns: list of functions 
  (prog fns)
  
  ;; functions 
  (fns (fn ...))
  ;; function
  ;;    g: function name
  ;;    vdecls: parameters
  ;;    ty: return type
  ;;    decl ...: list of variable declarations and local scopes 
  ;;    blk ...: list of basic blocks
  (fn (-> g vdecls ty decl ... blk ...))
  
  ;; declaration
  ;;    vdecl: single variable declaration
  ;;    scp: variable declarations inside a scope 
  (decl vdecl
        scp)
  
  ;; variable declarations 
  (vdecls (vdecl ...))
  (vdecl (x : ty)                     ;; immutable (by default)
         (mq x : ty))                 ;; decl with mutability qualifier 
  
  ;; mutability qualifiers
  (mq mut imm)
  
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
  (st (= lv rv))                        ;; single variable assignment 
  
  ;; terminator 
  (terminator return                    ;; return to caller
              resume                    ;; emitted by diverge call during unwinding 
              (switchInt lv             ;; switch on an integer, branching to bb l 
                         (const l) ...
                         (otherwise l))
              (assert rv l msg)         ;; checked branch
              ;;    rv: condition
              ;;    l: block to branch to, if rv evals to true
              ;;    msg: error message, if rv evals to false
              (assert rv l l msg)       ;; as above, with extra unwinding label 
              (call lv g rvs l)         ;; lv gets the result of calling g(rvs), branch to l on return  
              (call lv g rvs l l)       ;; as above, with extra unwinding label  
              (goto l)                  ;; goto l
              (drop lv l)               ;; drop lv, goto l 
              (drop lv l l))            ;; as above, with extra unwinding label 
  
  ;; lvalues
  (lvs (lv ...))
  (lv x
      (· lv f)                         ;; projection (e.g. tuple access)
      (* lv))                          ;; pointer deref
  
  ;; rvalues
  (rvs (rv ...))
  (rv (use lv)                          ;; use lv  
      (& borrowkind lv)                 ;; references (borrows)
      const                             ;; constants
      (binop operand operand)           ;; binary operations 
      (unop operand)                    ;; unary operations
      (cast castkind lv as ty)          ;; typecasts
      ;; FIXME: capacity? http://manishearth.github.io/rust-internals-docs///src/collections/vec.rs.html#296-299
      (vec ty len)                      ;; vectors 
      (operand ...)                     ;; aggregate values (i.e. tuples)
      ;; FIXME: is this the right information to keep in the box? 
      ;;        https://doc.rust-lang.org/src/alloc/boxed.rs.html#108
      (box rv)                          ;; pointer to heap-allocated val
      (struct s sts))                   ;; structs, with name and body
  
  ;; A subset of rvalues that can be used inside other rvalues 
  (operand (use lv)
           const)
  
  ;; A subset of rvalues that can be evaluated at compile time 
  (const boolean
         number
         unit)                          ;; unit values
  
  ;; binary operation kinds
  (binop + - * / % ^ & \| << >> == < <= != >= >)
  
  ;; unary operation kinds
  (unop ! -)
  
  ;; cast kinds 
  (castkind misc
            reifyfp                     ;; ReifyFnPointer
            closurefp                   ;; ClosureFnPointer 
            unsafefp                    ;; UnsafeFnPointer
            unsize)                     ;; Unsize 
  
  ;; borrow kinds
  (borrowkind shared unique mut)
  
  ;; types
  (ty unit-ty                           ;; i.e. ()
      int uint float                    ;; numbers 
      bool                              ;; booleans 
      (struct s)                        ;; structs 
      (ty ...)                          ;; aggregate types (i.e. tuples) 
      (vec ty len)                      ;; vectors 
      (& mq ty)                         ;; reference types with qualifiers
      (* mq ty)                         ;; raw pointer types with qualifiers
      (box ty)                          ;; box 
      ;; FIXME: multiple generic types https://doc.rust-lang.org/book/generics.html
      T)                                ;; generic            
  
  ;; error messages
  (msg string)
  
  ;; vector length 
  (len integer)
  
  (x variable-not-otherwise-mentioned)  ;; variables 
  (f integer)                           ;; field names
  (g variable-not-otherwise-mentioned)  ;; function names
  (l variable-not-otherwise-mentioned)  ;; labels (i.e. block names)
  (s variable-not-otherwise-mentioned)) ;; struct names

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-extended-language mir-machine mir
  ;; a state of the machine
  ;;    rv: a single rvalue ;;TODO: eventually work up to prog... 
  ;;    σ: the store (heap)
  ;;    ρ: the global variable table 
  (P (rv σ ρ))
  ;; the heap, addresses mapped to their heap value
  (σ (store (α v) ...))
  (v (ptr α)                           ;; pointer
     number                            ;; numerical value
     void)                             ;; void (uninitialized)
  ;; Variable environment, vars mapped to their addresses in the heap 
  (ρ (env (x α) ...))
  ;; Address 
  (α number)
  ;; Evaluation contexts
  (E hole
     ;; P
     (E σ ρ)
     ;; rvalues 
     (use E)
     (& borrowkind E)
     (binop E rv)
     (binop const E)
     (unop E)
     (box E)
     (cast castkind E as ty)          
     (const ... E rv ...)))

(define run
  (reduction-relation
   mir-machine
   ;; Rvalues 
   (--> ((in-hole E (use x_0)) σ ρ) 
        ((in-hole E (deref σ ρ x_0)) σ ρ)
        "use")
   (--> ((in-hole E (& borrowkind x_0)) σ ρ) 
        ((in-hole E (env-lookup ρ x_0)) σ ρ)
        "ref")
   (--> ((in-hole E (binop const_1 const_2)) σ ρ)
        ((in-hole E (eval-binop binop const_1 const_2)) σ ρ)
        "binop")
   (--> ((in-hole E (unop const)) σ ρ)
        ((in-hole E (eval-unop unop const)) σ ρ)
        "unop")
   (--> ((in-hole E (cast castkind lv as ty)) σ ρ)
        ((in-hole E (eval-cast lv ty)) σ ρ)
        "typecast")
   ;; (--> (in-hole E ((vec ty len) σ ρ)) Vector creation -- call a vec new fn?
   ;; boxes, structs, tuples
   ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Metafunctions 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Perform the binary operation
(define-metafunction mir-machine
  eval-binop : binop const const -> const  
  [(eval-binop + const_1 const_2) ,(+ (term const_1) (term const_2))]
  [(eval-binop - const_1 const_2) ,(- (term const_1) (term const_2))]
  [(eval-binop * const_1 const_2) ,(* (term const_1) (term const_2))]
  [(eval-binop / const_1 const_2) ,(/ (term const_1) (term const_2))]
  [(eval-binop ^ const_1 const_2) ,(bitwise-xor (term const_1) (term const_2))]
  [(eval-binop & const_1 const_2) ,(bitwise-and (term const_1) (term const_2))]
  [(eval-binop \| const_1 const_2) ,(bitwise-ior (term const_1) (term const_2))]
  [(eval-binop << const_1 const_2) ,(arithmetic-shift (term const_1) (term const_2))]
  [(eval-binop >> const_1 const_2) ,(arithmetic-shift (term const_1) (term (eval-unop - const_2)))]
  [(eval-binop < const_1 const_2) ,(< (term const_1) (term const_2))]
  [(eval-binop <= const_1 const_2) ,(<= (term const_1) (term const_2))]
  [(eval-binop != const_1 const_2) ,(not (term (eval-binop == const_1 const_2)))]
  [(eval-binop == const_1 const_2) ,(= (term const_1) (term const_2))]
  [(eval-binop > const_1 const_2) ,(> (term const_1) (term const_2))]
  [(eval-binop >= const_1 const_2) ,(>= (term const_1) (term const_2))]
  [(eval-binop % const_1 const_2) ,(remainder (term const_1) (term const_2))])

;; Perform the unary operation 
(define-metafunction mir-machine
  eval-unop : unop const -> const
  [(eval-unop ! const) ,(not (term const))]
  [(eval-unop - const) ,(- (term const))])

;; A dummy method, reminder we need to deal with typecasts
;; FIXME:: is this the right signature? 
;; eval-cast : lv ty -> rv 
(define-metafunction mir-machine
  eval-cast : lv ty -> rv
  [(eval-cast lv ty) (use lv)])

;; deref : σ ρ x -> v 
(define-metafunction mir-machine
  ;; Returns the value mapped to this variable in the heap 
  deref : σ ρ x -> v
  [(deref σ ρ x) (store-lookup σ α)
                 (where α (env-lookup ρ x))])

;; malloc : σ len -> (σ α)
(define-metafunction mir-machine
  ;; Allocates len blocks of memory on the heap, returns the new base address
  ;; If len == 0, will not allocate memory but returns next available address 
  malloc : σ len -> (σ α)
  [(malloc σ_old len) (σ_new α_new)
                      (where (store (α_old v) ...) σ_old)
                      (where α_new ,(add1 (apply max (term (-1 α_old ...)))))
                      (where σ_new (extend σ_old α_new len))])

;; extend : σ α len -> σ
(define-metafunction mir-machine
  ;; Extends the heap with len blocks, starting at the given address 
  extend : σ α len -> σ
  [(extend σ α_new 0) σ]
  [(extend (store (α_old v) ...) α_new len)
   (extend (store (α_new void) (α_old v) ...)
           ,(add1 (term α_new))
           ,(sub1 (term len)))])

;; env-lookup : ρ x -> α
(define-metafunction mir-machine
  ;; Returns the address mapped to the variable x in the env 
  env-lookup : ρ x -> α
  [(env-lookup (env (x_1 α_1) ... (x_0 α_0) (x_2 α_2) ...) x_0) α_0]
  [(env-lookup (env (x_1 α_1) ...) x) ,(error "not found in environment:" (term x))])

;; store-lookup : σ α -> v
(define-metafunction mir-machine
  ;; Returns the value mapped to the address α in the store 
  store-lookup : σ α -> v
  [(store-lookup (store (α_1 v_1) ... (α_0 v_0) (α_2 v_2) ...) α_0) v_0]
  [(store-lookup (store (α_1 v_1) ...) α) ,(error "not found in store:" (term α))])

;; put : σ ρ x v -> σ
(define-metafunction mir-machine
  ;; Updates the value mapped to this variable in the heap
  put : σ ρ x v -> σ
  [(put (store (α_1 v_1) ... (α_0 v_old) (α_2 v_2) ...) ρ x_0 v_new)
   (store (α_1 v_1) ... (α_0 v_new) (α_2 v_2) ...)
   (where α_0 (env-lookup ρ x_0))]
  [(put (store (α_1 v_1) ...) ρ x v) ,(error "not found in store:" (term x))])