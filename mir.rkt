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
      (vec α len cap)                   ;; vectors, with a base address, length and capacity  
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
      (vec ty)                          ;; vectors 
      (& mq ty)                         ;; reference types with qualifiers
      (* mq ty)                         ;; raw pointer types with qualifiers
      (box ty)                          ;; box 
      ;; FIXME: multiple generic types https://doc.rust-lang.org/book/generics.html
      T)                                ;; generic            
  
  ;; error messages
  (msg string)
  
  (len integer)                         ;; vector length 
  (cap integer)                         ;; vector capacity 
  
  (α integer)                           ;; addresses 
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
  ;;    st: a single statement ;;TODO: eventually work up to prog... 
  ;;    σ: the store (heap)
  ;;    ρ: the global variable table 
  (P (st σ ρ))
  ;; the heap, addresses mapped to their heap value
  (σ (store (α v) ...))
  (v (ptr α)                           ;; pointer
     number                            ;; numerical value
     void)                             ;; void (uninitialized)
  ;; Variable environment, vars mapped to their addresses in the heap 
  (ρ (env (x α) ...))
  ;; Typing environment, variables mapped to their data types  
  (Γ (tenv (x ty) ...))
  ;; Evaluation contexts
  (E hole
     ;; P
     (E σ ρ)
     ;; functions
     (E σ ρ Γ) 
     (-> g vdecls ty
         ;; Reduce declarations 
         void ... E decl ...
         blk ...) 
     ;; statements
     (void ... E st ...)
     (= E rv)
     (= x E)
     (= (ptr α) E)
     ;; lvalues
     (* E)
     (· E f)
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
   ;; Variable declarations
   (--> ((in-hole E (x : ty)) σ ρ Γ)
        ((in-hole E void) σ ρ (tenv-update Γ x ty))
        "vdecl")
   (--> ((in-hole E (mq x : ty)) σ ρ Γ)
        ((in-hole E void) σ ρ (tenv-update Γ x ty)) ;; FIXME: How to deal with mq? 
        "vdecl with mq")
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
   ;; Assignment
   (--> ((in-hole E (= x v)) σ ρ)
        ((in-hole E void) (store-update σ ρ x v) ρ)
        "store-update-var")
   (--> ((in-hole E (= (ptr α) v)) σ ρ)
        ((in-hole E void) (store-update-direct σ α v) ρ)
        "store-update-direct")
   ;; Lvalues 
   (--> ((in-hole E (* x)) σ ρ)
        ((in-hole E (deref σ ρ x)) σ ρ)
        "deref")
   (--> ((in-hole E (· x f)) σ ρ)
        ((in-hole E (ptr ,(+ (term (env-lookup ρ x)) (term f)))) σ ρ)
        "project")
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
                      (where σ_new (store-extend σ_old α_new len))])

;; store-extend : σ α len -> σ
(define-metafunction mir-machine
  ;; Extends the heap with len blocks, starting at the given address 
  store-extend : σ α len -> σ
  [(store-extend σ α_new 0) σ]
  [(store-extend (store (α_old v) ...) α_new len)
   (store-extend (store (α_new void) (α_old v) ...)
                 ,(add1 (term α_new))
                 ,(sub1 (term len)))])

;; env-lookup : ρ x -> α
(define-metafunction mir-machine
  ;; Returns the address mapped to the variable x in the env 
  env-lookup : ρ x -> α
  [(env-lookup (env (x_1 α_1) ... (x_0 α_0) (x_2 α_2) ...) x_0) α_0]
  [(env-lookup (env (x_1 α_1) ...) x) ,(error "env-lookup: variable not found in environment:" (term x))])

;; store-lookup : σ α -> v
(define-metafunction mir-machine
  ;; Returns the value mapped to the address α in the store 
  store-lookup : σ α -> v
  [(store-lookup (store (α_1 v_1) ... (α_0 v_0) (α_2 v_2) ...) α_0) v_0]
  [(store-lookup (store (α_1 v_1) ...) α) ,(error "store-lookup: address not found in store:" (term α))])

;; store-update : σ ρ x v -> σ
(define-metafunction mir-machine
  ;; Updates the value mapped to this variable in the heap
  store-update : σ ρ x v -> σ
  [(store-update (store (α_1 v_1) ... (α_0 v_old) (α_2 v_2) ...) ρ x_0 v_new)
   (store (α_1 v_1) ... (α_0 v_new) (α_2 v_2) ...)
   (where α_0 (env-lookup ρ x_0))]
  [(store-update (store (α_1 v_1) ...) ρ x v) ,(error "store-update: address not found in store:" (term x))])

;; store-update-direct : σ α v -> σ
(define-metafunction mir-machine
  ;; Updates the value at the address in the store
  store-update-direct : σ α v -> σ
  [(store-update-direct (store (α_1 v_1) ... (α_0 v_old) (α_2 v_2) ...) α_0 v_new)
   (store (α_1 v_1) ... (α_0 v_new) (α_2 v_2) ...)]
  [(store-update-direct (store (α_1 v_1) ...) α v) ,(error "store-update-direct: address not found in store:" (term α))])

;; store-remove : σ α len -> σ
(define-metafunction mir-machine
  ;; Remove len continuous blocks from the store starting at address α
  store-remove : σ α len -> σ
  [(store-remove σ α 0) σ]
  [(store-remove (store (α_1 v_1) ... (α_0 v_0) (α_2 v_2) ...) α_0 len)
   (store-remove (store (α_1 v_1) ... (α_2 v_2) ...)
                 ,(add1 (term α_0))
                 ,(sub1 (term len)))]
  [(store-remove (store (α_1 v_1) ...) α len)
   ,(error "store-remove: address not found in store:" (term α))])

;; copy : σ α α len -> σ
(define-metafunction mir-machine
  ;; Copy len continuous blocks starting from one address to another
  copy : σ α α len -> σ
  [(copy σ α_old α_new 0) σ]
  ;; old address is in front of new address 
  [(copy (store (α_1 v_1) ... (α_old v_tocopy) (α_2 v_2) ... (α_new v) (α_3 v_3) ...)
         α_old α_new len)
   (copy (store (α_1 v_1) ... (α_old v_tocopy) (α_2 v_2) ... (α_new v_tocopy) (α_3 v_3) ...)
         ,(add1 (term α_old))
         ,(add1 (term α_new))
         ,(sub1 (term len)))]
  ;; new address is in front of old address
  [(copy (store (α_1 v_1) ... (α_new v) (α_2 v_2) ... (α_old v_tocopy) (α_3 v_3) ...)
         α_old α_new len)
   (copy (store (α_1 v_1) ... (α_new v_tocopy) (α_2 v_2) ... (α_old v_tocopy) (α_3 v_3) ...)
         ,(add1 (term α_old))
         ,(add1 (term α_new))
         ,(sub1 (term len)))]
  [(copy (store (α_1 v_1) ... (α_old v_tocopy) (α_2 v_2) ...) α_old α_new len)
   ,(error "copy: attempted write to address not found in store:" (term α_new))]
  [(copy (store (α_1 v_1) ...) α_old α_new len)
   ,(error "copy: attempted read from address not found in store:" (term α_old))])

;; tenv-lookup: Γ x -> ty 
(define-metafunction mir-machine
  ;; Lookup the type of the variable x in the type env 
  tenv-lookup : Γ x -> ty
  [(tenv-lookup (tenv (x_1 ty_1) ... (x_0 ty_0) (x_2 ty_2) ...) x_0) ty_0]
  [(tenv-lookup (tenv (x_1 ty_1) ...) x) ,(error "tenv-lookup: variable not found in type environment:" (term x))])

;; tenv-update : Γ x ty -> Γ
(define-metafunction mir-machine
  ;; Update the type of this variable in the type env
  tenv-update : Γ x ty -> Γ
  [(tenv-update (tenv (x_1 ty_1) ... (x_0 ty_old) (x_2 ty_2) ...) x_0 ty_new)
   (tenv (x_1 ty_1) ... (x_0 ty_new) (x_2 ty_2) ...)]
  ;; Not yet defined in tenv, let's add it 
  [(tenv-update (tenv (x_1 ty_1) ...) x ty) (tenv-add (tenv (x_1 ty_1) ...) x ty)])

;; tenv-add : Γ x ty -> Γ
(define-metafunction mir-machine
  ;; Add this variable and its type to the type env
  tenv-add : Γ x ty -> Γ
  [(tenv-add (tenv (x_1 ty_1) ...) x ty) (tenv (x ty) (x_1 ty_1) ...)])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Standard functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Create a new empty vec (no allocation) 
(define-metafunction mir-machine
  vec-new : σ -> (σ (vec α len cap))
  [(vec-new σ) (σ (vec ,(cadr (term (malloc σ 0))) 0 0))])

;; Create a new empty vec with capacity
(define-metafunction mir-machine
  vec-with-capacity : σ cap -> (σ (vec α len cap))
  [(vec-with-capacity σ cap)
   (σ_new (vec α_new 0 cap))
   (where (σ_new α_new) (malloc σ cap))])

;; Pushes a value onto the back of the vec
(define-metafunction mir-machine
  ;; FIXME assume we only have vectors of "heap values" (size 1) i.e. ptrs or nums
  ;; Defer to when we can look up the size of the type and calculate offset accordingly.
  vec-push : σ (vec α len cap) v -> (σ (vec α len cap))
  [(vec-push σ (vec α_old 0 0) v)                           ;; unallocated vecs get cap. 4
   (vec-push σ_new (vec α_new 0 4) v)
   (where (σ_new α_new) (malloc σ 4))] 
  [(vec-push σ (vec α len cap) v)                           ;; not at capacity 
   ((store-update-direct σ ,(+ (term len) (term α)) v) (vec α ,(add1 (term len)) cap))
   (side-condition (< (term len) (term cap)))]
  [(vec-push σ (vec α_old len cap_old) v)                   ;; at capacity, must resize (double current cap.)
   (vec-push σ_new (vec α_new len cap_new) v)
   (where (σ_new (vec α_new len cap_new)) (vec-resize σ (vec α_old len cap_old)))])

(define-metafunction mir-machine
  ;; Resize vector, assign new address, copy over old contents 
  vec-resize : σ (vec α len cap) -> (σ (vec α len cap))
  [(vec-resize σ (vec α_old len cap_old))
   (σ_remove_old (vec α_new len ,(* (term cap_old) 2)))
   (where (σ_malloc_new α_new) (malloc σ ,(* (term cap_old) 2)))
   (where σ_copy_old (copy σ_malloc_new α_old α_new len))
   (where σ_remove_old (store-remove σ_copy_old α_old len))])
