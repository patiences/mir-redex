#lang racket
(require redex)
(provide (all-defined-out))

;; A Redex model for the intermediate representation (MIR) inside the
;; Rust compiler.

;; This is an implementation that discards most type information
;; (except typed nums). Since we don't need type information to
;; evaluate the program in most cases, let's see what happens when
;; we don't have it.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;; Grammar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-language mir
  ;; program
  ;;     fns: list of functions
  (prog fns)
  
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
  (vars (let-vars ([= lv rv] ...))) ;; Are these lvalues distinct?
  
  ;; basic blocks, with a unique integer identifier, local variables and a terminator
  (bbs (let-bbs (blk ...)))
  (blk [bb idx_!_ vars terminator])
  
  ;; terminator
  (terminator return                              ;; return to caller
              resume                              ;; emitted by diverge call during unwinding 
              (switchInt lv                       ;; switch on an integer, branching to bb l 
                         (const idx) ...
                         (otherwise idx))
              (assert operand boolean idx msg)    ;; checked branch
              ;;    rv: condition
              ;;    idx: index of block to branch to, if rv evals to true
              ;;    msg: error message, if rv evals to false
              (assert operand boolean idx idx msg);; as above, with extra unwinding label 
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
      (cast castkind lv as ty)                    ;; typecasts
      (operand ...)                               ;; aggregate types i.e. tuples   
      (struct s ([= lv_!_ rv] ...)))              ;; structs ;; FIXME reconsider rvs? 
  ;; TODO: handle remaining rvalues 
  
  ;; A subset of rvalues that can be used inside other rvalues 
  (operand (use lv)
           const)
  
  ;; A subset of rvalues that can be evaluated at compile time
  ;; http://manishearth.github.io/rust-internals-docs/rustc_typeck/middle/const_val/enum.ConstVal.html
  (const boolean
         typed-num
         unit)                                    ;; unit values
  (typed-num (n numtype))
  (n number)
  
  ;; binary operation kinds
  (binop + - * / % ^ & \| << >> comp-binop)
  (comp-binop == < <= != >= >)
  
  ;; unary operation kinds
  (unop ! -)
  
  ;; cast kinds 
  (castkind misc
            reifyfp                               ;; ReifyFnPointer
            closurefp                             ;; ClosureFnPointer 
            unsafefp                              ;; UnsafeFnPointer
            unsize)                               ;; Unsize
  
  ;; error messages i.e. for asserts
  (msg string)
  
  ;; types of numbers
  (numtype i32 i64 u8 u32 u64)
  
  ;; borrow kinds
  (borrowkind shared unique mut)
  
  (idx integer)                                   ;; indices 
  (x variable-not-otherwise-mentioned)            ;; variables 
  (f integer)                                     ;; field "names"
  (g variable-not-otherwise-mentioned))           ;; function names

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-extended-language mir-machine mir
  ;; Variable environment, vars mapped to their store addresses 
  (ρ (env (x α) ...))
  ;; addresses
  (α (ptr variable-not-otherwise-mentioned))
  ;; the store, addresses mapped to variables 
  (σ (store (α v) ...))
  ;; the global stack, with the first being the current frame 
  (stack (stk frame ...))
  ;; a stack frame, local variables mapped to frame values 
  (frame (frm (x α) ...))
  ;; function call, with parameters 
  (callfn g (rv ...))
  ;; store values 
  (v α                                 ;; pointer
     typed-num                         ;; numerical value
     void                              ;; void (uninitialized)
     (α_0 α_1 ...))                    ;; aggregate values: keep addresses of contents
  ;; Evaluation contexts
  (E hole
     (prog E σ ρ stack)
     ;; basic blocks
     (bb idx E terminator)
     ;; variable statements
     (let-vars E)
     (void ... E (= lv rv) ...)
     (= E rv)
     (= x E)
     (= α E)
     ;; lvalues 
     (* E)
     (· E f)
     ;; rvalues
     (use E)
     (binop E rv)
     (binop const E)
     (unop E)))

(define run
  (reduction-relation
   mir-machine
   ;; load program 
   (--> prog
        (prog (callfn main ()) (store) (env) (stk))
        "call main")
   ;; call function 
   (--> (prog (in-hole E (callfn g (rv ...))) σ ρ stack)
        (prog (in-hole E (lookup-fn prog g)) σ_new ρ stack_new)
        (where (σ_new stack_new) (alloc-vars-in-fn (lookup-fn prog g) σ stack))
        "callfn")
   (--> (prog (in-hole E (g (x ...) bbs idx)) σ ρ stack)
        (prog (in-hole E (lookup-bb bbs idx)) σ ρ stack)
        "run-bb-0")
   ;; basic block control flow
   (--> (prog (in-hole E (bb idx void return)) σ ρ stack)
        (prog (in-hole E void) σ ρ stack)
        "ret")
   ;; Variable assignments
   (--> (prog (in-hole E (let-vars (void ...))) σ ρ stack)
        (prog (in-hole E void) σ ρ stack))
   (--> (prog (in-hole E (= x v)) σ ρ stack)
        (prog (in-hole E void) (store-update σ stack x v) ρ stack)
        "store-update-var")
   (--> (prog (in-hole E (= α v)) σ ρ stack)
        (prog (in-hole E void) (store-update-direct σ α v) ρ stack)
        "store-update-direct")
   ;; Lvalues 
   (--> (prog (in-hole E (* x)) σ ρ stack)
        (prog (in-hole E (deref σ stack x)) σ ρ stack)
        "deref")
   (--> (prog (in-hole E (· x f)) σ ρ stack)
        (prog (in-hole E (deref-projection σ stack x f)) σ ρ stack)
        "deref-projection")
   ;; Rvalues 
   (--> (prog (in-hole E (use x_0)) σ ρ stack) 
        (prog (in-hole E (deref σ stack x_0)) σ ρ stack)
        "use")
   (--> (prog (in-hole E (& borrowkind x_0)) σ ρ stack)
        ; assuming x_0 is in the current stack frame
        (prog (in-hole E (frm-lookup (list-ref stack 1) x_0)) σ ρ stack) 
        "ref")
   (--> (prog (in-hole E (binop const_1 const_2)) σ ρ stack)
        (prog (in-hole E (eval-binop binop const_1 const_2)) σ ρ stack)
        "binop")
   (--> (prog (in-hole E (unop const)) σ ρ stack)
        (prog (in-hole E (eval-unop unop const)) σ ρ stack)
        "unop")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Metafunctions 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-metafunction mir-machine
  deref : σ stack x -> v
  ;; Returns the value mapped to this variable in the store 
  [(deref σ stack x) (store-lookup σ α)
                     (where frame (list-ref stack 1)) ; look in the first frame 
                     (where α (frm-lookup frame x))])

(define-metafunction mir-machine
  deref-projection : σ stack x f -> v
  ;; Dereferences a projected value
  [(deref-projection σ stack x f) (store-lookup σ α_projected)                        
                                  (where (α_0 ...) (deref σ stack x))
                                  (where α_projected (list-ref (α_0 ...) f))])

(define-metafunction mir-machine
  list-ref : any idx -> any
  ;; Returns the ith element of a list with form like (frm (x_0 v_0) ...)
  [(list-ref () idx) ,(error "list-ref: index not in range:" (term idx))]
  [(list-ref (any_0 any_1 ...) 0) any_0]
  [(list-ref (any_0 any_1 ...) idx) (list-ref (any_1 ...) ,(sub1 (term idx)))]) 

(define-metafunction mir-machine
  store-lookup : σ α -> v
  ;; Returns the value mapped to the address α in the store 
  [(store-lookup (store (α_1 v_1) ... (α_0 v_0) (α_2 v_2) ...) α_0) v_0]
  [(store-lookup (store (α_1 v_1) ...) α) ,(error "store-lookup: address not found in store:" (term α))])

(define-metafunction mir-machine
  store-update : σ stack x v -> σ
  ;; Updates the value mapped to this variable in the heap
  [(store-update (store (α_1 v_1) ... (α_0 v_old) (α_2 v_2) ...) stack x_0 v_new)
   (store (α_1 v_1) ... (α_0 v_new) (α_2 v_2) ...)
   (where frame (list-ref stack 1))
   (where α_0 (frm-lookup frame x_0))]
  [(store-update (store (α_1 v_1) ...) stack x v) ,(error "store-update: address not found in store:" (term x))])

(define-metafunction mir-machine
  store-update-direct : σ α v -> σ
  ;; Updates the value at the address in the store
  [(store-update-direct (store (α_1 v_1) ... (α_0 v_old) (α_2 v_2) ...) α_0 v_new)
   (store (α_1 v_1) ... (α_0 v_new) (α_2 v_2) ...)]
  [(store-update-direct (store (α_1 v_1) ...) α v) ,(error "store-update-direct: address not found in store:" (term α))])

(define-metafunction mir-machine
  frm-lookup : frame x -> α
  ;; Returns the address mapped to the variable x in the frame 
  [(frm-lookup (frm (x_1 α_1) ... (x_0 α_0) (x_2 α_2) ...) x_0) α_0]
  [(frm-lookup (frm (x_1 α_1) ...) x) ,(error "frm-lookup: variable not found in frame:" (term x))])

(define-metafunction mir-machine
  env-lookup : ρ x -> α
  ;; Returns the address mapped to the variable x in the env 
  [(env-lookup (env (x_1 α_1) ... (x_0 α_0) (x_2 α_2) ...) x_0) α_0]
  [(env-lookup (env (x_1 α_1) ...) x) ,(error "env-lookup: variable not found in environment:" (term x))])

(define-metafunction mir-machine
  alloc-vars-in-fn : fn σ stack -> (σ stack)
  ;; Create a stack frame, allocate space in the frame and heap for all variables in the function 
  [(alloc-vars-in-fn fn σ (stk frame ...)) (σ_new (stk frame_new frame ...))
                                     (where (σ_new frame_new) (alloc-vars-in-fn-helper fn σ (frm)))])

(define-metafunction mir-machine
  alloc-vars-in-fn-helper : fn σ frame -> (σ frame)
  ;; Allocate space in the frame and heap for all variables in the function 
  ;; No bbs
  [(alloc-vars-in-fn-helper (g (x ...) (let-bbs ()) idx) σ frame) (σ frame)]
  ;; Traverse bbs 
  [(alloc-vars-in-fn-helper (g (x ...) (let-bbs (blk_0 blk_1 ...)) idx) σ_old frame_old)
   (alloc-vars-in-fn-helper (g (x ...) (let-bbs (blk_1 ...)) idx) σ_new frame_new)
   (where (σ_new frame_new) (alloc-vars-in-bb blk_0 σ_old frame_old))])

(define-metafunction mir-machine
  alloc-vars-in-bb : blk σ frame -> (σ frame)
  ;; Allocate space in the stack frame and the heap for all variables in the block 
  [(alloc-vars-in-bb (bb idx (let-vars ()) terminator) σ frame) (σ frame)]
  [(alloc-vars-in-bb (bb idx (let-vars ([= lv_0 rv_0] [= lv_1 rv_1] ...)) terminator) σ frame)
   (alloc-vars-in-bb (bb idx (let-vars ([= lv_1 rv_1] ...)) terminator) σ_new frame_new)
   (where (σ_new frame_new) (alloc-var lv_0 σ frame))])

;; This function assumes we never have projections/derefs on the left
;; side of the assignment (which I haven't seen yet and seems to make
;; sense). 
(define-metafunction mir-machine
  alloc-var : lv σ frame -> (σ frame)
  ;; Allocate space for this variable if necessary
  ;; x_0 has already been allocated, return 
  [(alloc-var x_0 σ (frm (x_1 α_1) ... (x_0 α_0) (x_2 α_2) ...)) (σ (frm (x_1 α_1) ... (x_0 α_0) (x_2 α_2) ...))]
  [(alloc-var x_0 σ (frm (x α) ...))
   (σ_new (frm (x_0 α_new) (x α) ...))
   (where (σ_new α_new) (malloc σ))])

(define-metafunction mir-machine
  malloc : σ -> (σ α)
  ;; Allocate a space in the heap and return the address
  [(malloc (store (α v) ...))
   ((store (α_new void) (α v) ...) α_new)
   (where α_new (ptr ,(gensym)))])

(define-metafunction mir-machine
  is-allocated : x σ frame -> boolean
  ;; Returns true if x has been allocated in the heap and stack frame
  [(is-allocated x_0
                 (store (α_1 v_1) ... (α_0 v_0) (α_2 v_2) ...)
                 (frm (x_1 α_1) ... (x_0 α_0) (x_2 α_2) ...))
   #t]
  [(is-allocated x σ frame) #f])

(define-metafunction mir-machine
  eval-binop : binop const const -> const
  ;; Evaluate the expression depending on the type of operation
  ;; TODO: Non-numeric operands
  ;; i.e. http://manishearth.github.io/rust-internals-docs///rustc/middle/const_val/enum.ConstVal.html?
  ;; TODO: Handle checked operations, see test-lang#225
  [(eval-binop comp-binop const_1 const_2)
   (eval-comp-binop-helper comp-binop n_1 n_2)
   (where n_1 ,(car (term const_1)))
   (where n_2 ,(car (term const_2)))]
  [(eval-binop binop const_1 const_2)
   ((eval-binop-helper binop n_1 n_2) numtype_1)
   (where n_1 ,(car (term const_1)))
   (where n_2 ,(car (term const_2)))
   (where numtype_1 ,(cadr (term const_1)))])

(define-metafunction mir-machine
  eval-binop-helper : binop n n -> any
  [(eval-binop-helper + n_1 n_2) ,(+ (term n_1) (term n_2))] 
  [(eval-binop-helper - n_1 n_2) ,(- (term n_1) (term n_2))]
  [(eval-binop-helper * n_1 n_2) ,(* (term n_1) (term n_2))]
  [(eval-binop-helper / n_1 n_2) ,(/ (term n_1) (term n_2))]
  [(eval-binop-helper ^ n_1 n_2) ,(bitwise-xor (term n_1) (term n_2))]
  [(eval-binop-helper & n_1 n_2) ,(bitwise-and (term n_1) (term n_2))]
  [(eval-binop-helper \| n_1 n_2) ,(bitwise-ior (term n_1) (term n_2))]
  [(eval-binop-helper << n_1 n_2) ,(arithmetic-shift (term n_1) (term n_2))]
  [(eval-binop-helper >> n_1 n_2) ,(arithmetic-shift (term n_1) (term (eval-unop - n_2)))]
  [(eval-binop-helper % n_1 n_2) ,(remainder (term n_1) (term n_2))])

(define-metafunction mir-machine
  eval-comp-binop-helper : binop n n -> boolean
  [(eval-comp-binop-helper < n_1 n_2) ,(< (term n_1) (term n_2))]
  [(eval-comp-binop-helper <= n_1 n_2) ,(<= (term n_1) (term n_2))]
  [(eval-comp-binop-helper != n_1 n_2) ,(not (term (eval-binop == n_1 n_2)))]
  [(eval-comp-binop-helper == n_1 n_2) ,(= (term n_1) (term n_2))]
  [(eval-comp-binop-helper > n_1 n_2) ,(> (term n_1) (term n_2))]
  [(eval-comp-binop-helper >= n_1 n_2) ,(>= (term n_1) (term n_2))])

(define-metafunction mir-machine
  eval-unop : unop const -> const
  [(eval-unop unop (n_1 numtype_1))
   ((eval-unop-helper unop n_1) numtype_1)]
  [(eval-unop unop boolean) (eval-unop-helper unop boolean)])

(define-metafunction mir-machine
  eval-unop-helper : unop any -> any
  [(eval-unop-helper ! any) ,(not (term any))]
  [(eval-unop-helper - any) ,(- (term any))])

(define-metafunction mir-machine
  lookup-bb : bbs idx -> blk
  ;; Find the block with the given index 
  [(lookup-bb (let-bbs ([bb idx_1 vars_1 terminator_1] ...
                        [bb idx_start vars_start terminator_start]
                        [bb idx_2 vars_2 terminator_2] ...))
              idx_start)
   (bb idx_start vars_start terminator_start)]
  [(lookup-bb bbs idx) ,(error "lookup-bb: basic block with index not found:" (term idx))])

(define-metafunction mir-machine
  lookup-fn : prog g -> fn
  ;; Find the function with the given name
  [(lookup-fn ((g_1 (x_1 ...) bbs_1 idx_1) ... (g_0 (x_0 ...) bbs_0 idx_0) (g_2 (x_2 ...) bbs_2 idx_2) ...) g_0)
   (g_0 (x_0 ...) bbs_0 idx_0)]
  [(lookup-fn prog g) ,(error "lookup-fn: function with name not found:" (term g))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Metafunctions for testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-metafunction mir-machine
  list-length : (any ...) -> integer
  ;; Returns the length of a list of the form (list-type element_0 ...)
  [(list-length (any_0)) 0]
  [(list-length (any_0 any_1 ...)) ,(add1 (term (list-length (any_1 ...))))])

(define-metafunction mir-machine
  get-stack : (prog v σ ρ stack) -> stack
  ;; Get the stack from the result of a reduction 
  [(get-stack (prog v σ ρ stack)) stack])

(define-metafunction mir-machine
  get-store : (prog v σ ρ stack) -> σ
  ;; Get the store from the result of a reduction
  [(get-store (prog v σ ρ stack)) σ])