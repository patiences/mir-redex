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
  (g variable-not-otherwise-mentioned)            ;; function names
  (s variable-not-otherwise-mentioned))


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
  ;;
  ;; the stack frame, local variables mapped to frame values 
  (frame (frm (x frm-v) ...))
  ;; frame values
  (frm-v v                             ;; heap addresses, nums, void 
         (list x ...))                 ;; aggregate values like tuples, vecs.
                                       ;;     this is a pointer to each of the
                                       ;;     contents in the aggregate data structure 
  ;; store values 
  (v α                                 ;; pointer
     typed-num                         ;; numerical value
     void)                             ;; void (uninitialized)
  ;; Evaluation contexts
  (E hole
     ;; single function
     #;(E σ ρ)
     (E σ ρ frame)
     ;; variable statements
     (let-vars E)
     (void ... E (= lv rv) ...)
     (= E rv)
     (= x E)
     (= α E)
     ;; lvalues 
     (* E)
     ;; TODO: projection 
     ;; rvalues
     (use E)
     (binop E rv)
     (binop const E)
     (unop E)))

(define run
  (reduction-relation
   mir-machine
   ;; functions
   #; (--> ((in-hole E (g (x ...)
                       (let-bbs ([bb idx_1 vars_1 terminator_1]...
                                 [bb idx_start vars_start terminator_start]
                                 [bb idx_2 vars_2 terminator_2] ...))
                       idx_start)) σ ρ)
        (g (x ...)
           (let-bbs ([bb idx_1 vars_1 terminator_1]...
                     [bb idx_start vars_start terminator_start]
                     [bb idx_2 vars_2 terminator_2] ...))
           (in-hole E ((bb idx_start vars_start terminator_start) σ ρ)))) 
   
   ;; Variable assignments
   (--> ((in-hole E (let-vars (void ...))) σ ρ frame)
        ((in-hole E void) σ ρ))
   
   (--> ((in-hole E (= x frm-v)) σ ρ frame)                 ;; update the value of the local variable in the frame           
        ((in-hole E void) σ ρ (update frame x frm-v))
        "store-update-var")
   
   (--> ((in-hole E (= α v)) σ ρ)                           ;; update the value of the variable in memory 
        ((in-hole E void) (store-update-direct σ α v) ρ frame)
        "store-update-direct")
   
   ;; Lvalues 
   (--> ((in-hole E (* x)) σ ρ frame)
        ((in-hole E (deref frame x)) σ ρ frame)
        "deref")
   (-->((in-hole E (· x f)) σ ρ frame)
       ((in-hole E (deref frame x.f)) σ ρ frame)
       (where x.f (get-projection x f))
       "projection")
   ;; Rvalues 
   (--> ((in-hole E (use x_0)) σ ρ frame) 
        ((in-hole E (lookup frame x_0)) σ ρ frame)
        "use")
   (--> ((in-hole E (& borrowkind x_0)) σ ρ frame) 
        ((in-hole E (env-lookup ρ x_0)) σ ρ frame)
        "ref")
   (--> ((in-hole E (binop const_1 const_2)) σ ρ frame)
        ((in-hole E (eval-binop binop const_1 const_2)) σ ρ frame)
        "binop")
   (--> ((in-hole E (unop const)) σ ρ frame)
        ((in-hole E (eval-unop unop const)) σ ρ frame)
        "unop")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Metafunctions 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-metafunction mir-machine
  ;; Returns the key pointer to the field inside the aggregate object 
  get-projection : frame x f -> x
  [(get-projection (frm (x_1 frm-v_1) ... (x_0 (list x ...)) (x_2 frm-v_2) ...) x_0 f)
   ,(list-ref (term (list x ...)) (term f))]
  [(get-projection (frm (x_1 frm-v_1) ... (x_0 frm-v_0) (x_2 frm-v_2) ...) x_0 f)
   ,(error "get-projection: not an aggregate object:" (term x_0))]
  [(get-projection (frm (x_1 frm-v_1) ...) x f)
   ,(error "get-projection: variable not in frame:" (term x))])
  
(define-metafunction mir-machine
 update : frame x frm-v -> frame
  [(update (frm (x_1 frm-v_1) ... (x_0 frm-v_old) (x_2 frm-v_2) ...) x_0 frm-v_new)
   (frm (x_1 frm-v_1) ... (x_0 frm-v_new) (x_2 frm-v_2) ...)]
  [(update (frm (x_1 frm-v_1) ...) x frm-v_new)
   ,(error "update: variable not found in frame:" (term x))])

(define-metafunction mir-machine
  ;; Lookup the value of the variable in the stack frame
  lookup : frame x -> frm-v
  [(lookup (frm (x_1 frm-v_1) ... (x_0 frm-v_0) (x_2 frm-v_2) ...) x_0) frm-v_0]
  [(lookup frame x) ,(error "lookup: variable not found in frame:" (term x))])

(define-metafunction mir-machine
  ;; Returns the address mapped to this variable in the stack frame 
  deref : frame x -> α
  [(deref (frm (x_1 frm-v_1) ... (x_0 α_0) (x_2 frm-v_2) ...) x_0)
   α_0]
  [(deref (frm (x_1 frm-v_1) ... (x_0 frm-v_0) (x_2 frm-v_2) ...) x_0)
   ,(error "deref: invalid dereference, not a pointer:" (term x_0))]
  [(deref (frm (x_1 frm-v_1) ...) x) ,(error "deref: variable not found in frame:" (term x))])

#;(define-metafunction mir-machine
  ;; Returns the value mapped to this variable in the store 
  deref : σ ρ x -> v
  [(deref σ ρ x) (store-lookup σ α)
                 (where α (env-lookup ρ x))])

(define-metafunction mir-machine
  ;; Returns the value mapped to the address α in the store 
  store-lookup : σ α -> v
  [(store-lookup (store (α_1 v_1) ... (α_0 v_0) (α_2 v_2) ...) α_0) v_0]
  [(store-lookup (store (α_1 v_1) ...) α) ,(error "store-lookup: address not found in store:" (term α))])

(define-metafunction mir-machine
  ;; Updates the value mapped to this variable in the heap
  store-update : σ ρ x v -> σ
  [(store-update (store (α_1 v_1) ... (α_0 v_old) (α_2 v_2) ...) ρ x_0 v_new)
   (store (α_1 v_1) ... (α_0 v_new) (α_2 v_2) ...)
   (where α_0 (env-lookup ρ x_0))]
  [(store-update (store (α_1 v_1) ...) ρ x v) ,(error "store-update: address not found in store:" (term x))])

(define-metafunction mir-machine
  ;; Updates the value at the address in the store
  store-update-direct : σ α v -> σ
  [(store-update-direct (store (α_1 v_1) ... (α_0 v_old) (α_2 v_2) ...) α_0 v_new)
   (store (α_1 v_1) ... (α_0 v_new) (α_2 v_2) ...)]
  [(store-update-direct (store (α_1 v_1) ...) α v) ,(error "store-update-direct: address not found in store:" (term α))])

(define-metafunction mir-machine
  ;; Returns the address mapped to the variable x in the env 
  env-lookup : ρ x -> α
  [(env-lookup (env (x_1 α_1) ... (x_0 α_0) (x_2 α_2) ...) x_0) α_0]
  [(env-lookup (env (x_1 α_1) ...) x) ,(error "env-lookup: variable not found in environment:" (term x))])

(define-metafunction mir-machine
  ;; TODO: Non-numeric operands
  ;; i.e. http://manishearth.github.io/rust-internals-docs///rustc/middle/const_val/enum.ConstVal.html?
  ;; TODO: Handle checked operations, see test-lang#225
  eval-binop : binop const const -> const
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
