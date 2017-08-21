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
              (switchInt operand                  ;; switch on an integer, branching to bb l 
                         (const idx) ...
                         (otherwise idx))
              (assert operand boolean idx msg)    ;; checked branch
              ;;    operand: condition
              ;;    idx: index of block to branch to, if rv evals to true
              ;;    msg: error message, if rv evals to false
              (assert operand boolean idx idx msg);; as above, with extra unwinding label 
              (call lv g (operand ...) idx)       ;; lv gets the result of calling g(operands ...), branch to l on return  
              (call lv g (operand ...) idx idx)   ;; as above, with extra unwinding label  
              (goto idx)                          ;; goto l
              (drop lv idx)                       ;; drop lv, goto l 
              (drop lv idx idx))                  ;; as above, with extra unwinding label
  
  ;; lvalues
  (lv x
      (· lv f)                                    ;; projection (e.g. tuple access)
      (* lv))                                     ;; pointer deref
  
  ;; rvalue
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
  (s variable-not-otherwise-mentioned))           ;; struct names

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Evaluation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-extended-language mir-machine mir
  ;; Variable environment, vars mapped to their store addresses 
  (ρ (env (x α) ...))
  ;; addresses
  (α (ptr variable-not-otherwise-mentioned))
  ;; the store, addresses mapped to values 
  (σ (store (α v) ...))
  ;; the global data stack, with the first being the current frame 
  (δ (stk frame ...))
  ;; a stack frame, with the function we're in and its local variables mapped to frame values 
  (frame (frm (x α) ...))
  ;; function call, with parameters 
  (callfn g (rv ...))
  ;; store values 
  (v α                                 ;; pointer
     typed-num                         ;; numerical value
     boolean 
     void                              ;; void (uninitialized)
     (α_0 α_1 ...))                    ;; aggregate values: keep addresses of contents
  ;; Evaluation contexts
  (E hole
     (prog (in-call fn E) σ ρ δ)
     (prog E σ ρ δ)
     ;; basic blocks
     (bb idx E terminator)
     (bb idx void (assert E boolean idx msg))
     (bb idx void (switchInt E (const idx_1) ... (otherwise idx_2)))
     (bb idx void (call lv g (operand ...) idx))
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
     (unop E)
     (const ... E rv ...)))

(define run
  (reduction-relation
   mir-machine
   ;; load program 
   (--> prog
        (prog (callfn main ()) (store) (env) (stk))
        "call main")
   ;; call function 
   (--> (prog (in-hole E (callfn g (operand ...))) σ ρ δ)
        (prog (in-call (lookup-fn prog g) (in-hole E (lookup-fn prog g)))
              σ_new ρ δ_new)
        (where (σ_new δ_new) (alloc-vars-in-fn (lookup-fn prog g) σ δ))
        "callfn")
   (--> (prog (in-call fn (in-hole E (g (x ...) bbs idx))) σ ρ δ)
        (prog (in-call fn (in-hole E (lookup-bb bbs idx))) σ ρ δ)
        "run-bb-0")
   ;; basic block control flow
   (--> (prog (in-call fn (in-hole E (bb idx void return))) σ ρ δ)
        (prog (get-return-value δ σ) σ ρ δ) 
        "ret")
   (--> (prog (in-call fn (in-hole E (bb idx void (goto idx_next)))) σ ρ δ)
        (prog (in-call fn (in-hole E (next-bb fn idx_next))) σ ρ δ)
        "goto")        
   (--> (prog (in-call fn (in-hole E (bb idx void (switchInt const_0 (const_1 idx_1) ... (otherwise idx_2))))) σ ρ δ)
        (prog (in-call fn (in-hole E (next-bb fn (lookup-next-bb-idx (switchInt const_0 (const_1 idx_1) ... (otherwise idx_2))))))
              σ ρ δ)
        "switchInt")
   (--> (prog (in-call fn (in-hole E (bb idx void (assert const boolean idx_next msg)))) σ ρ δ)
        (prog (in-call fn (in-hole E (next-bb fn idx_next))) σ ρ δ)
        (side-condition (equal? (term const) (term boolean)))
        "checked-branch-ok")
   (--> (prog (in-call fn (in-hole E (bb idx void (assert const boolean idx_next msg)))) σ ρ δ)
        (prog msg σ ρ δ)
        "checked-branch-error")   
;   (--> (prog (in-call fn (in-hole E (bb idx void (call lv g (operand ...) idx)))))
;        (prog (in-call fn (in-hole E (bb
;                                      idx
;                                      (let-vars ([= lv (get-return-value-from-reduction (prog (callfn g (operand ...)) (store) (env) (stk)))]))
;                                      (goto idx_next))))
;              σ ρ δ)
;        "call")
   
;   (--> (prog (in-call fn (in-hole E (bb idx void (call lv g (operand ...) idx)))))
;        (prog (in-call fn (in-hole E (bb
;                                      idx
;                                      (let-vars ([= lv (get-return-value-from-reduction (prog (in-hole E (callfn g (operand ...))) (store) (env) (stk)))]))
;                                      (goto idx_next))))
;              σ ρ δ)
;        "call")

;   (--> (prog (in-call fn (in-hole E (bb idx void (call lv g (operand ...) idx)))))
;        (prog (in-call fn (in-hole E (bb
;                                      idx
;                                      (let-vars ([= lv return-val]))
;                                      (goto idx_next))))
;              σ ρ δ)
;        (where return-val (get-return-value-from-reduction (prog (in-hole E (callfn g (operand ...))) (store) (env) (stk))))
;        "call")
   ;; Variable assignments
   (--> (prog (in-call fn (in-hole E (let-vars (void ...)))) σ ρ δ)
        (prog (in-call fn (in-hole E void)) σ ρ δ)
        "done-allocation")
   (--> (prog (in-call fn (in-hole E (= x v))) σ ρ δ)
        (prog (in-call fn (in-hole E void)) (store-update σ δ x v) ρ δ)
        "store-update-var")
   (--> (prog (in-call fn (in-hole E (= α v))) σ ρ δ)
        (prog (in-call fn (in-hole E void)) (store-update-direct σ α v) ρ δ)
        "store-update-direct")
   ;; FIXME for now this will work for numbers (which are vs and also consts)
   ;; But this reduction relation should really work on consts (same as above)
   (--> (prog (in-call fn (in-hole E (= x (v ...)))) σ ρ δ) 
        (prog (in-call fn (in-hole E void)) (store-update-aggregate σ δ x (v ...)) ρ δ)
        "store-update-aggregate")
   ;; Lvalues 
   (--> (prog (in-call fn (in-hole E (* x))) σ ρ δ)
        (prog (in-call fn (in-hole E (deref σ δ x))) σ ρ δ)
        "deref")
   (--> (prog (in-call fn (in-hole E (· x f))) σ ρ δ)
        (prog (in-call fn (in-hole E (deref-projection σ δ x f))) σ ρ δ)
        "deref-projection")
   ;; Rvalues 
   (--> (prog (in-call fn (in-hole E (use x_0))) σ ρ δ) 
        (prog (in-call fn (in-hole E (deref σ δ x_0))) σ ρ δ)
        "use")
   (--> (prog (in-call fn (in-hole E (& borrowkind x_0))) σ ρ δ)
        ; assuming x_0 is in the current stack frame
        (prog (in-call fn (in-hole E (frm-lookup (list-ref δ 1) x_0))) σ ρ δ) 
        "ref")
   (--> (prog (in-call fn (in-hole E (binop const_1 const_2))) σ ρ δ)
        (prog (in-call fn (in-hole E (eval-binop binop const_1 const_2))) σ ρ δ)
        "binop")
   (--> (prog (in-call fn (in-hole E (unop const))) σ ρ δ)
        (prog (in-call fn (in-hole E (eval-unop unop const))) σ ρ δ)
        "unop")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Metafunctions 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define-metafunction mir-machine
  deref : σ δ x -> v
  ;; Returns the value mapped to this variable in the store 
  [(deref σ δ x) (store-lookup σ α)
                 (where frame (list-ref δ 1)) ; look in the first frame 
                 (where α (frm-lookup frame x))])

(define-metafunction mir-machine
  deref-projection : σ δ x f -> v
  ;; Dereferences a projected value
  [(deref-projection σ δ x f) (store-lookup σ α_projected)                        
                              (where (α_0 ...) (deref σ δ x))
                              (where α_projected (list-ref (α_0 ...) f))])

(define-metafunction mir-machine
  list-ref : any idx -> any
  ;; Returns the ith element of a list 
  [(list-ref () idx) ,(error "list-ref: index not in range:" (term idx))]
  [(list-ref (any_0 any_1 ...) 0) any_0]
  [(list-ref (any_0 any_1 ...) idx) (list-ref (any_1 ...) ,(sub1 (term idx)))]) 

(define-metafunction mir-machine
  store-lookup : σ α -> v
  ;; Returns the value mapped to the address α in the store 
  [(store-lookup (store _ ... (α_0 v_0) _ ...) α_0) v_0]
  [(store-lookup σ α) ,(error "store-lookup: address not found in store:" (term α))])

(define-metafunction mir-machine
  store-update : σ δ x v -> σ
  ;; Updates the value mapped to this variable in the heap
  [(store-update (store (α_1 v_1) ... (α_0 v_old) (α_2 v_2) ...) δ x_0 v_new)
   (store (α_1 v_1) ... (α_0 v_new) (α_2 v_2) ...)
   (where frame (list-ref δ 1))
   (where α_0 (frm-lookup frame x_0))]
  [(store-update σ δ x v) ,(error "store-update: address not found in store:" (term x))])

(define-metafunction mir-machine
  store-update-direct : σ α v -> σ
  ;; Updates the value at the address in the store
  [(store-update-direct (store (α_1 v_1) ... (α_0 v_old) (α_2 v_2) ...) α_0 v_new)
   (store (α_1 v_1) ... (α_0 v_new) (α_2 v_2) ...)]
  [(store-update-direct σ α v) ,(error "store-update-direct: address not found in store:" (term α))])

(define-metafunction mir-machine
  store-update-aggregate : σ δ x (v ...) -> σ
  ;; Updates the aggregate value at the address in the store
  [(store-update-aggregate σ δ x_0 (v_1 ...))
   (store-update-aggregate-helper σ (α_1 ...) (v_1 ...))
   (where frame (list-ref δ 1))
   (where α_0 (frm-lookup frame x_0))
   (where (α_1 ...) (store-lookup σ α_0))])

(define-metafunction mir-machine
  store-update-aggregate-helper : σ (α ...) (v ...) -> σ
  ;; Updates the aggregate value that has inner values at the given addresses
  [(store-update-aggregate-helper σ () ()) σ]
  [(store-update-aggregate-helper σ (α_1 α_2 ...) (v_1 v_2 ...))
   (store-update-aggregate-helper σ_new (α_2 ...) (v_2 ...))
   (where σ_new (store-update-direct σ α_1 v_1))]
  [(store-update-aggregate-helper σ (α ...) (v ...)) ,(error "store-update-aggregate-helper: addresses and values don't match")])

(define-metafunction mir-machine
  frm-lookup : frame x -> α
  ;; Returns the address mapped to the variable x in the frame 
  [(frm-lookup (frm _ ... (x_0 α_0) _ ...) x_0) α_0]
  [(frm-lookup frame x) ,(error "frm-lookup: variable not found in frame:" (term x))])

(define-metafunction mir-machine
  env-lookup : ρ x -> α
  ;; Returns the address mapped to the variable x in the env 
  [(env-lookup (env _ ... (x_0 α_0) _ ...) x_0) α_0]
  [(env-lookup ρ x) ,(error "env-lookup: variable not found in environment:" (term x))])

(define-metafunction mir-machine
  alloc-vars-in-fn : fn σ δ -> (σ δ)
  ;; Create a stack frame, allocate space in the frame and heap for all variables in the function and a return value
  [(alloc-vars-in-fn fn σ (stk frame ...)) (σ_withreturn (stk frame_withreturn frame ...))
                                           (where (σ_new frame_new) (alloc-vars-in-fn-helper fn σ (frm)))
                                           (where (σ_withreturn frame_withreturn) (alloc-return-value σ_new frame_new))])

(define-metafunction mir-machine
  alloc-return-value : σ frame -> (σ frame)
  ;; Allocates a space for the return value in the frame and heap
  [(alloc-return-value σ frame) (alloc-var return-ptr unit σ frame)])

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
   (where (σ_new frame_new) (alloc-var lv_0 rv_0 σ frame))])

;; This function assumes we never have projections/derefs on the left
;; side of the assignment (which I haven't seen yet and seems to make
;; sense). 
(define-metafunction mir-machine
  alloc-var : lv rv σ frame -> (σ frame)
  ;; Allocate the necessary space for this variable if necessary
  [(alloc-var x_0 rv_0 σ (frm (x_1 α_1) ... (x_0 α_0) (x_2 α_2) ...)) ;; already been allocated
   (σ (frm (x_1 α_1) ... (x_0 α_0) (x_2 α_2) ...))]
  [(alloc-var x_base (operand ...) σ (frm (x α) ...)) ;; allocate enough space for aggregate value
   (σ_newer (frm (x_base α_base) (x α) ...))           ;; add the base pointer to the frame
   (where (σ_new (α_base α_1 ...)) (malloc σ ,(+ 1 (term (list-length (operand ...)))))) ;; count the first item, plus a pointer for the entire value
   (where σ_newer (store-update-direct σ_new α_base (α_1 ...)))] ;; store pointers to memory locations of inner contents
  ;; TODO: handle allocation for structs
  [(alloc-var x_0 rv_0 σ (frm (x α) ...)) ;; allocate one space for a single value 
   (σ_new (frm (x_0 α_new) (x α) ...))
   (where (σ_new (α_new)) (malloc σ 1))])

(define-metafunction mir-machine
  malloc : σ n -> (σ (α ...))
  ;; Allocate n spaces in the heap and return the new addresses
  [(malloc σ n) (malloc-helper σ n ())])

(define-metafunction mir-machine
  malloc-helper : σ n (α ...) -> (σ (α ...))
  ;; Allocate n spaces in the heap, adding each address to the accumulator
  [(malloc-helper σ 0 (α ...)) (σ (α ...))]
  [(malloc-helper (store (α v) ...) n (α_0 ...))
   (malloc-helper (store (α v) ... (α_new void)) ,(sub1 (term n)) (α_0 ... α_new))
   (where α_new (ptr ,(gensym)))])

(define-metafunction mir-machine
  is-allocated : x σ frame -> boolean
  ;; Returns true if x has been allocated in the heap and stack frame
  [(is-allocated x_0
                 (store _ ... (α_0 v_0) _ ...) 
                 (frm _ ... (x_0 α_0) _ ...))
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
  [(lookup-fn (_ ... (g_0 (x_0 ...) bbs_0 idx_0) _ ...) g_0)
   (g_0 (x_0 ...) bbs_0 idx_0)]
  [(lookup-fn prog g) ,(error "lookup-fn: function with name not found:" (term g))])

(define-metafunction mir-machine
  get-return-value : δ σ -> v
  ;; Produce the value mapped to return-ptr (return pointer)
  [(get-return-value δ σ)
   (store-lookup σ (frm-lookup (get-current-frame δ) return-ptr))])

(define-metafunction mir-machine
  lookup-next-bb-idx : (switchInt const (const idx) ... (otherwise idx)) -> idx
  ;; Produce the index of the next basic block, matching on the operand 
  [(lookup-next-bb-idx (switchInt const (otherwise idx))) idx]
  [(lookup-next-bb-idx (switchInt const_0 (const_1 idx_1) (const_2 idx_2) ... (otherwise idx)))
   idx_1
   (side-condition (equal? (term const_0) (term const_1)))]
  [(lookup-next-bb-idx (switchInt const_0 (const_1 idx_1) (const_2 idx_2) ... (otherwise idx)))
   (lookup-next-bb-idx (switchInt const_0 (const_2 idx_2) ... (otherwise idx)))])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Metafunctions for testing
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-metafunction mir-machine
  size : (any ...) -> integer
  [(size (store any_0 ...)) (list-length (any_0 ...))]
  [(size (env any_0 ...)) (list-length (any_0 ...))]
  [(size (frm any_0 ...)) (list-length (any_0 ...))]
  [(size (stk any_0 ...)) (list-length (any_0 ...))])

(define-metafunction mir-machine
  list-length : (any ...) -> integer
  ;; Returns the number of elements in the list
  [(list-length ()) 0]
  [(list-length (any_0 any_1 ...)) ,(add1 (term (list-length (any_1 ...))))])

(define-metafunction mir-machine
  get-stack : (prog any σ ρ δ) -> δ
  ;; Get the stack from the result of a reduction 
  [(get-stack (prog any σ ρ δ)) δ])

(define-metafunction mir-machine
  get-store : (prog any σ ρ δ) -> σ
  ;; Get the store from the result of a reduction
  [(get-store (prog any σ ρ δ)) σ])

(define-metafunction mir-machine
  get-frame-contents : frame -> ([x α] ...)
  ;; Get the mappings in the frame
  [(get-frame-contents (frm [x α] ...)) ([x α] ...)])

(define-metafunction mir-machine
  get-current-frame : δ -> frame
  ;; Get the first frame in the stack
  [(get-current-frame (stk frame_0 frame_1 ...)) frame_0])

(define-metafunction mir-machine
  get-bbs : fn -> bbs
  ;; Get the basic blocks in this function
  [(get-bbs (g (x_!_ ...) bbs idx)) bbs])

(define-metafunction mir-machine
  get-return-value-from-reduction : (prog any σ ρ δ) -> any
  ;; Get the return value from the result of a reduction
  [(get-return-value-from-reduction (prog any σ ρ δ)) any])

(define-metafunction mir-machine
  next-bb : fn idx -> blk
  ;; Get the basic block inside the current function with the specified index 
  [(next-bb fn idx_next) (lookup-bb (get-bbs fn) idx_next)])