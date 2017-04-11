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
      (· lv f))                         ;; projection (e.g. tuple access)
  
  ;; TODO: Consider combining tuples & structs & vectors into single "aggregate" value? 
  ;;        See http://manishearth.github.io/rust-internals-docs///rustc/mir/enum.Rvalue.html
  
  ;; rvalues
  (rvs (rv ...))
  (rv (use lv)                          ;; use lv  
      (& borrowkind lv)                 ;; references (borrows)
      const                             ;; constants
      (binop rv rv)                     ;; binary operations 
      (unop rv)                         ;; unary operations
      (cast castkind lv as ty)          ;; typecasts
      (vec ty len)                      ;; vectors 
      (rv ...)                          ;; aggregate values (i.e. tuples)
      (struct s sts))                   ;; structs, with name and body 
  
  ;; constants (can be evaluated at compile time)
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
      (& mq ty)                         ;; types with qualifiers 
      (box ty))                         ;; boxes 
  
  ;; error messages
  (msg string)
  
  ;; vector length ;; FIXME: is this capacity or length? 
  (len integer)
  
  (x variable-not-otherwise-mentioned)  ;; variables 
  (f number)                            ;; field names
  (g variable-not-otherwise-mentioned)  ;; function names
  (l variable-not-otherwise-mentioned)  ;; labels (i.e. block names)
  (s variable-not-otherwise-mentioned)) ;; struct names

;; =========================================================
;; Evaluation
;; =========================================================
(define-extended-language mir-machine mir
  ;; a state of the machine
  ;;    rv: a single rvalue ;;TODO: eventually work up to prog... 
  ;;    H: the heap
  ;;    V: the global variable table 
  (P (rv H V))
  ;; Heap, addresses mapped to their heap value
  (H ((α hv) ...))
  (hv (ptr α)                           ;; pointer
      number                            ;; numerical value
      void)                             ;; void (uninitialized)
  ;; Variable symbol table, vars mapped to their addresses in the heap 
  (V ((x α) ...))
  ;; Address (index of a value into the heap-list?)
  (α number)
  ;; Evaluation contexts
  (Cx hole
      ;; P
      (Cx H V)
      ;; rvalues 
      (use Cx)
      (& borrowkind Cx)
      (binop Cx rv)
      (binop const Cx)
      (unop Cx)
      (box Cx)
      (cast castkind Cx as ty)          
      (const ... Cx rv ...)))

(define run
  (reduction-relation
   mir-machine

   (--> (in-hole Cx ((use x_0) H V))
        (in-hole Cx (deref H V x_0))
        "use")
   (--> (in-hole Cx ((& borrowkind x_0) H V))
        (in-hole Cx (get-address V x_0))
        "ref")))

;; FIXME error handling? 
;; Returns the address mapped to this variable in the variable symbol table
(define-metafunction mir-machine
  get-address : V x -> α
  [(get-address V x) (get V x)])

;; FIXME error handling?
;; Returns the value mapped to this variable in the heap 
(define-metafunction mir-machine
  deref : H V x -> hv
  [(deref H V x) (get H α)
                 (where α (get-address V x))])

;; Allocates 1 block of memory on the heap, returns the new address
(define-metafunction mir-machine
  malloc : H -> (H α)
  [(malloc H_old) (H_new α_new)
                  (where ((α_old hv) ...) H_old)
                  (where α_new ,(add1 (apply max (term (-1 α_old ...)))))
                  (where H_new (extend H_old α_new))])

;; Extends the heap with the new address 
(define-metafunction mir-machine
  extend : H α -> H
  [(extend ((α_old hv) ...) α_new) ((α_new void) (α_old hv) ...)])

;; Returns the value to which the key is mapped in the list
;; #f if there is no mapping 
(define-metafunction mir-machine
  ;; Assumes key is unique in the list 
  get : ((any any) ...) any -> any or #f
  [(get ((any_1 any_v1) ... (any_0 any_v0) (any_2 any_v2) ...) any_0)
   any_v0]
  [(get ((any_1 any_v1) ...) any_0) #f])

;; Updates the value mapped to this variable in the heap
(define-metafunction mir-machine
  ;; Assumes this variable has already been initialized in V and H
  put : H V x hv -> H
  [(put ((α_1 hv_1) ... (α_0 hv_old) (α_2 hv_2) ...) V x_0 hv_new)
   ((α_1 hv_1) ... (α_0 hv_new) (α_2 hv_2) ...)
   (where α_0 (get-address V x_0))])

(define-extended-language mir-ops mir
  ;; Evaluation contexts for operations
  (Cx hole 
     (+ const Cx) (+ Cx rv)
     (- const Cx) (- Cx rv)
     (* const Cx) (* Cx rv)
     (/ const Cx) (/ Cx rv)
     (< const Cx) (< Cx rv)
     (> const Cx) (> Cx rv)
     (% const Cx) (% Cx rv)))

(define reduce
  (reduction-relation
   mir-ops
   ;; BinOp evaluation rules 
   ;; FIXME: how does Rust deal with NaNs? 
   (--> (in-hole Cx (+ const_1 const_2))
        (in-hole Cx ,(+ (term const_1) (term const_2)))
        "+")
   (--> (in-hole Cx (- const_1 const_2))
        (in-hole Cx ,(- (term const_1) (term const_2)))
        "-")
   (--> (in-hole Cx (* const_1 const_2))
        (in-hole Cx ,(* (term const_1) (term const_2)))
        "*")
   (--> (in-hole Cx (/ const_1 const_2))
        (in-hole Cx ,(/ (term const_1) (term const_2)))
        "/")
   (--> (in-hole Cx (< const_1 const_2))
        (in-hole Cx ,(< (term const_1) (term const_2)))
        "<")
   (--> (in-hole Cx (> const_1 const_2))
        (in-hole Cx ,(> (term const_1) (term const_2)))
        ">")
   (--> (in-hole Cx (% const_1 const_2))
        (in-hole Cx ,(remainder (term const_1) (term const_2)))
        "remainder")))
