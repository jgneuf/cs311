;; -------------------------------------------------------------------------
;; Name: Jonathan Neufeld
;; Student ID: 30671093
;; -------------------------------------------------------------------------
#lang plai

;; -------------------------------------------------------------------------
;; Datatype definitions and helper functions.
;; -------------------------------------------------------------------------
;; Definition of a binding.
(define-type Binding
  [binding (name symbol?) (named-expr CFWAE?)])

;; BNF of CFWAE.
(define-type CFWAE
  [num (n number?)]
  [binop (op procedure?) (lhs CFWAE?) (rhs CFWAE?)]
  [with (b Binding?) (body CFWAE?)]
  [id (name symbol?)]
  [if0 (c CFWAE?) (t CFWAE?) (e CFWAE?)]
  [fun (args (listof symbol?)) (body CFWAE?)]
  [app (f CFWAE?) (args (listof CFWAE?))])

;; Definition of an environment. It's either empty, or not :).
(define-type Env
  [mtEnv]
  [anEnv (name symbol?) (value CFWAE-Value?) (env Env?)])

;; A CFWAE-Value.
(define-type CFWAE-Value
  [numV (n number?)]
  [thunkV (body CFWAE?) (env Env?)]
  [closureV (param symbol?)
            (body CFWAE?)
            (env Env?)])

;; Define a list of supported binary operators.
(define binops (list '+ + '- - '* * '/ /))

;; get-procedure : symbol -> procedure
;; Use the helper function to return a procedure matching the given operator.
(define (get-procedure operator)
  (get-procedure-helper operator binops))

;; get-procedure-helper : symbol (listof binop) -> procedure
;; Consumes a symbol (operator) and the binop list and returns the
;; procedure associated with the given operator.
(define (get-procedure-helper op bo)
  (if (symbol=? op (first bo)) 
      (second bo)
      (get-procedure-helper op (rest (rest bo))) ))

;; is-binop : symbol -> boolean
;; Consume a symbol and return true if it's a (supported) binary operator.
;; Could potentially cause errors if an actual procedure is passed.
(define (is-binop sym)
  (if (member sym binops) true false))

;; is-with : symbol -> boolean
;; Consumes a symbol from parse and returns true if the symbol indicates
;; the sexp in parse is a with.
(define (is-with sym)
  (if (symbol=? sym 'with) true false))

;; is-if0 : symbol -> boolean
;; Consumes a symbol from parse and returns true if the symbol indicates
;; the sexp in parse is an if0.
(define (is-if0 sym)
  (if (symbol=? sym 'if0) true false))

;; is-fun : symbol -> boolean
;; Consumes a symbol from parse and returns true if the symbol indicates
;; the sexp in parse is a fun.
(define (is-fun sym)
  (if (symbol=? sym 'fun) true false))

;; is-app : symbol -> boolean
;; Consumes a symbol from parse and returns true if the symbol indicates
;; the sexp in parse is an app.
(define (is-app sym)
  (if (symbol=? sym 'app) true false))

;; -------------------------------------------------------------------------
;; Main functions.
;; -------------------------------------------------------------------------
;; parse : expression -> CFWAE
;; This procedure parses an expression into a CFWAE
(define (parse sexp)
  (cond
    ;; Numbers and symbols are trivial.
    [(number? sexp) (num sexp)]
    
    ;; A ist can be a variety of things, find out what it is.
    [(list? sexp)
     (cond
       ;; Create binop by grabbing correct procedure from binops
       ;; and parsing the operands.
       [(is-binop (first sexp)) 
        (make-binop (get-procedure (first sexp))
                    (parse (second sexp))
                    (parse (third sexp)))]
       
       ;; Create with by making binding and parsing body.
       [(is-with (first sexp))
        (make-with (make-binding (first (second sexp))
                                 (parse (second (second sexp))))
                   (parse (third sexp)))] 
       
       ;; Create if0 by parsing each element of the expression
       [(is-if0 (first sexp))
        (local 
          ([define c (parse (second sexp))]
           [define t (parse (third sexp))]
           [define e (parse (fourth sexp))])
          (make-if0 c t e)) ]
       
       ;; Create function definition by parsing arguments and body.
       [(is-fun (first sexp))
        (make-fun
         (second sexp)
         (parse (third sexp)))]
       
       ;; Must be a function application. Parse function and arguments.
       [else (local ([define f (parse (first sexp))]
                     [define args (map parse (rest sexp))])
               (make-app f args))] )]
    
    
    ;; If it's nothing else, it's an id.
    [else (id sexp)] ))

;; pre-process : CFWAE -> CFWAE
;; Consumes a CFWAE and constructs a corresponding CFWAE without
;; with expressions (which are replaced by function application) 
;; and with no functions or applications of more than one argument.
;; (Assumes the input was successfully produced by parse.)
(define (pre-process expr)
  (cond
    ;; Remove with from AST by constructing an equivilant function
    ;; application.
    [(with? expr)
     (local ([define f-arg (binding-name (with-b expr))]
             [define f-body (with-body expr)]
             [define app-args (binding-named-expr (with-b expr))])
       (make-app (make-fun (list f-arg) f-body) (list app-args)))]
    
    ;; Functions of more than one argument are transformed into
    ;; nested functions. Some sort of currying thingy.
    [(fun? expr)
     (if (> (length (fun-args expr)) 1)
         (make-fun (list (first (fun-args expr)))
                   (pre-process
                    (make-fun (rest (fun-args expr))
                              (fun-body expr))))
         expr)]
    [else expr] ))

;; subst : WAE symbol WAE -> WAE
;; Substitute second arg with third arg in first arg such that
;; the resulting WAE has no free instances of second arg. For the
;; most part, this code is from the PLAI text and has been slightly
;; modified to suit my needs.
(define (subst expr sub-id val)
  (type-case CFWAE expr
    [num (n) expr]
    
    ;; Retain abstract syntax for binops while substituting in WAEs.
    [binop (o l r)
           (make-binop
            o
            (subst l sub-id val)
            (subst r sub-id val))]
    
    ;; Substitution algorithm given in PLAI text. Implementation
    ;; specific details have been altered for my own use.
    [with (binds body)
          (if (symbol=? (binding-name binds) sub-id)
              (make-with (make-binding (binding-name binds)
                                       (subst (binding-named-expr binds)
                                              sub-id
                                              val))
                         body)
              (make-with (make-binding (binding-name binds)
                                       (subst (binding-named-expr binds)
                                              sub-id
                                              val))
                         (subst body sub-id val)) )]
    
    ;; Return correct binding value.
    [id (v) (if (symbol=? v sub-id) val expr)]
    
    ;; TODO: subst f0
    [if0 (c t e) '...]
    
    ;; TODO: subst fun
    [fun (args body) '...]
    
    ;; TODO: subst app
    [app (f args) '...] ))

;; interp : CFWAE -> CFWAE-Value
;; This procedure interprets the given CFWAE and produces a result 
;; in the form of a CFWAE-Value (either a closureV, thunkV, or numV).
;; (Assumes the input was successfully produced by pre-process.)
(define (interp expr)
  (type-case CFWAE expr
    [num (n) n]
    [binop (o l r) (o (interp l) (interp r))]
    [with (binds body)
          (interp (subst body
                         (binding-name binds)
                         (num (interp (binding-named-expr binds)))))]
    [id (v) v]
    
    ;; TODO: interp if0
    [if0 (c t e) '...]
    
    ;; TODO: interp fun
    [fun (args body) '...]
    
    ;; TODO: interp app
    [app (f args) '...] ))

;; run : sexp -> CFWAE-Value
;; Consumes an sexp and passes it through parsing, pre-processing,
;; and then interpretation to produce a result.
(define (run sexp)
  (interp (pre-process (parse sexp))))


;; Possibly useful additional functions:

;; failed-tests : -> (listof plai-test-result)
;; Generates a list of only the failed (non-good) tests from plai-all-test-results.
(define (failed-tests)
  (reverse (filter (compose not (curry symbol=? 'good) first) plai-all-test-results)))

;; CFWAE-pre-fold : (CFWAE -> CFWAE) CFWAE -> CFWAE
;; Takes a function and applies it to each expression node in the 
;; given CFWAE.  Note that the function is applied pre-order; so
;; it is applied to a node before its sub-trees.  WARNING: if
;; your function generates a new node that itself needs to be
;; re-processed through the function, CFWAE-pre-fold will not do
;; so.  (It calls f on a node and then recurses into any sub-nodes
;; of whatever node f returns.  It does not reprocess the node 
;; itself.)
(define (CFWAE-pre-fold f expr)
  (local ([define (ffold expr)
            (type-case CFWAE (f expr)
              [num (n) (num n)]
              [binop (op lhs rhs) (binop op (ffold lhs) (ffold rhs))]
              [with (b body) (with (binding (binding-name b)
                                            (ffold (binding-named-expr b)))
                                   (ffold body))]
              [id (name) (id name)]
              [if0 (c t e) (if0 (ffold c) (ffold t) (ffold e))]
              [fun (args body) (fun args (ffold body))]
              [app (f args) (app (ffold f) (map ffold args))])])
    (ffold expr)))

;; Example: 
;; swap-op-args : CFWAE -> CFWAE
;; Consumes a program and generates the corresponding program in which
;; each instance of a binop has had its lhs and rhs swapped.
;(define (swap-op-args program)
;  (CFWAE-pre-fold (lambda (exp)
;                    (type-case CFWAE exp
;                               [binop (op lhs rhs) (binop op rhs lhs)]
;                               [else exp]))
;                  program))
;
;(test (swap-op-args (parse '{+ 1 2})) (parse '{+ 2 1}))
;(test (swap-op-args (parse '{+ 3 {- {* 1 2} {/ 3 4}}}))
;      (parse '{+ {- {/ 4 3} {* 2 1}} 3}))
;(test (swap-op-args (parse '{fun {x} {+ x {if0 0 {+ 1 2} 3}}}))
;      (parse '{fun {x} {+ {if0 0 {+ 2 1} 3} x}}))


;; -------------------------------------------------------------------------
;; Unit testing.
;; -------------------------------------------------------------------------
;; Helper function tests
;; is-binop tests
(test (is-binop '+) true)
(test (is-binop '/) true)
(test (is-binop '^) false)

;; get-procedure tests
(test (get-procedure '+) +)
(test (get-procedure '*) *)

;; is-with tests
(test (is-with (first '(with x 5))) true)
(test (is-with 'binop) false)

;; is-if0 tests
(test (is-if0 (first '(if0 0 'x 'y))) true)
(test (is-if0 (first '(+ 5 1))) false)

;; is-fun tests
(test (is-fun (first '(fun {x} (+ x 1)))) true)
(test (is-fun (first '(with x 5))) false)

;; is-app tests
(test (is-app (first '(app (fun {x} (+ x 1)) {1}))) true)
(test (is-app (first '(if0 0 'x 'y))) false)

;; parser tests
"simple parse tests"
(test (parse '1) (num 1))
(test (parse 'x) (id 'x))
(test (parse '{+ 1 1}) (binop + (num 1) (num 1)))
(test (parse '{with {x 1} x}) (with (binding 'x (num 1)) (id 'x)))
(test (parse '{with {x 1} {with {x 2} x}}) 
      (with (binding 'x (num 1)) (with (binding 'x (num 2)) (id 'x))))
(test (parse '{with {x 10} {with {y 5} {* {+ x x} {/ x y}}}}) 
      (with
       (binding 'x (num 10))
       (with
        (binding 'y (num 5))
        (binop
         *
         (binop + (id 'x) (id 'x))
         (binop / (id 'x) (id 'y))))))
(test (parse '{if0 0 x y}) (if0 (num 0) (id 'x) (id 'y)))
(test (parse '(if0 (+ 2 0) true false))
      (if0 (binop + (num 2) (num 0)) (id 'true) (id 'false)))
(test (parse '(if0 (with (x 5) {* 0 x}) x (+ a b)))
      (if0 (with (binding 'x (num 5)) (binop * (num 0) (id 'x)))
           (id 'x)
           (binop + (id 'a) (id 'b))))
(test (parse '{fun {x} {+ 1 x}}) (fun '(x) (binop + (num 1) (id 'x))))
(test (parse '{fun {x y z} {* z {+ x y}}}) 
      (fun '(x y z) (binop * (id 'z) (binop + (id 'x) (id 'y)))))
(test (parse '(x y)) (app (id 'x) (list (id 'y))))
(test (parse '(x y z j k)) (app (id 'x) (list (id 'y) (id 'z) (id 'j) (id 'k))))

;; pre-process tests
"pre-process tests"
(test (pre-process (parse '(x y z j k))) (app (id 'x) (list (id 'y) (id 'z) (id 'j) (id 'k))))
(test (pre-process (parse '{fun {x} {+ 1 x}})) (fun '(x) (binop + (num 1) (id 'x))))
(test (pre-process (parse '{if0 0 x y})) (if0 (num 0) (id 'x) (id 'y)))
(test (pre-process (parse '{with {x 1} x})) (app (fun '(x) (id 'x)) (list (num 1))))
(test (pre-process (parse '{with {x 2} {* 2 x}}))
                    (app (fun '(x) (binop * (num 2) (id 'x))) (list (num 2))))
(test (pre-process (parse '{fun {x y} {+ x y}})) 
      (fun '(x) (fun '(y) (binop + (id 'x) (id 'y)))))
(test (pre-process (parse '(fun (x y z) (+ x (* y z)))))
      (fun '(x) (fun '(y) (fun '(z) (binop + (id 'x) (binop * (id 'y) (id 'z)))))))

;; interpreter tests
"simple interp tests"
(test (interp (num 1)) 1)
(test (interp (id 'x)) 'x)
(test (interp (binop + (num 1) (num 1))) 2)
(test (interp (binop + (binop - (num 10) (num 5))
                     (binop * (num -1) (num -5)))) 10)
(test (interp (num 100)) 100)
(test (interp (parse '{+ 1 2})) 3)
(test (interp (parse '{with {x 1} x})) 1)

;; full interp texts from plai
"interp with from PLAI"
(test (interp (parse '{with {x {+ 5 5}} {+ x x}})) 20)
(test (interp (parse '{with {x 5} {+ x x}})) 10)
(test (interp (parse '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}})) 14)
(test (interp (parse '{with {x 5} {with {y {- x 3}} {+ y y}}})) 4)
(test (interp (parse '{with {x 5} {+ x {with {x 3} 10}}})) 15)
(test (interp (parse '{with {x 5} {+ x {with {x 3} x}}})) 8)
(test (interp (parse '{with {x 5} {+ x {with {y 3} x}}})) 10)
(test (interp (parse '{with {x 5} {with {y x} y}})) 5)
(test (interp (parse '{with {x 5} {with {x x} x}})) 5)

;; print out failed tests explicity
"tests failed:"
(failed-tests)