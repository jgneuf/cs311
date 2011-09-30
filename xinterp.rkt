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
;; the sexp in parse is a with WAE.
(define (is-with sym)
  (if (symbol=? sym 'with) true false))

;; -------------------------------------------------------------------------
;; Main functions.
;; -------------------------------------------------------------------------
;; parse : expression -> CFWAE
;; This procedure parses an expression into a CFWAE
(define (parse sexp)
  (cond
    ;; Numbers and symbols are trivial.
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    
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
                   (parse (third sexp)))] )] ))

;; pre-process : CFWAE -> CFWAE
;; Consumes a CFWAE and constructs a corresponding CFWAE without
;; with expressions (which are replaced by function application) 
;; and with no functions or applications of more than one argument.
;; (Assumes the input was successfully produced by parse.)
(define (pre-process expr)
  '...)

;; interp : CFWAE -> CFWAE-Value
;; This procedure interprets the given CFWAE and produces a result 
;; in the form of a CFWAE-Value (either a closureV, thunkV, or numV).
;; (Assumes the input was successfully produced by pre-process.)
(define (interp expr)
  '...)

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


