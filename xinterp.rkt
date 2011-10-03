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
  [binop (op procedure?) 
         (lhs CFWAE?) 
         (rhs CFWAE?)]
  [with (b Binding?) 
        (body CFWAE?)]
  [id (name symbol?)]
  [if0 (c CFWAE?) 
       (t CFWAE?) 
       (e CFWAE?)]
  [fun (args (listof symbol?)) 
       (body CFWAE?)]
  [app (f CFWAE?) 
       (args (listof CFWAE?))])

;; Definition of an environment. It's either empty, or not :).
(define-type Env
  [mtEnv]
  [anEnv (name symbol?) 
         (value CFWAE-Value?) 
         (env Env?)])

;; A CFWAE-Value.
(define-type CFWAE-Value
  [numV (n number?)]
  [thunkV (body CFWAE?) 
          (env Env?)]
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
  (if (symbol? sym)
      (if (symbol=? sym 'with) true false)
      false))

;; is-if0 : symbol -> boolean
;; Consumes a symbol from parse and returns true if the symbol indicates
;; the sexp in parse is an if0.
(define (is-if0 sym)
  (if (symbol? sym)
      (if (symbol=? sym 'if0) true false)
      false))

;; is-fun : symbol -> boolean
;; Consumes a symbol from parse and returns true if the symbol indicates
;; the sexp in parse is a fun.
(define (is-fun sym)
  (if (symbol? sym)
      (if (symbol=? sym 'fun) true false)
      false))

;; is-app : symbol -> boolean
;; Consumes a symbol from parse and returns true if the symbol indicates
;; the sexp in parse is an app.
(define (is-app sym)
  (if (symbol? sym)
      (if (symbol=? sym 'app) true false)
      false))

;; lookup-env : Env symbol -> WAE-Value
;; looks up the symbol in the given environment, returning its value
;; ERROR if the symbol is undefined.
;; NOTE: where the symbol is bound multiple times, returns the value
;; of the binding that is outermost in the Env object.
;; NOTE: This code was given in lecture notes, I did not write it
(define (lookup-env env target-name)
  (type-case Env env
    [mtEnv () (error 'lookup-env "Unbound identifier ~a" target-name)]
    [anEnv (name value restEnv)
           (if (symbol=? target-name name)
               value
               (lookup-env restEnv target-name))]))

;; -------------------------------------------------------------------------
;; Main functions.
;; -------------------------------------------------------------------------
;; parse : expression -> CFWAE
;; This procedure parses an expression into a CFWAE
(define (parse sexp)
  (cond
    ;; Numbers are trivial.
    [(number? sexp) (num sexp)]
    
    ;; A ist can be a variety of things, find out what it is.
    [(list? sexp)
     (cond
       ;; Create binop by grabbing correct procedure from binops
       ;; and parsing the operands.
       [(is-binop (first sexp))
        (if (not (= (length sexp) 3))
            (error 'parse "binop expects 2 args")
            (local ([define opr (get-procedure (first sexp))]
                    [define lhs (second sexp)]
                    [define rhs (third sexp)])
              (make-binop opr (parse lhs) (parse rhs)) ))]
       
       ;; Create with by making binding and parsing body.
       [(is-with (first sexp))
        (if (not (= (length sexp) 3))
            (error 'parse "with expects 2 args")
            (local ([define id         (first (second sexp))]
                    [define named-expr (second (second sexp))]
                    [define body       (third sexp)])
              (with (make-binding id (parse named-expr))
                    (parse body)) ))]
       
       ;; Create if0 by parsing each element of the expression
       [(is-if0 (first sexp))
        (if (not (= (length sexp) 4))
            (error 'parse "if0 expects 3 args")
            (local 
              ([define c (parse (second sexp))]
               [define t (parse (third sexp))]
               [define e (parse (fourth sexp))])
              (make-if0 c t e)) )]
       
       ;; Create function definition by parsing arguments and body.
       [(is-fun (first sexp))
        (if (not (= (length sexp) 3))
            (error 'parse "fun expects 2 args")
            (local ([define args (second sexp)]
                    [define body (third sexp)])
              (make-fun args (parse body))))]
       
       
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
    ;; Clean out the cases of if0.
    [(if0? expr)
     (local ([define c (if0-c expr)]
             [define t (if0-t expr)]
             [define e (if0-e expr)])
       (if0 (pre-process c)
            (pre-process t)
            (pre-process e)))]
    
    ;; Make sure to pre-process a binops left and right operands.
    [(binop? expr)
     (local ([define o (binop-op expr)]
             (define l (binop-lhs expr))
             (define r (binop-rhs expr)))
       (binop o (pre-process l) (pre-process r)))]
    
    ;; Remove with from AST by constructing an equivilant function
    ;; application.
    [(with? expr)
     (local ([define f-arg    (pre-process (binding-name (with-b expr)))]
             [define f-body   (pre-process (with-body expr))]
             [define app-args (pre-process (binding-named-expr (with-b expr)))])
       (pre-process (make-app (make-fun (list f-arg) f-body) 
                              (list app-args))))]
    
    ;; Functions of more than one argument are transformed into
    ;; nested functions. Some sort of currying thingy.
    [(fun? expr)
     (if (> (length (fun-args expr)) 1)
         ;; Function has more than one argument, transform it:
         (local ([define first-arg  (pre-process (first (fun-args expr)))]
                 [define other-args (pre-process (rest (fun-args expr)))]
                 [define body       (pre-process (fun-body expr))])
           (pre-process (make-fun (list first-arg)
                                  (pre-process (make-fun other-args
                                                         body)))))
         ;; Just one (or zero) arguments, leave it as is:
         expr)]
    
    ;; Function applications of more then one argument are transformed
    ;; into nested applications of a single argument, like functions.
    [(app? expr)
     (if (> (length (app-args expr)) 1)
         (local ([define first-arg  (pre-process (first (app-args expr)))]
                 [define other-args (pre-process (rest (app-args expr)))]
                 [define f          (pre-process (app-f expr))])
           (pre-process (make-app (make-app f (list first-arg))
                                  other-args)))
         expr)]
    
    ;; pre-process only checks with, fun and app. Otherwise we don't
    ;; preprocess it.
    [else expr] ))

;; interp : CFWAE -> CFWAE-Value
;; This procedure interprets the given CFWAE and produces a result 
;; in the form of a CFWAE-Value (either a closureV, thunkV, or numV).
;; (Assumes the input was successfully produced by pre-process.)
(define (interp expr)
  (interp-env (mtEnv) expr))

;; interp-env : Env CFWAE -> CFWAE-Value
;; Takes an environment and an abstract syntax tree and computes the
;; corresponding value within the given environment.
(define (interp-env env expr)
  ;; If expr is a CFWAE-Value we're at the end of some brach in the AST
  ;; otherwise we need to do more work.
  (if (CFWAE-Value? expr)
      ;; If it's a CFWAE-Value, just give it back.
      expr
      
      ;; Continue interping expr in the given environment.
      (type-case CFWAE expr
        ;; Give back numbers as numVs.
        [num (n) (numV n)]
        
        ;; Binops result to numVs.
        [binop (o l r) 
               (local ([define lhs (numV-n (interp-env env l))]
                       [define rhs (numV-n (interp-env env r))])
                 (numV (o lhs rhs)) )]
        
        ;; The AST given to interp should never contain a with.
        [with (binds body)
              (error 'interp-env "encountered 'with'")]
        
        ;; Look up any identifier in the current environment.
        [id (name) (lookup-env env name)]
        
        ;; TODO: interp if0
        [if0 (c t e)
             (local ([define c-result (interp-env env c)])
               (if (numV? c-result)
                   (if (= 0 (numV-n c-result))
                       (interp-env env t)
                       (interp-env env e))
                   (error 'interp-env "if0 result not numV") ))]
        
        ;; Create a closure of the function in the given environment.
        [fun (arg body)
             (if (empty? arg)
                 ;; No arguments to this function, make a thunk.
                 (thunkV body env)
                 ;; Function has argument, create a closure.
                 (closureV (first arg) body env))]
        
        [app (f-expr args)
             (local ([define fun-val (interp-env env f-expr)])
               (cond
                 
                 ;; Function application with arguments.
                 [(closureV? fun-val)
                  (local ([define val     (interp-env env (first args))]
                          [define fun-val (interp-env env f-expr)]
                          [define fun-par (closureV-param fun-val)]
                          [define fun-bod (closureV-body fun-val)])
                    (interp-env (anEnv fun-par val (closureV-env fun-val))
                                fun-bod))]
                 
                 ;; Function of zero arguments, just interp the body.
                 [(thunkV? fun-val)
                  (interp-env (thunkV-env fun-val)
                              (thunkV-body fun-val))]
                 
                 ;; Not a thunk or a closure, signal an error.
                 [else (error 'interp-env "app did not get closure or thunk")] ))] )))

;; run : sexp -> CFWAE-Value
;; Consumes an sexp and passes it through parsing, pre-processing,
;; and then interpretation to produce a result.
(define (run sexp)
  (interp (pre-process (parse sexp))))

;; failed-tests : -> (listof plai-test-result)
;; Generates a list of only the failed (non-good) tests from plai-all-test-results.
(define (failed-tests)
  (reverse (filter (compose not (curry symbol=? 'good) first) plai-all-test-results)))

;; -------------------------------------------------------------------------
;; Unit testing.
;; -------------------------------------------------------------------------
;; Helper function tests
"helper function tests"
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

;; lookup-env tests
(test/exn (lookup-env (mtEnv) 'x) "")
(test (lookup-env (anEnv 'x (numV 1) (mtEnv)) 'x) (numV 1))
(test/exn (lookup-env (anEnv 'y (numV 1) (mtEnv)) 'x) "")
(test (lookup-env (anEnv 'x (numV 1) (anEnv 'y (numV 2) (mtEnv))) 'x) (numV 1))
(test (lookup-env (anEnv 'x (numV 1) (anEnv 'y (numV 2) (mtEnv))) 'y) (numV 2))
(test (lookup-env (anEnv 'x (numV 1) (anEnv 'x (numV 2) (mtEnv))) 'x) (numV 1))

;; parser tests
"parse tests"
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
(test (parse '{fun {x} {+ 1 x}}) (fun '(x) 
                                      (binop + (num 1) (id 'x))))
(test (parse '{fun {x y z} {* z {+ x y}}}) 
      (fun '(x y z) (binop * (id 'z) (binop + (id 'x) (id 'y)))))
(test (parse '(x y)) (app (id 'x) (list (id 'y))))
(test (parse '(x y z j k)) (app (id 'x) 
                                (list (id 'y) (id 'z) (id 'j) (id 'k))))
(test (parse '{with {double {fun {x} {+ x x}}} {double 10}})
      (with (binding 'double (fun '(x) 
                                  (binop + (id 'x) (id 'x)))) 
            (app (id 'double) (list (num 10)))))
(test (parse '{with {target 20} {with {inc-by-target 
                                       {fun {x} {+ x target}}} {inc-by-target 10}}})
      (with (binding 'target (num 20)) 
            (with (binding 'inc-by-target (fun '(x) 
                                               (binop + (id 'x) (id 'target))))
                  (app (id 'inc-by-target) (list (num 10))))))

(test/exn (parse '{+ 1}) "")
(test/exn (parse '{+ 5 1 x}) "")
(test/exn (parse '{with {x 5} {+ 5 x} {+ 5 x}}) "")
(test/exn (parse '{with {x 5}}) "")


;; pre-process tests
"pre-process tests"
(test (pre-process (parse '(x y z j k)))
      (app (app (app (app (id 'x) (list (id 'y))) 
                     (list (id 'z))) (list (id 'j))) (list (id 'k))))
(test (pre-process (parse '{fun {x} {+ 1 x}})) 
      (fun '(x) (binop + (num 1) (id 'x))))
(test (pre-process (parse '{if0 0 x y})) 
      (if0 (num 0) (id 'x) (id 'y)))
(test (pre-process (parse '{with {x 1} x})) 
      (app (fun '(x) (id 'x)) (list (num 1))))
(test (pre-process (parse '{with {x 2} {* 2 x}}))
      (app (fun '(x) (binop * (num 2) (id 'x))) (list (num 2))))
(test (pre-process (parse '{fun {x y} {+ x y}})) 
      (fun '(x) (fun '(y) (binop + (id 'x) (id 'y)))))
(test (pre-process (parse '(fun (x y z) (+ x (* y z)))))
      (fun '(x) (fun '(y) (fun '(z) (binop + (id 'x) (binop * (id 'y) (id 'z)))))))
(test (pre-process (parse '{with {target 20} 
                                 {with {inc-by-target {fun {x} {+ x target}}} {inc-by-target 10}}}))
      (app (fun '(target)
                (app (fun '(inc-by-target)
                          (app (id 'inc-by-target) 
                               (list (num 10)))) (list (fun '(x) (binop + (id 'x) (id 'target))))))
           (list (num 20))))


;; full interp texts from plai
"interp with from PLAI"
(test (run '{with {x 5} {+ x x}}) (numV 10))
(test (run '{with {x {+ 5 5}} {+ x x}}) (numV 20))
(test (run '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}}) (numV 14))
(test (run '{with {x 5} {with {y {- x 3}} {+ y y}}}) (numV 4))
(test (run '{with {x 5} {+ x {with {x 3} 10}}}) (numV 15))
(test (run '{with {x 5} {+ x {with {x 3} x}}}) (numV 8))
(test (run '{with {x 5} {+ x {with {y 3} x}}}) (numV 10))
(test (run '{with {x 5} {with {y x} y}}) (numV 5))
(test (run '{with {x 5} {with {x x} x}}) (numV 5))

;; interpreter/run tests
"interp/run tests"
(test (run '1) (numV 1))
(test (run '{+ 1 1}) (numV 2))
(test (run '{+ {* 10 2} {/ 20 2}}) (numV 30))
(test (run '{with {x 1} x}) (numV 1))
(test (run '{with {double {fun {x} {+ x x}}}
                  {double 10}}) (numV 20))
(test (run '{if0 0 1 2}) (numV 1))
(test (run '{if0 {+ 5 1} {/ 1 0} {with {x 3} {* x x}}})
      (numV 9))
(test (run '{with {x 1} 2}) (numV 2))
(test (run '{with {x {+ 1 2}} 2}) (numV 2))
(test (run '{with {x 1} {with {x 2} x}}) (numV 2))
(test (run '{with {x 1} {with {y 2} x}}) (numV 1))
(test (run '{with {x 1} {with {x {+ x 1}} x}}) (numV 2))
(test (run '{with {x 1} {with {y {+ x 1}} {+ x y}}}) (numV 3))

(test/exn (run '{1 2}) "")
(test (run '(with {myfun {fun () 400}} {myfun})) (numV 400))
(test (run '{with {target 5}
                  {with {inc-by-target {fun {x} {+ x target}}}
                        {inc-by-target 6}}}) (numV 11))
(test (run '{with {inc-by-target 
                   {with {target 25} {fun {x} {+ x target}}}}
                  {inc-by-target 2}}) (numV 27))
(test/exn (run '{with {f {fun {x} y}}
                      {with {y 10}
                            {f 0}}}) "")
(test (run '{with {target 3}
                  {with {inc-by-target {fun {x} {+ x target}}}
                        {with {target 10000}
                              {inc-by-target 3}}}}) (numV 6))
(test/exn (run '{with {taarget 20}
                      {with {inc-by-target {fun {x} {+ x target}}}
                            {inc-by-target 10}}}) "")
(test/exn (run '{with {taarget 20}
                      {with {inc-by-target {fun {x} {+ x target}}}
                            {with {target 20}
                                  {inc-by-target 10}}}}) "")
(test (run '{if0 {if0 {with {x 5} {- x 5}} 0 {+ 20 25}} 1 2})
      (numV 1))
(test (run '{if0 {with {inc-by-target 
                        {with {target 25} {fun {x} {+ x target}}}}
                       {inc-by-target 2}} 
                 {with {x 1} {with {y {+ x 1}} {+ x y}}} 
                 {with {target 3}
                       {with {inc-by-target {fun {x} {+ x target}}}
                             {with {target 10000}
                                   {inc-by-target 3}}}}})
      (numV 6))
(test (run '{if0 {with {inc-by-target 
                        {with {target -2} {fun {x} {+ x target}}}}
                       {inc-by-target 2}} 
                 {with {x 1} {with {y {+ x 1}} {+ x y}}} 
                 {with {target 3}
                       {with {inc-by-target {fun {x} {+ x target}}}
                             {with {target 10000}
                                   {inc-by-target 3}}}}})
      (numV 3))

;; print out failed tests explicity
"tests failed:"
(failed-tests)