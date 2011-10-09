;; -------------------------------------------------------------------------
;; Name: Jonathan Neufeld
;; Student ID: 30671093
;; Favorite Prog Lang Keyword: KTHXBAI from LOLCODE ;)
;;
;; COMMENTS:
;; Some code taken from PLAI text. If a function contains code from an
;; outside source its documentation will indicate where. Helper functions
;; and datatype definitions are at the top, major functions like parse and
;; interp in the middle and tests are at the bottom of the source. I handled
;; with* cases separately in my interp function. 
;; -------------------------------------------------------------------------
#lang plai

;; -------------------------------------------------------------------------
;; Datatype definitions and helper functions.
;; -------------------------------------------------------------------------
;; Binding of a WAE to a symbol for with and with* forms.
(define-type Binding
  [binding (name symbol?) (named-expr WAE?)])

;; Definition of WAE.
(define-type WAE
  [num (n number?)]
  [binop (op procedure?) (lhs WAE?) (rhs WAE?)]
  [with (b Binding?) (body WAE?)]
  [with* (lob (listof Binding?)) (body WAE?)]
  [id (name symbol?)])

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

;; is-with* : symbol -> boolean
;; Consumes a symbol from parse and returns true if the symbol indicates
;; the sexp in parse is a with* WAE.
(define (is-with* sym)
  (if (symbol=? sym 'with*) true false))

;; -------------------------------------------------------------------------
;; Main functions.
;; -------------------------------------------------------------------------
;; parse : s-exp -> WAE
;; Consumes an s-expression and generates the corresponding WAE.
;; The idea here to is to translate the concrete syntax of our
;; language into the abstract syntax the interpreter understands.
(define (parse sexp)
  (cond
    ;; Numbers and symbols are trivial.
    [(number? sexp) (num sexp)]
    [(symbol? sexp) (id sexp)]
    ;; Lists can be a variety of datatypes, find out what it is.
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
       ;; Create with* by mapping make-binding and separating the 
       ;; binding elements in a lambda function. Also parse body.
       [(is-with* (first sexp))
        (make-with*
         ;; Empty binding lists are silly but we have to check for them.
         (if (empty? (second sexp))
             '()
             ;; Not an empty binding list. Make a list of bindings with
             ;; map. Make lists of binding ids and binding values from 
             ;; sexp with mapped lambda functions.
             (map make-binding
                  (map (lambda (ls) (first ls)) (second sexp))
                  (map (lambda (ls) (parse (second ls))) (second sexp))))
         (parse (third sexp)))] )] ))

;; subst : WAE symbol WAE -> WAE
;; Substitute second arg with third arg in first arg such that
;; the resulting WAE has no free instances of second arg. For the
;; most part, this code is from the PLAI text and has been slightly
;; modified to suit my needs.
(define (subst expr sub-id val)
  (type-case WAE expr
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
    [with* (lob body) '()]
    [id (v) (if (symbol=? v sub-id) val expr)] ))

;; interp : WAE -> number
;; Consumes a WAE representation of an expression and computes
;; the corresponding numerical result.
(define (interp expr)
  (type-case WAE expr
    [num (n) n]
    [binop (o l r) (o (interp l) (interp r))]
    [with (binds body)
          (interp (subst body
                         (binding-name binds)
                         (num (interp (binding-named-expr binds)))))]
    [with* (lob body) body]
    [id (v) v] ))

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

;; is-with* test
(test (is-with* (first '{with* {{x 5}} x})) true)
(test (is-with* (first '{with x 5})) false)

;; Main function tests
;; parser tests
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

;; interpreter tests
(test (interp (num 1)) 1)
(test (interp (id 'x)) 'x)
(test (interp (binop + (num 1) (num 1))) 2)
(test (interp (binop + (binop - (num 10) (num 5))
                     (binop * (num -1) (num -5)))) 10)
(test (interp (num 100)) 100)
(test (interp (parse '{+ 1 2})) 3)
(test (interp (parse '{with {x 1} x})) 1)

;; full interp texts from plai
(test (interp (parse '{with {x {+ 5 5}} {+ x x}})) 20)
(test (interp (parse '{with {x 5} {+ x x}})) 10)
(test (interp (parse '{with {x {+ 5 5}} {with {y {- x 3}} {+ y y}}})) 14)
(test (interp (parse '{with {x 5} {with {y {- x 3}} {+ y y}}})) 4)
(test (interp (parse '{with {x 5} {+ x {with {x 3} 10}}})) 15)
(test (interp (parse '{with {x 5} {+ x {with {x 3} x}}})) 8)
(test (interp (parse '{with {x 5} {+ x {with {y 3} x}}})) 10)
(test (interp (parse '{with {x 5} {with {y x} y}})) 5)
(test (interp (parse '{with {x 5} {with {x x} x}})) 5)

;; with* tests
(test (parse '{with* {} 1})
      (with* '() (num 1)))
(test (parse '{with* {{x 1}} x})
      (with* (list (binding 'x (num 1))) (id 'x)))
(test (parse '{with* {{x 1} {y 1}} {+ x y}}) 
      (with* (list (binding 'x (num 1)) 
                   (binding 'y (num 1))) 
             (binop + (id 'x) (id 'y))))
(test (parse '{with* {{x 1}
                      {x {+ x 1}}}
                     x})
      (with* (list (binding 'x (num 1))
                   (binding 'x (binop + (id 'x) (num 1))))
             (id 'x)))
(test (parse '{with* { {x 1} {y x} } {+ x y}})
      (with* (list (binding 'x (num 1)) (binding 'y (num 1))) (binop + (id 'x) (id 'y))))