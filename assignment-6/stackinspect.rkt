#lang plai

;; Name 1: 
;; Student ID 1:

;; Name 2: 
;; Student ID 2:

(define-type KCFAE
  [num (n number?)]
  [add (lhs KCFAE?) (rhs KCFAE?)]
  [id (name symbol?)]
  [if0 (test KCFAE?) (truth KCFAE?) (falsity KCFAE?)]
  [fun (param symbol?) (body KCFAE?)]
  [app (fun-expr KCFAE?) (arg-expr KCFAE?)]
  [bless (body KCFAE?)]
  [check])

(define-type KCFAE-Value
  [numV (n number?)]
  [closureV (p procedure?)])

(define-type DefrdSub
  [mtSub]
  [aSub (name symbol?) (value KCFAE-Value?) (ds DefrdSub?)])

;; lookup : symbol DefrdSub -> KCFAE-Value
(define (lookup name ds)
  (type-case DefrdSub ds
             [mtSub () (error 'lookup "no binding for identifier")]
             [aSub (bound-name bound-value rest-ds)
                   (if (symbol=? bound-name name)
                       bound-value
                       (lookup name rest-ds))]))

;; num+ : KCFAE-Value KCFAE-Value -> KCFAE-Value
(define (num+ x y)
  (numV (+ (type-case KCFAE-Value x
                      [numV (n) n]
                      [else (error 'num+ "addition of a non-number")])
           (type-case KCFAE-Value y
                      [numV (n) n]
                      [else (error 'num+ "addition of a non-number")]))))
;; num-zero? : KCFAE-Value -> boolean
(define (num-zero? n)
  (type-case KCFAE-Value n
                [numV (n) (= n 0)]
                [else (error 'num-zero? "num-zero? argumnet must be a number")]))


;; interp : KCFAE Env receiver -> (doesn't return)
(define (interp expr env k)
  (type-case KCFAE expr
             [num (n) (k (numV n))]
             [add (l r) (interp l env
                                (lambda (lv)
                                  (interp r env
                                          (lambda (rv)
                                            (k (num+ lv rv))))))]
             [if0 (test truth falsity)
                  (interp test env
                          (lambda (tv)
                            (if (num-zero? tv)
                                (interp truth env k)
                                (interp falsity env k))))]
             [id (v) (k (lookup v env))]
             [fun (param body)
                  (k (closureV (lambda (arg-val dyn-k)
                                 (interp body (aSub param arg-val env) dyn-k))))]
             [app (fun-expr arg-expr)
                  (interp fun-expr env
                          (lambda (fun-val)
                            (interp arg-expr env
                                    (lambda (arg-val)
                                      (type-case KCFAE-Value fun-val
                                                 [closureV (c) (c arg-val k)]
                                                 [else (error "not an applicable value")])))))]
	     [else (error 'interp "incomplete interp!")]))

;; test-interp : KCFAE -> KCFAE-Value
(define (test-interp expr)
  (let/cc k (interp expr (mtSub) k)))
