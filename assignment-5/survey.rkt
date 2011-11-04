#lang plai
(require web-server/servlet
         web-server/servlet-env)

;; A closure represents the data a user enters (and the question that they're answering)
;; and potentially the current environment it's being evaluated in -- which is what we're
;; using for continuations.
(define-type Closure
  [closureS (question string?) (answer string?) (context Env?)])

;; An environment stores the data the user answered for a particular question as well as
;; a list of environments. This enables to store a list of questions, i.e. all the
;; questions the user has answered. The continuation aspect is stored in the closure
;; itself, which holds the current environment. That way if we go back to another question
;; we simply do a lookup to grab the closure and build on its environment.
(define-type Env
  [mtEnv]
  [env (data Closure?) (environment Env?)])

;; Begin the survey with question 1 and and empty environment.
(define (start req)
  (send-and-get 1 (mtEnv)))

;; Given a question key number, return the question string.
(define (lookup-question key)
  (cond 
    [(= key 1) "Are you planning to travel soon?"]
    [(= key 2) "What country are you planning to travel?"]
    [(= key 3) "Are you also interested in travelling to Italy?"]
    [(= key 4) "How many people are accompanying you?"]
    [else "Would you like to subscribe to our news letter?"]))

;; We start here with q = 1, which gets question 1.
(define (send-and-get q cenv)
  (local ((define question (lookup-question q))
          [define request (send/suspend (make-request-page question))]
          [define bindings (request-bindings request)]
          [define answer (extract-binding/single 'ans bindings)]
          [define closure (closureS question answer cenv)])
    (cond 
      [(= q 1) (if (equal? "Yes" answer)
                   (send-and-get 2 (env closure cenv))
                   (send-and-get 7 (env closure cenv)))]
      [else (send/suspend (make-result-page (env closure cenv)))])))

;; After question 7 print out the survey results contained in the given environment.
(define (make-result-page allEnvs)
  (lambda (k-url)
    (response/xexpr `(html
                      (body
                       "Survey Result:"
                       (br)
                       (map (lambda (x) x) ,(format-result allEnvs)))))))

;; Append isn't working right, we also tried cons but that gave back a list.
(define (format-result allEnvs)
  (if (mtEnv? allEnvs)
      '()
      (local ([define aClosure (env-data allEnvs)]
              [define restEnv (env-environment allEnvs)]
              [define question (closureS-question aClosure)]
              [define answer (closureS-answer aClosure)])
        (cons '(div 
                  ()
                  ,question (br) ,answer)
                (format-result restEnv)))))

;; Create a page that asks the user for their answer given a question.
(define (make-request-page question)
  (lambda (k-url)
    (response/xexpr `(html
                      (body
                       (form ((action ,k-url) (method "post"))
                             (h2 ,question)
                             (input ((type "text") (name "ans")))
                             (input ((type "submit") 
                                     (name "submit") 
                                     (value "Submit")))))))))


(serve/servlet start)