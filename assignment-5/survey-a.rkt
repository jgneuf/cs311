#lang plai
(require web-server/servlet
         web-server/servlet-env)


(define promoCode (number->string (random 99999)))

;; When a user enters an answer to a question, we store the question and the user's iput in a
;; closure. Since they may come back to this page we also store the environment they entered
;; this information in. If they change their response, we simply continue the survey from the
;; environment they are working in at that time.
(define-type Closure
  [closureS (question string?) (answer string?) (context Env?)])

;; An environment stores the data the user answered for a particular question as well as
;; a list of environments. This enables us to store a list of questions, i.e. all the
;; questions the user has answered, and the context it was answered in. The continuation aspect
;;is stored in the closure itself, which holds the current environment. That way if we go back
;;to another question we simply do a lookup to grab the closure and build on its environment.
(define-type Env
  [mtEnv]
  [env (data Closure?) (environment Env?)])

;; Begin the survey with question 1 and and empty environment.
(define (start req)
  (send-and-get 1 (mtEnv)))

;; lookup-question : number -> string
;; Associate each question (the string) with a question number. When lookup-question is given
;; the question number it returns the associated string, i.e. the actual question.
(define (lookup-question key)
  (cond
    [(= key 1) "Are you planning to travel soon?"]
    [(= key 2) "What country are you planning to travel?"]
    [(= key 3) "Are you also interested in travelling to Italy?"]
    [(= key 4) "How many people are accompanying you?"]
    [(= key 5) "We offer 30% discount for those traveling with 4 or more people to Italy.
                Are you Interested?"]
    [(= key 6) "We offer Basic Saving Plan of 5% discount for people travelling in groups.
               This is your promote code:"]
    [(= key 7) "Would you like to subscribe to our news letter?"]
    [else (error 'lookup-question "Expect argument 1 through 7, got ~a" key)]))

;; lookup-answer : environment -> string/number
(define (lookup-answer environment question)
  (cond
    [(mtEnv? environment) (error 'lookup-answer "Question has not been answered: ~a" question)]
    [(equal? question (closureS-question (env-data environment)))
     (closureS-answer (env-data environment))]
    [else (lookup-answer (closureS-context (env-data environment)) question)] ))

;; send-and-get : number, environement ->
;; Take a number, which is the question to ask, and an environment. We get the question string
;; associated with the question number and create a page that asks the user the question via
;; make-request-page. The answer is parsed out of the bindings and we create a closure with it.
;; The closure stores the question, answer and current environment -- we can use this environment
;; as a continuation later if the user comes back to this question and enters new values for
;; later questions.
(define (send-and-get q cenv)
  (local ([define question (lookup-question q)]
          [define request  (send/suspend (make-request-page question))]
          [define bindings (request-bindings request)]
          [define answer   (if (= q 6)
                               promoCode
                               (extract-binding/single 'ans bindings))]
          [define closure  (closureS question answer cenv)]) ;; TODO: answer is random
    (cond
      ;; If user answers yes we send them to question 2, else question 7.
      [(= q 1) (if (equal? "Yes" answer)
                   (send-and-get 2 (env closure cenv))
                   (send-and-get 7 (env closure cenv)))]
      
      ;; If user answers Italy we send them to question 4, else question 3.
      [(= q 2) (if (equal? "Italy" answer)
                   (send-and-get 4 (env closure cenv))
                   (send-and-get 3 (env closure cenv)))]
      
      ;; Always send the user to question 4.
      [(= q 3) (send-and-get 4 (env closure cenv))]
      
      ;; Depending on what user answers and what they answered for questions 2 and 3,
      ;; send user to question 5, 6 or 7.
      [(= q 4) (cond
                 [(< (string->number answer) 0) (send/suspend make-error-page)]
                 [(and (> (string->number answer) 3) 
                       (or 
                        (equal? (lookup-answer (closureS-context closure) (lookup-question 2)) "Italy")
                        (equal? (lookup-answer (closureS-context closure) (lookup-question 3)) "Yes")))
                  (send-and-get 5 (env closure cenv))]
                 [(> (string->number answer) 1)
                  (send-and-get 6 (env closure cenv))]
                 [else
                  (send-and-get 7 (env closure cenv))] )]
      
      ;; Always send user to question 7.
      [(= q 5) (send-and-get 7 (env closure cenv))]
      
      ;; Always send user to question 7.
      [(= q 6) (send-and-get 7 (env closure cenv))]
      
      ;; Last question, print results after this.
      [(= q 7 (send/suspend (make-result-page (env closure cenv))))]
      
      ;; I'm not sure how anyone could get here, but it's probably good to have this.
      [else ('error "send-and-get got question ~a, expected 1 through 7." q)] )))

;; make-result-page : environment ->
;; After question 7, print out the survey results contained in the given environment. Each
;; question and answer the user gave is stored in the environment, so we use a helper function
;; to grab each pair and put it on the page.
(define (make-result-page allEnvs)
  (lambda (k-url)
    (response/xexpr `(html
                      (body
                       (h1 "Survey Result:")
                       ,(format-result allEnvs))))))

;; make-error-page
;; If the user types in an inapproriate answer, for example a negative number in question 4, this
;; error would occur.
(define make-error-page
  (lambda (k-url)
    (response/xexpr `(html
                      (body
                       (h1 "ERROR:")
                       (p "Please go back and give an appropriate answer"))))))

;; format-result : environment -> string
;; ???
(define (format-result allEnvs)
  (if (mtEnv? allEnvs)
      '(p)
      (local ([define curClosure (env-data allEnvs)]
              [define restEnvs   (env-environment allEnvs)]
              [define question   (closureS-question curClosure)]
              [define answer     (closureS-answer curClosure)])
        `(cons ,(format-result restEnvs) (div ,question (br) ,answer)))))

;; make-request-page : string ->
;; Create a page that asks the user for their answer given a question.
(define (make-request-page question)
  (cond 
    [(equal? (lookup-question 6) question)
     (lambda (k-url)
       (response/xexpr `(html
                         (body
                          (form ((action ,k-url) (method "post"))
                                (h2 ,question)
                                (p ,promoCode)
                                (input ((type "submit")
                                        (name "submit")
                                        (value "Next"))))))))]
    [(or (equal? (lookup-question 1) question) 
         (equal? (lookup-question 3) question) 
         (equal? (lookup-question 5) question)
         (equal? (lookup-question 7) question)) 
     (lambda (k-url)
       (response/xexpr `(html
                         (body
                          (form ((action ,k-url) (method "post"))
                                (h2 ,question)
                                (input ((type "radio") (name "ans") (value "Yes"))) "Yes"
                                (br)
                                (input ((type "radio") (name "ans") (value "No"))) "No"
                                (br)
                                (input ((type "submit")
                                        (name "submit")
                                        (value "Submit"))))))))]
    [else (lambda (k-url)
            (response/xexpr `(html
                              (body
                               (form ((action ,k-url) (method "post"))
                                     (h2 ,question)
                                     (input ((type "text") (name "ans")))
                                     (input ((type "submit")
                                             (name "submit")
                                             (value "Submit"))))))))]))

;; Start the server.
(serve/servlet start)