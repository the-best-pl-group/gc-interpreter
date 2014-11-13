;; hw7-includes.ss


;; ================ Parser Definitions ==================================

;; This define the lexical specification of the scanner
(define the-lexical-spec
  '((whitespace (whitespace) skip)                   ;; PL ignores whitespace
    (comment ("%" (arbno (not #\newline))) skip)     ;; PL ignores comments written by %
    (identifier                                          
     (letter (arbno (or letter digit "_" "-" "?")))  ;; PL has ids which begin with a letter followed by 
     symbol)                                         ;; any number of letters digits _ - ? 
    (number (digit (arbno digit)) number)            ;; PL has positive integer numbers
    (number ("-" digit (arbno digit)) number)        ;; PL has negative integer numbers
    (number ("-" digit (arbno digit) "." digit (arbno digit)) number)  ;; PL has negative floating point numbers
    (number (digit (arbno digit) "." digit (arbno digit)) number)   ;; PL has positive floating points numbers
    ))

;; This defines the translation from the concrete syntax to the abstract syntax
(define the-grammar
  '((program                        ;; <Program> ::= 
     (expression)                   ;;   Concrete    <Expression>
     a-prog)                        ;;   Abstract    (a-prog exp)
    
    (expression                     ;; <Expression> ::= 
     (number)                       ;;   Concrete       <Number> 
     const-exp)                     ;;   Abstract       (const-exp num)
    
    (expression                     ;; <Expression> ::= 
     ("zero?(" expression ")")      ;;   Concrete       zero?(<Expression>)
     zero?-exp)                     ;;   Abstract       (zero?-exp exp)
    
    (expression                                             ;; <Expression> ::= 
     ("if" expression "then" expression "else" expression)  ;;   Concrete       if <Expression> then <Expression> else <Expression>
     if-exp)                                                ;;   Abstract       (if-exp exp1 exp2 exp3)
        
    (expression                     ;; <Expression> ::= 
     (identifier)                   ;;   Concrete       <Identifier>
     var-exp)                       ;;   Abstract       (var-exp var)
    
    (expression                                          ;; <Expression> ::= 
     ("let" identifier "=" expression "in" expression)   ;;   Concrete       let <Identifier> = <Expression> in <Expression>
     let-exp)                                            ;;   Abstract       (let-exp var exp1 exp2)


    ;; ============== New definitions for HW5 below ========================

    (program                               ;; <Program> ::= 
     ("def!" identifier "=" expression)    ;;  Concrete     def! <Identifier> = <Expression>
     def-prog)                             ;;  Abstract     (def-prog var exp)
    
    (expression                            ;; <Expression> ::= 
     ("#true")                             ;;   Concrete       #true
     const-true)                           ;;   Abstract       (const-true)
    
    (expression                            ;; <Expression> ::=
     ("#false")                            ;;   Concrete       #false
     const-false)                          ;;   Abstract       (const-false)
     
    (expression                            ;; <Expression> ::= 
     ("*(" expression "," expression ")")  ;;   Concrete       *(<Expression>,<Expression>)
     times-exp)                            ;;   Abstract       (times-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::= 
     ("/(" expression "," expression ")")  ;;   Concrete       /(<Expression>,<Expression>)
     div-exp)                              ;;   Abstract       (div-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::= 
     ("-(" expression "," expression ")")  ;;   Concrete       -(<Expression>,<Expression>)
     diff-exp)                             ;;   Abstract       (diff-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::= 
     ("+(" expression "," expression ")")  ;;   Concrete       +(<Expression>,<Expression>)
     plus-exp)                             ;;   Abstract       (plus-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::= 
     ("=(" expression "," expression ")")  ;;   Concrete       =(<Expression>,<Expression>)
     equal-exp)                            ;;   Abstract       (equal-exp exp1 exp2)

    (expression                            ;; <Expression> ::= 
     ("<(" expression "," expression ")")  ;;   Concrete       <(<Expression>,<Expression>)
     less-than-exp)                        ;;   Abstract       (less-than-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::= 
     ("&(" expression "," expression ")")  ;;   Concrete       &(<Expression>,<Expression>)
     and-exp)                              ;;   Abstract       (and-exp exp1 exp2)

    (expression                            ;; <Expression> ::= 
     ("|(" expression "," expression ")")  ;;   Concrete       |(<Expression>,<Expression>)
     or-exp)                               ;;   Abstract       (or-exp exp1 exp2)
    
    (expression                            ;; <Expression> ::= 
     ("!(" expression ")")                 ;;   Concrete       !(<Expression>)
     not-exp)                              ;;   Abstract       (not-exp exp)
 
    (expression                                      ;; <Expression> ::=
     ("proc" "(" (arbno identifier) ")" expression)  ;;   Concrete       proc ({<Identifier>}*) <Expression>
     proc-exp)                                       ;;   Abstract       (proc-exp vars exp) 
    
    (expression                                      ;; <Expression> ::= 
     ("(" expression (arbno expression) ")")         ;;   Concrete       (<Expression> {<Expression>}*)
     call-exp)                                       ;;   Abstract       (call-exp exp exps)
    
    (expression                                      ;; <Expression> ::=                                 
     ("letrec" identifier "(" (arbno identifier)     ;;   Concrete       letrec <Identifier>({<Identifier>}*) = <Expression> in <Expression>
      ")" "=" expression "in" expression)            ;;   Abstract       (letrec-exp p-name p-vars p-body body)
     letrec-exp)
   
    (expression                                           ;; <Expression> ::=
     ("{" (arbno expression) "}")                         ;;   Concrete       {{<Expression>}*}
     block-exp)                                           ;;   Abstract       (block-exp exps)

    (expression                                           ;; <Expression> ::=
     ("print!" "(" expression ")")                        ;;   Concrete       print!(<Expression>)
     print-exp)                                           ;;   Abstract       (print-exp exp)                         

    (expression                                           ;; <Expression> ::=
     ("newline!")                                         ;;   Concrete       newline!
     newline-exp)                                         ;;   Abstract       (newline-exp) 

    (expression                                           ;; <Expression> ::=
     ("newref!" "(" expression ")")                       ;;   Concrete       newref!(<Expression>)
     newref-exp)                                          ;;   Abstract       (newref-exp exp)
    
    (expression                                           ;; <Expression> ::=
     ("deref" "(" expression ")")                         ;;   Concrete       deref(<Expression>)
     deref-exp)                                           ;;   Abstract       (deref-exp exp)
    
    (expression                                           ;; <Expression> ::=
     ("setref!" "(" expression "," expression ")")        ;;   Concrete       setref!(<Expression>,<Expression>)
     setref-exp)                                          ;;   Abstract       (setref-exp exp1 exp2)
    
    (expression                                           ;; <Expression> ::=
     ("set" identifier "=" expression )                   ;;   Concrete       set <Identifier> = <Expression>
     assign-exp)                                          ;;   Abstract       (assign-exp var exp)
    
    ))

;; ================= sllgen boilerplate ==================================

;; This command makes the abstract grammar from the description in the-grammar
;; With this you don't need to use define-datatypes
(sllgen:make-define-datatypes the-lexical-spec the-grammar)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

;; Use to translate from concrete syntax to abstract syntax
(define scan&parse
  (sllgen:make-string-parser the-lexical-spec the-grammar))

(define just-scan
  (sllgen:make-string-scanner the-lexical-spec the-grammar))

(define parse
  (lambda (str)
    (scan&parse str)))

;; ================== Error Handling & Display ============================

;; Datatype for encapsulating exception information for later
;; display. Has one variant: (ex-val who format data)
;;   who = a symbol specifying the name of the function the exception
;;         occured in.
;;   format = an eopl:printf appropriate format string indicating the
;;            type of error.
;;   data = a list of values to put into the format string when displayed.
(define-datatype except except?
  (runtime-except-val
   (who symbol?)
   (format string?)
   (data list?))
  (parse-except-val
   (who symbol?)
   (format string?)
   (data list?))
)

;; Displays the exception message.  The input to this function should
;; be data constructed using (except-val who format data). In the case the
;; data is not given via an except-val, the function assumes the exception
;; was thrown by Scheme (not us) and attempts to display it.
(define display-exception
  (lambda (e)
    (cond
     [(except? e)   ;; Raised by us.
      (cases except e
	     [runtime-except-val (who format data) 
				 (display "Runtime Error:\n")
				 (apply eopl:printf (cons format data))
				 (newline)]
	     [parse-except-val (who format data) 
			       (display "Parse Error:\n")
			       (apply eopl:printf (cons format data))
			       (newline)])]
     [else          ;; Raised by Scheme.
      (display "Exception raised by Scheme:\n")
      (set! doh e)
      (if (who-condition? e)
	  (apply printf (cons "  Exception occured in ~:s:" (list (condition-who e))))
	  #t
	  )
      (if (and (message-condition? e) (irritants-condition? e))
	  (apply printf (cons (string-append "  " (condition-message e)) (condition-irritants e)))
	  (if (message-condition? e)
	      (apply printf (list (condition-message e)))
	      #t
	      ))
      (newline)]
     )))

(define raise-exception-maker
  (lambda (evar)
    (lambda (who format . data)
      (raise (evar who format data))
      )))

;; Overrides sllgen:error to prevent stopping.
(define sllgen:error (raise-exception-maker parse-except-val))
(define raise-exception (raise-exception-maker runtime-except-val))
