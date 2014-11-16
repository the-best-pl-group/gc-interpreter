;; hw7-starter.ss

(load "hw7-includes.ss")

;; ================== General Helper Functions ========================

(define accumulate
  (lambda (f acc ls)
    (cond
     [(null? ls) acc]
     [else (accumulate f (f (car ls) acc) (cdr ls))])))

;; =============== Environment Definition =============================


;; This is an implementation of the var-val pair list representation
;; of an environment, we wrote earlier.  I translated the
;; representation into a define-datatype so we get the constructors
;; and type checking predicate for free, and can use cases to process.

(define-datatype environment environment?
  (empty-env)                   ;; (empty-env) gives an empty environment
  (extend-env                   ;; (extend-env var val env) extends the environment
   (var symbol?)
   (val ref-val?)
   (env environment?))
  (extend-env-rec                                                                                    
    (p-name symbol?)                                                                                 
    (p-vars (list-of symbol?))                                                                                  
    (p-body expression?)                                                                             
    (env environment?)) 
)

;; (apply-env env target-var) s to figure out the maping of target-var                               
;; in the environment env.                                                                           
(define apply-env ; Env x Var -> SType                                                               
  (lambda (env target-var)                                                                           
    (cases environment env                                                                           
      [empty-env () (raise-exception 'apply-env "No binding for ~s" target-var)]                     
      [extend-env (var val env*)                                                                     
         (cond                                                                                       
           [(equal? var target-var) val]                                                             
           [else (apply-env env* target-var)])]                                                      
      [extend-env-rec (p-name p-vars p-body env*)                                                     
         (cond                                                                                       
           [(equal? p-name target-var)
            (let ([return (newref! (proc-val (procedure p-vars p-body env)))])
              (set! proc-val-ref (expval->ref return))
              return)]                                                
           [else (apply-env env* target-var)])])))      

;; ============== Environment Helper Functions ======================

(define make-init-env
  (lambda ()
    (extend-env 
     'pi (newref! (num-val 3.14159))
     (extend-env
      'e (newref! (num-val 2.71828))
      (empty-env)))))

(define env->string
  (lambda (env)
    (cases environment env 
	   [empty-env () "[]"] 
	   [extend-env (var val env*)
		       (string-append "[" (symbol->string var)
				      " = " (expval->string val)
				      (env->string* env*) "]")] 
	   [extend-env-rec (p-name p-var p-body env*)
			   (string-append "[" (symbol->string p-name) " = #recproc" (env->string* env*) "]")] )))

(define env->string*
  (lambda (env)
    (cases environment env 
	   [empty-env () ""] 
	   [extend-env (var val env*)
		       (string-append ", " (symbol->string var)
				      " = " (expval->string val)
				      (env->string* env*))] 
	   [extend-env-rec (p-name p-var p-body env*)
			   (string-append ", " (symbol->string p-name) " = #recproc" (env->string* env*))] )))


;; =============== Store ====================

;; the-store! is the store!
(define the-store! 'uninitialized)
(define store-count! 'uninitialized)
(define empty-store-spots! 'uninitialized)
(define empty-value 'empty)
(define proc-val-ref 'uninitialized)

;; (empty-store) return an empty Scheme list representing the empty
;; store.
(define empty-store
  (lambda ()
    (make-vector 5 empty-value)))

;; (initialize-store!) it initializes the-store! to (empty-store)
(define initialize-store!
  (lambda ()
    (set! store-count! 0)
    (set! the-store! (empty-store))
    (set! empty-store-spots! '())))

;; doubles the size of the store
(define double-store!
  (lambda ()
    (set! the-store! (double-store!* the-store! (make-vector (* 2 (vector-length the-store!)) empty-value) 0))))

(define double-store!*
  (lambda (old-store new-store ref-num)
    (cond
      [(>= ref-num store-count!) new-store]
      [else
	(vector-set! new-store ref-num (vector-ref old-store ref-num))
	(double-store!* old-store new-store (+ ref-num 1))])))

;;adds a value to the store and increments the store count and returns its ref-val
(define add-to-store!
  (lambda (val)
    (cond [(null? empty-store-spots!)
            (vector-set! the-store! store-count! val)
            (set! store-count! (+ 1 store-count!))
            (ref-val (- store-count! 1))]
          [else
            (let [(ref (car empty-store-spots!))]
                (vector-set! the-store! ref val)
                (set! store-count (+ 1 store-count!))
                (set! empty-store-spots! (cdr empty-store-spots!))
                (ref-val ref))]
            )))

;;removes a value from the store & adds the now empty index to empty-store-spots!
(define remove-from-store!
  (lambda (ref-num)
    (vector-set! the-store! ref-num empty-value)
    (set! empty-store-spots! (cons ref-num empty-store-spots!))))

;; (newref! val) takes a value val adds to the the-store!, and returns
;; a ref-val to the added value val.
(define newref!
  (lambda (val)
    (cond
      [(>= (+ store-count! 1) (vector-length the-store!))
       (double-store!)
       (add-to-store! val)]
      [else
       (add-to-store! val)])))

;; (deref ev) expects that ev is a reference (ref-val ref), and
;; returns the value of associated with ref in the store.
(define deref
  (lambda (ev)
    (let 
      ([ref (expval->ref ev)])
      (vector-ref the-store! ref))))

;; (setref! ev val) expects that ev is a reference (ref-val ref), and
;; it sets the reference ref to val in the the-store!
(define setref!
  (lambda (ev val)
    (let
      ([ref (expval->ref ev)])
      (vector-set! the-store! ref val))))

(define garbage-collect!
  (lambda (env)
    (let ([new-store (make-vector (vector-length the-store!) empty-value)])
      (map (lambda (x) (vector-set!
			 new-store
			 (cdr x)
			 (car x)))
	   (to-keep env))
      (set! the-store! new-store))))

(define to-keep
  (lambda (env)
    (cases environment env
	   [empty-env () '()]
	   [extend-env (var val env*) (cons
					(cons (deref val) (expval->ref val))
					(to-keep env*))]
	   [else (raise-exception 'garbage-collection-to-keep "to-keep garbage collection broke")])))

;; ==================== Expressed Values ==================================

;; Expressed values are Int + Bool + Unit
(define-datatype expval expval?
		 (num-val
		   (num number?))
		 (bool-val
		   (bool boolean?))
		 (unit-val)
		 (proc-val                                                                                          
		   (p proc?))  
		 (ref-val
		   (ref integer?))
		 )

(define-datatype proc proc?                                                                          
		 (procedure                                                                                         
		   (params (list-of symbol?))                                                                                  
		   (body expression?)                                                                               
		   (saved-env environment?)))                                                                       

(define ref-val?
  (lambda (ev)
    (cases expval ev
	   [ref-val (ref) #t]
	   [else #f])))

(define expval->num 
  (lambda (ev)
    (cases expval ev
	   [num-val (num) num]
	   [bool-val (bool) (if bool 1 0)]
	   [else (raise-exception 'expval->num "Expressed value is not a number or a Boolean: ~s" ev)])))

(define expval->bool
  (lambda (ev)
    (cases expval ev
	   [bool-val (bool) bool]
	   [num-val (num) (not (= num 0))]
	   [else (raise-exception 'expval->bool "Expressed value is not a Boolean or a number: ~s" ev)])))

(define expval->ref
  (lambda (ev)
    (cases expval ev
	   [ref-val (ref) ref]
	   [else (raise-exception 'expval->ref "Expressed value is not a reference: ~s" ev)])))


(define expval->proc                                                                                 
  (lambda (ev)                                                                                       
    (cases expval ev                                                                                 
	   [proc-val (p) p]                                                                               
	   [else (raise-exception 'expval->proc "Expressed value is not a procedure: ~s" ev)])))  


(define expval->string
  (lambda (ev)
    (cases expval ev
	   [bool-val (bool) (if bool "#true" "#false")]
	   [num-val (num) (number->string num)]
	   [unit-val () "#unit"]
	   [proc-val (p) "#proc"]      
	   [ref-val (ref) (string-append "#ref(" (number->string ref) ")") ]
	   )))

;; ==================== Evaluater ====================================


;; Returns true iff the variable appears in the environment
(define in-env?
  (lambda (var env)
    (cases environment env
           [empty-env () #f]
           [extend-env (var* val* env*)
                (cond
                  [(equal? var var*) #t]
                  [else (in-env? var env*)])]
           [extend-env-rec (pname pvars pbody env*)
                (in-env? var env*)]
           [else (raise-exception 'in-env? "Environment you're searching in is not a proper environment.")])))

(define value-of
  (lambda (prog env)
    (cases program prog
	   [a-prog (exp) (cons (value-of-exp exp env) env)]
	   [def-prog (var exp)
                 (cond
                   [(in-env? var env) (setref! (apply-env env var) (value-of-exp exp env)) (cons (unit-val) env)]
                   [else (cons (unit-val) (extend-env var (newref! (value-of-exp exp env)) env))])]
	   [else (raise-exception 'value-of-prog "Abstract syntax case not implemented: ~s" (car prog))])))

(define let-garbage-collect 
  (lambda (new-environment)
    (cases environment new-environment
      [extend-env (var val old-environment)
		  (remove-from-store! (expval->ref val))]
      [else (display "you done goofed")])))

(define call-garbage-collect
  (lambda (ref-vals)
    (map (lambda (x) (remove-from-store! x)) ref-vals)))

(define letrec-garbage-collect
  (lambda ()
    (remove-from-store! proc-val-ref)
    (set! proc-val-ref 'uninitialized)))

(define count 0)

(define value-of-exp
  (lambda (exp env)
;;    (display (env->string env))
;;    (display (vector->list the-store!))
;;    (newline)
    (cases expression exp

	   ;; Variable Expressions
	   [var-exp (var) (deref (apply-env env var))]

	   ;; Control Expressions
	   [if-exp (exp1 exp2 exp3) (if (expval->bool (value-of-exp exp1 env)) (value-of-exp exp2 env) (value-of-exp exp3 env))]
	   [let-exp (var exp1 exp2) (let* ([newenv (extend-env var (newref! (value-of-exp exp1 env)) env)]
					   [return-value (value-of-exp exp2 newenv)])
				      (let-garbage-collect newenv)
				      return-value)]

	   ;; Constant Expressions
	   [const-true () (bool-val #t)]
	   [const-false () (bool-val #f)]
	   [const-exp (num) (num-val num)]

	   ;; Arithmetic / Logical Operators
	   [zero?-exp (exp) (apply-unary-op zero? (value-of-exp exp env) expval->num bool-val)]
	   [diff-exp (exp1 exp2) (apply-binary-op - (value-of-exp exp1 env) (value-of-exp exp2 env) expval->num num-val)]
	   [plus-exp (exp1 exp2) (apply-binary-op + (value-of-exp exp1 env) (value-of-exp exp2 env) expval->num num-val)]
	   [div-exp (exp1 exp2) 
		    (apply-binary-op 
		      (lambda (x y) 
			(if (= y 0) (raise-exception 'value-of-exp "Attempt to divide by zero = ~s/~s." x y) (/ x y)))
		      (value-of-exp exp1 env) (value-of-exp exp2 env) expval->num num-val)]
	   [times-exp (exp1 exp2) (apply-binary-op * (value-of-exp exp1 env) (value-of-exp exp2 env) expval->num num-val)]
	   [less-than-exp (exp1 exp2) (apply-binary-op < (value-of-exp exp1 env) (value-of-exp exp2 env) expval->num bool-val)]
	   [equal-exp (exp1 exp2) (apply-binary-op = (value-of-exp exp1 env) (value-of-exp exp2 env) expval->num bool-val)]
	   [and-exp (exp1 exp2) (apply-binary-op (lambda (a b) (and a b)) (value-of-exp exp1 env) (value-of-exp exp2 env) expval->bool bool-val)]
	   [or-exp (exp1 exp2) (apply-binary-op (lambda (a b) (or a b)) (value-of-exp exp1 env) (value-of-exp exp2 env) expval->bool bool-val)]
	   [not-exp (exp) (apply-unary-op not (value-of-exp exp env) expval->num bool-val)]

	   ;; References 
	   [newref-exp (exp) (newref! (value-of-exp exp env))]
	   [deref-exp (exp) (deref (value-of-exp exp env))]      
	   [setref-exp (exp1 exp2) (setref! (value-of-exp exp1 env) (value-of-exp exp2 env)) (unit-val)] 
	   [assign-exp (var exp) (setref! (apply-env env var) (value-of-exp exp env)) (unit-val)]

	   ;; Procedures
	   [proc-exp (vars body) (proc-val (procedure vars body env))]                                      
	   [call-exp (exp exps)                                                                          
		(define ref-vals! '()) 

        (let ([return-value 
        (cases proc (expval->proc (value-of-exp exp env))                                                               
			    [procedure (params body saved-env)                                                        
				       (cond
					 [(= (length params) (length exps)) 
					  (let
					    ([vals (map (lambda (x) (value-of-exp x env)) exps)])
					    (value-of-exp body 
							  (accumulate 
							    (lambda (head acc) 
                                  (let ([ref (newref! (cdr head))])
                                  (set! ref-vals! (cons (expval->ref ref) ref-vals!))
                                  (extend-env (car head) ref acc))) 
							    saved-env
							    (reverse (map (lambda (param val) (cons param val)) params vals)))))]
					 [else (raise-exception 'value-of-exp 
								"Attempt to apply function with inconsistent number of arguments: ~s ~s." exp exps)])])])
          (call-garbage-collect ref-vals!)
          return-value)]
	   [letrec-exp (p-name p-vars p-body body)                                                         
		       (let* ([newenv (extend-env-rec p-name p-vars p-body env)]
                              [return-value (value-of-exp body newenv)])
			 (letrec-garbage-collect)
			 return-value)]

	   ;; Printing
	   [print-exp (exp) (display (expval->string (value-of-exp exp env))) (unit-val)]
	   [newline-exp ()  (newline) (unit-val)]

	   ;; Blocks
	   [block-exp (exps) (accumulate (lambda (exp acc) (value-of-exp exp env)) (unit-val) exps)]

	   [else (raise-exception 'value-of-exp "Abstract syntax case not implemented: ~s" (car exp))])))

;; ==================== Evaluation Helper Functions ====================

(define all?
  (lambda (p? ls)
    (accumulate (lambda (head acc) (and (p? head) acc)) #t ls)))

(define apply-unary-op
  (lambda (op arg in-acc out-cons)
    (cases expval arg

      [else (apply-unary-op-scalar op arg in-acc out-cons)])))

(define apply-unary-op-scalar
  (lambda (op arg in-acc out-cons)
    (out-cons (op (in-acc arg)))))

(define apply-binary-op
  (lambda (op arg1 arg2 in-acc out-cons)
    (apply-binary-op* op arg1 arg2 in-acc out-cons)))

(define apply-binary-op*
  (lambda (op arg1 arg2 in-acc out-cons)
	 (apply-binary-op-scalar op arg1 arg2 in-acc out-cons)))
                   
(define apply-binary-op-scalar
  (lambda (op arg1 arg2 in-acc out-cons)
    (out-cons (op (in-acc arg1) (in-acc arg2)))))

(define range
  (lambda (start end inc)
    (cond 
     [(> start end) '()]
     [(= start end) (list (num-val end))]
     [else (cons (num-val start) (range (+ start inc) end inc))])))


;; ==================== Interpreter ====================================

;; (start) -- Starts the interpreter.
(define start
  (lambda ()
    (begin
      (display "\n=== Welcome to the Basic HW 5 Interpreter === \n\n")
      (initialize-store!)
      (read-eval-print (make-init-env)))))

;; (get-input-string) -- Reads a line from the interactive input
;; port.  Ignores zero length strings.
(define get-input-string
  (lambda ()
    (let ([str (get-line (current-input-port))])
      (if (= (string-length str) 0) 
	  (get-input-string)
	  str))))

;; (read-eval-print) -- Main read, eval, and print loop.
(define read-eval-print
  (lambda (env)
    ;; Display an interpreter prompt
    (display "==> ")
    ;; Read a line user input
    (let ([code (get-input-string)])
      (cond 
       [(equal? code "!quit")
	(display "Goodbye!")  ;; Quit if 'quit entered.
	(newline)]
       [else   ;; Do something
	(cond
	 [(equal? code "!debug0")
	  (untrace value-of value-of-exp)
	  (untrace expval->num expval->bool expval->string)]
	 [(equal? code "!debug1")
	  (trace value-of value-of-exp)
	  (untrace expval->num expval->bool expval->string expval->proc)]
     [(equal? code "!debug2")
	  (trace value-of value-of-exp expval->num expval->bool expval->string expval->proc)]
	 [(equal? code "!debug3")
	  (trace value-of value-of-exp expval->num expval->bool expval->string apply-env expval->proc extend-env empty-env)]
	 [(equal? code "!env")
	  (display (env->string env))
	  (newline)]
	 [(equal? code "!store")
	  (display (vector->list the-store!)) ;; this should be fixed so that it prints human-readable output
	  (newline)]
	 [(equal? code "!reset-env")
	  (set! env (make-init-env))]
	 [else
	 ;; Parse code, eval expression, and print result.
	  (guard 
	   (e [else (display-exception e)]) ;; Now handles exceptions based on datatypes
	   (let
	       ([abstract-code (parse code)])  ;; Try to parse the input line
	     (let*
		 ([result (value-of abstract-code env)]
		  [val (car result)]
		  [new-env (cdr result)])
	       (display (expval->string val))
	       (set! env new-env)  
	       (newline)
	       )))])
	(garbage-collect! env)
	(read-eval-print env)]))))
