;;;; ***************************************************
;;;; Group 15
;;;; Tyler Avery (tma58)
;;;; Thomas Bornhorst (thb34)
;;;; CSDS 345 Spring 2023
;;;; Simple Language Interpreter Project, Part 2
;;;; ***************************************************

#lang racket
(require "classParser.rkt")

(define testFile "mainTest.txt")
(define testFile2 "tests/test1.txt")

(parser testFile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;main interpreter functionality;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;main interpret call that parses file and calls the main interpreter functionality
(define interpret
  (lambda (filename class-name)
    (interpreter-main (parser filename) (state-init) class-name)))

(define error-break (lambda(v) (error 'breakoutsideloop "Break statement not inside while loop")))
(define error-continue (lambda (v) (error 'continueoutsideloop  "Continue statement not inside while loop")))
(define error-throw (lambda (v) (error 'throwwithoutcatch "Throw statement without catch")))

;starts interpreter - runs outer layer then calls main
(define interpreter-main
  (lambda (tree state class-name)
    (state-get-class-definitions tree state (lambda (s) (interpreter-start (closure-function-body (value-class-closure-find-main (value-get-var class-name s))) (state-add-layer s))))))

;global layer of interpreter getting class definitions
(define state-get-class-definitions
  (lambda (tree state return)
    (cond
      [(null? tree) (return state)]
      [(eq? (operator (first-stmt tree)) 'class) (state-get-class-definitions (other-stmts tree) (state-define-class (first-stmt tree) state) return)]
      [else (state-get-class-definitions (other-stmts tree) state return)])))

;outer layer - global variable declarations & function definitions
(define state-get-definitions
  (lambda (tree state throw return)
    (cond
      [(null? tree) (return state)]
      [(null? (first-stmt tree)) (return state)]
      [(eq? (operator (first-stmt tree)) 'function) (state-get-definitions (other-stmts tree) (state-define-function (first-stmt tree) state) throw return)]
      [(eq? (operator (first-stmt tree)) 'var) (state-get-definitions (other-stmts tree) (state-declaration-global (first-stmt tree) state throw) throw return)]
      [else (state-get-definitions (other-stmts tree) state throw return)])))

;start of rest of interpreter
(define interpreter-start
  (lambda (tree state)
    (call/cc
     (lambda (ret)
       (interpret-statement tree state ret error-break error-continue error-throw)))))

;define behavior for various keywords of statements
(define interpret-statement
  (lambda (tree state main-return break continue throw)
    (cond
      [(null? tree) state]
      [(null? (first-stmt tree)) state]
      [(eq? (operator (first-stmt tree)) 'function) (interpret-statement (other-stmts tree) (state-define-function (first-stmt tree) state) main-return break continue throw)]
      [(eq? (operator (first-stmt tree)) 'var) (interpret-statement (other-stmts tree) (state-declaration (first-stmt tree) state throw) main-return break continue throw)]
      [(eq? (operator (first-stmt tree)) 'funcall) (state-function-call (first-stmt tree) state throw (lambda (v) (interpret-statement (other-stmts tree) state main-return break continue throw)))]
      [(eq? (operator (first-stmt tree)) 'if) (interpret-statement (other-stmts tree) (state-if (first-stmt tree) state main-return break continue throw) main-return break continue throw)]
      [(eq? (operator (first-stmt tree)) 'while) (interpret-statement (other-stmts tree) (call/cc (lambda (new-break)
                                                            (state-while (first-stmt tree) state main-return new-break continue throw))) main-return break continue throw)]
      [(eq? (operator (first-stmt tree)) 'return) (return-exit (first-stmt tree) state throw main-return)]
      [(eq? (operator (first-stmt tree)) '=) (interpret-statement (other-stmts tree) (state-assignment (first-stmt tree) state throw) main-return break continue throw)]
      [(eq? (operator (first-stmt tree)) 'begin) (interpret-statement (other-stmts tree) (state-pop-layer (interpret-statement (other-stmts (first-stmt tree)) (state-add-layer state) main-return break continue throw)) main-return break continue throw)]
      [(eq? (operator (first-stmt tree)) 'break) (break (state-pop-layer state))]
      [(eq? (operator (first-stmt tree)) 'continue) (continue state)]
      [(eq? (operator (first-stmt tree)) 'try) (interpret-statement (other-stmts tree) (state-try (first-stmt tree) state main-return break continue throw) main-return break continue throw)]
      [(eq? (operator (first-stmt tree)) 'throw) (value-process-expression (first-operand (first-stmt tree)) state error-throw (lambda (v) (throw (state-var-declaration 'throw v state))))]
      [(eq? (operator (first-stmt tree)) 'finally) (interpret-statement (cadr (first-stmt tree)) state main-return break continue throw)]
      [else (error 'norelop "No relevant statements found")])))

;;;;;;;;;; CLASSES
;get certain information from class definition statement
(define class-definition-name cadr)
(define class-definition-extends caddr)
(define class-definition-body cadddr)
(define class-definition-extends-class-name cadr)

;get certain information from class closure
(define class-closure-super car)
(define class-closure-instance-fields cadr)
(define class-closure-methods caddr)
(define class-closure-class-fields cadddr)
(define class-closure-constructors
  (lambda (stmt) (cadddr (cdr stmt))))

(define value-class-closure-find-main
  (lambda (class-closure)
    (value-get-var 'main (class-closure-methods class-closure))))      

;class definition
(define state-define-class
  (lambda (stmt state)
    (state-var-declaration (class-definition-name stmt) (value-make-class-closure stmt state) state)))

;make closure for class - super class, instance field names & expressions for initial values, methods/function names & closures, class field names/values, constructors
(define value-make-class-closure
  (lambda (stmt state)
    (cons (value-class-definition-get-super (class-definition-extends stmt)) (cons (value-class-definition-get-instance-fields (class-definition-body stmt)) (cons (value-class-definition-get-methods (class-definition-body stmt)) (cons (value-class-definition-get-class-fields (class-definition-body stmt)) (list (value-class-definition-get-constructors (class-definition-body stmt)))))))))

;get name of super class
(define value-class-definition-get-super
  (lambda (extends-stmt)
    (if (null? extends-stmt) '() (class-definition-extends-class-name extends-stmt))))

(define stmt-var-name cadr)
(define stmt-expression caddr)

;get instance field names & expressions that calculate the initial values (if any)
(define value-class-definition-get-instance-fields
  (lambda (class-body)
    (cond
      [(null? class-body) (state-init)]
      [(eq? (operator (first-stmt class-body)) 'var) (state-var-declaration (stmt-var-name (first-stmt class-body)) (stmt-expression (first-stmt class-body)) (value-class-definition-get-instance-fields (other-stmts class-body)))]
      [else (value-class-definition-get-instance-fields (other-stmts class-body))])))

;get methods / function names and closures (abstract and static functions)
(define value-class-definition-get-methods
  (lambda (class-body)
    (cond
      [(null? class-body) (state-init)]
      [(or (eq? (operator (first-stmt class-body)) 'function) (eq? (operator (first-stmt class-body)) 'static-function)) (state-define-function (first-stmt class-body) (value-class-definition-get-methods (other-stmts class-body)))]
      [else (value-class-definition-get-methods (other-stmts class-body))])))

;get class field names / initial values
(define value-class-definition-get-class-fields
  (lambda (class-body)
    (cond
      [(null? class-body) (state-init)]
      [(eq? (operator (first-stmt class-body)) 'static-var) (state-var-declaration (stmt-var-name (first-stmt class-body)) (stmt-expression (first-stmt class-body)) (value-class-definition-get-class-fields (other-stmts class-body)))]
      [else (value-class-definition-get-class-fields (other-stmts class-body))])))

;get constructors
(define value-class-definition-get-constructors
  (lambda (class-body)
    (cond
      [(null? class-body) (state-init)]
      [(eq? (operator (first-stmt class-body)) 'constructor) (state-var-declaration (stmt-var-name (first-stmt class-body)) (stmt-expression (first-stmt class-body)) (value-class-definition-get-constructors (other-stmts class-body)))]
      [else (value-class-definition-get-constructors (other-stmts class-body))])))

;create instance closure - instance's class, instance field values
(define value-make-instance-closure
  (lambda (class-name state)
    (cons class-name (list (value-class-init-instance-fields (class-closure-instance-fields (value-get-class-closure class-name state)))))))

(define value-get-class-closure
  (lambda (class-name state)
    (value-get-var class-name state)))

(define value-class-init-instance-fields
  (lambda (class-closure-instance-fields)
    (if (or (null? class-closure-instance-fields) (first-stmt class-closure-instance-fields)) (state-init)
        (state-var-declaration (car (first-stmt class-closure-instance-fields)) (value-get-var (car (first-stmt class-closure-instance-fields)) class-closure-instance-fields) (value-class-init-instance-fields (other-stmts class-closure-instance-fields))))))

;;;;;;;;;;

;implementation of a try statement
(define state-try
  (lambda (stmt state main-return break continue throw)
    (state-pop-layer (interpret-statement (list (full-finally-stmt stmt)) (state-add-layer (state-catch stmt (call/cc (lambda (new-throw) (interpret-statement (try-stmt stmt) (state-add-layer state) main-return break continue new-throw))) main-return break continue throw)) main-return break continue throw))))

;implementation of a catch statement
(define state-catch
  (lambda (stmt state main-return break continue throw)
    (cond
      [(null? (catch-stmt stmt)) (state-pop-layer state)]
      [(var-is-declared? 'throw state) (state-pop-layer (interpret-statement (caddr (catch-stmt stmt)) (state-add-layer (state-var-declaration (car (cadr (catch-stmt stmt))) (value-get-var 'throw state) (state-pop-layer state))) main-return break continue throw))]
      [else (state-pop-layer state)])))

;implementation of an if statement
(define state-if
  (lambda (stmt state main-return break continue throw)
    (if (value-check-condition stmt state throw)
        (interpret-statement (list (second-operand stmt)) state main-return break continue throw)
        (if (not (null? (third-operand stmt)))
            (interpret-statement (list (third-operand stmt)) state main-return break continue throw)
            state))))

;implementation of a while loop
(define state-while
  (lambda (stmt state main-return break continue throw)
    (if (value-check-condition stmt state throw)
        (state-while stmt (call/cc (lambda (new-continue) (interpret-statement (list (second-operand stmt)) state main-return break new-continue throw))) main-return break continue throw)
        state)))

;function that defines variables as used in the interpreter
(define state-declaration
  (lambda (expr state throw)
    (value-process-expression (decl-right-operand expr) state throw (lambda (v) (state-var-declaration (left-operand expr) v state)))))

(define state-declaration-global
  (lambda (expr state throw)
    (value-process-expression (decl-right-operand expr) state throw (lambda (v) (state-var-declaration-global (left-operand expr) v state)))))

;sets the value of a variable
(define state-assignment
  (lambda (expr state throw)
    (value-process-expression (right-operand expr) state throw (lambda (v) (state-update-var (left-operand expr) v state)))))

;returns the result of an expression
(define return-exit
  (lambda (expr state throw return)
    (value-process-expression (first-operand expr) state throw (lambda (v) (return (value-convert-return v))))))

;;;;;;;;;;;;;;;;; FUNCTIONS

;function definition
(define state-define-function
  (lambda (stmt state)
    (state-var-declaration (function-name stmt) (value-make-closure stmt state) state)))

;make closure for function
(define value-make-closure
  (lambda (stmt state)
    (cons (formal-parameters stmt) (cons (function-body stmt) (list (lambda (current-state) (state-function-environment (function-name stmt) current-state)))))))

;take in state at call and return function environment - everything on define-state and lower (check for name of function)
(define state-function-environment
  (lambda (function-name call-state)
    (if (var-in-layer? function-name (top-layer call-state)) call-state (state-function-environment function-name (cdr call-state)))))

;check if variable is in the layer given
(define var-in-layer?
  (lambda (var layer)
    (cond
      [(null? layer) #f]
      [(eq? var (var-name (first-pair layer))) #t]
      [else (var-in-layer? var (rest-of-pairs layer))])))

;call function - only modifications to the state will be through global variables so don't need to actually return the state
(define state-function-call
  (lambda (stmt state throw return)
    (return (value-function-call stmt state throw))))

;get closure, call function, and return the value
(define value-function-call
  (lambda (stmt state throw)
    (value-function-call-with-closure stmt (value-get-var (function-call-name stmt) state) state throw)))

;Get fstate -> bind parameters -> run function body
(define value-function-call-with-closure
  (lambda (stmt closure state throw)
    (call/cc
     (lambda (ret)
       (state-function-throw (call/cc (lambda (new-throw) 
    (interpret-statement (closure-function-body closure) (state-bind-parameters (closure-formal-parameters closure) (actual-parameters stmt) (state-add-layer ((get-function-environment closure) state)) state throw) ret error-break error-continue new-throw))) state throw)))))

;if throw in fstate -> add top layer of fstate to state -> throw with state
(define state-function-throw
  (lambda (fstate state throw)
    (if (var-is-declared? 'throw fstate) (throw (state-add-on-layer (top-layer fstate) state)) state)))
     
;bind actual parameters to formal parameters
(define state-bind-parameters
  (lambda (formal-parameters actual-parameters fstate state throw)
    (cond
      [(and (null? formal-parameters) (null? actual-parameters)) fstate]
      [(or (null? formal-parameters) (null? actual-parameters)) (error 'mismatchedparams "Mismatched parameters and arguments")]
      [else (value-process-expression (first-parameter actual-parameters) state throw (lambda (v) (state-bind-parameters (rest-of-parameters formal-parameters) (rest-of-parameters actual-parameters) (state-var-declaration (first-parameter formal-parameters) v fstate) state throw)))])))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;variables - functions that directly interact with the state;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;checks whether a given variable is declared
(define var-is-declared?
  (lambda (var state)
    (cond
      [(null? state) #f]
      [(null? (top-layer state)) (var-is-declared? var (rest-of-pairs state))]
      [(eq? var (top-first-var-name state)) #t] ; car state = top-layer, car top-layer = first-binding-pair, car first-binding-pair = var name
      [else (var-is-declared? var (rest-of-state state))])))

;declares a "global" variable, making sure it ins't defined in any layer
(define state-var-declaration-global
  (lambda (var val state)
    (if (var-is-declared? var state)
        (error 'vardeclaredtwice "Variable already declared")
        (cons (cons (new-binding-pair var val) (top-layer state)) (cdr state)))))

;defines variable, checking that it isn't defined in top layer
(define state-var-declaration
  (lambda (var val state)
    (if (var-in-layer? var (top-layer state))
        (error 'vardeclaredtwice "Variable already declared")
        (cons (cons (new-binding-pair var val) (top-layer state)) (cdr state)))))

;updates the value of an already defined variable
(define state-update-var
  (lambda (var val state)
    (state-update-var-CPS var val state (lambda (v) v))))

;updates the value of an already defined variable with CPS
(define state-update-var-CPS
  (lambda (var val state return)
    (cond
      [(null? state) (error 'varnotdeclared "Variable not yet declared")]
      [(null? (top-layer state)) (state-update-var-CPS var val (cdr state) (lambda (v) (return (cons '() v))))]
      [(eq? var (top-first-var-name state)) (begin (set-box! (cadr (first-pair (top-layer state))) val) (return state))]
      [else (state-update-var-CPS var val (rest-of-state state) (lambda (v) (return (cons (cons (first-pair (top-layer state)) (top-layer v)) (cdr v)))))])))

;gets value of variable
(define value-get-var
  (lambda (var state)
    (cond
      [(null? state) (error 'varnotdeclared "Variable not yet declared")]
      [(null? (top-layer state)) (value-get-var var (cdr state))]
      [(eq? var (top-first-var-name state)) (unbox (cadr (first-pair (top-layer state))))]
      [else (value-get-var var (rest-of-state state))])))

;return the starting state
(define state-init
  (lambda ()
    (list '())))

;add a new empty layer to the top of the state
(define state-add-layer
  (lambda (state)
    (cons '() state)))

;pop off the top layer of the state
(define state-pop-layer
  (lambda (state)
    (cdr state)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;integer and conditional operations;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;defines behavior for integer operations
(define value-integer-operations
  (lambda (expr state)
    (cond
      [(or (null? (left-operand expr)) (null? (right-operand expr))) (error 'varnotassgn "Variable not yet assigned")]
      [(eq? (operator expr) '+) (+         (left-operand expr) (right-operand expr))]
      [(eq? (operator expr) '-) (-         (left-operand expr) (right-operand expr))]
      [(eq? (operator expr) '*) (*         (left-operand expr) (right-operand expr))]
      [(eq? (operator expr) '/) (quotient  (left-operand expr) (right-operand expr))]
      [(eq? (operator expr) '%) (remainder (left-operand expr) (right-operand expr))]
      [else (value-conditional-operations expr state)]))) ; if not an integer operation, pass to conditional operations

;defines behavior for conditional operators
;! op included in processExpression checks
(define value-conditional-operations
  (lambda (condition state)
    (cond
      ((eq? (operator condition) '==) (eq?      (left-operand condition) (right-operand condition)))
      ((eq? (operator condition) '!=) (not (eq? (left-operand condition) (right-operand condition))))
      ((eq? (operator condition) '<) (<         (left-operand condition) (right-operand condition)))
      ((eq? (operator condition) '>) (>         (left-operand condition) (right-operand condition)))
      ((eq? (operator condition) '<=) (<=       (left-operand condition) (right-operand condition)))
      ((eq? (operator condition) '>=) (>=       (left-operand condition) (right-operand condition)))
      ((eq? (operator condition) '&&) (and      (left-operand condition) (right-operand condition)))
      ((eq? (operator condition) '||) (or       (left-operand condition) (right-operand condition)))
      (else (error 'unknownop "Bad Operator")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;expression processing;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;returns 'true' and 'false' rather than '#t' and '#f"
(define value-convert-return
  (lambda (val)
    (cond
      [(eq? val #t) 'true]
      [(eq? val #f) 'false]
      [(null? val) (error 'returnbeforeinit "Can't return value before initializing it")]
      [else val])))

(define class-new-instance-name cadr)

;proccesses a given expression
(define value-process-expression
  (lambda (expr state throw return)
    (cond
      [(null? expr) (return '())]
      [(not (list? expr)) (return (value-single-expr expr state))] ; if just a value
      [(eq? (operator expr) 'funcall) (return (value-function-call expr state throw))]
      [(eq? (operator expr) 'new) (return (value-make-instance-closure (class-new-instance-name expr) state))]
      [(is-one-operand? expr) (value-single-operand-expr expr state throw return)] ; if only one operand
      [(list? (left-operand expr)) (if (list? (right-operand expr)) ; if left-operand is a list (nested expression), recurse through it - if right-operand is a list, recurse through it
                                       (value-process-expression (left-operand expr) state throw
                                                                 (lambda (v1) (value-process-expression (right-operand expr) state throw
                                                                                                        (lambda (v2) (value-process-expression
                                                                                                                      (value-assemble-expression (operator expr) v1 v2) state throw return))))) ; both operands are lists
                                      (value-process-expression (left-operand expr) state throw
                                                                (lambda (v1) (value-process-expression (value-assemble-expression (operator expr) v1 (right-operand expr)) state throw return))))] ; only left-operand is a list
      [(list? (right-operand expr)) (value-process-expression (right-operand expr) state throw
                                                              (lambda (v2) (value-process-expression (value-assemble-expression (operator expr) (left-operand expr) v2) state throw return)))] ; only right-operand is a list
      [else (return (value-integer-operations (value-assemble-expression (operator expr) ; (essentially the base case) if neither are lists, make sure each operand is a value and pass to integer/conditional operations functions
                                                                         (value-single-expr (left-operand expr) state)
                                                                         (value-single-expr (right-operand expr) state)) state))])))

;processes the result of an expression with one operand
(define value-single-operand-expr
  (lambda (expr state throw return)
    (cond
      [(eq? (operator expr) '!) (value-process-expression (first-operand expr) state throw (lambda (v) (return (not v))))]
      [(eq? (operator expr) '-) (value-process-expression (first-operand expr) state throw (lambda (v) (return (* v -1))))])))

;returns the value of an expression with one element
(define value-single-expr
  (lambda (expr state)
    (cond
      [(eq? expr 'false) #f]
      [(eq? expr 'true) #t]
      [(eq? expr #f) #f]
      [(eq? expr #t) #t]
      [(number? expr) expr]
      [else (value-get-var expr state)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; helper functions to abstract the details and get the denotational code above to read better ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;function abstractions:
;function name in definition
(define function-name cadr)

;formal parameters of statement when defining
(define formal-parameters caddr)

;function body
(define function-body cadddr)

;function name in call
(define function-call-name cadr)

;actual parameters of statement
(define actual-parameters cddr)

;formal parameters in closure
(define closure-formal-parameters car)

;function body in closure
(define closure-function-body cadr)

;returns function enviornment function from closure
(define get-function-environment caddr)

;first parameter in list
(define first-parameter car)

;other parameters in list
(define rest-of-parameters cdr)

;abstraction for if and while functions
(define value-check-condition
  (lambda (stmt state throw)
    (value-check-condition-errors (value-process-expression (first-operand stmt) state throw (lambda (v) v)))))

(define value-check-condition-errors
  (lambda (val)
    (cond
      [(null? val) (error 'nullcondtition "Condition can't be null value")]
      [(number? val) (error 'numbercondition "Condition can't result in number")]
      [else val])))

;defines the operator as the car of the expression
(define operator
  (lambda (expression)
    (car expression)))

;defines the left-operand as the cadr of the expression
(define left-operand cadr)

;define right-operand as the caddr of the expression
(define right-operand caddr)

;defines the first-operand as the same as the left-operand (for readability and clarity)
(define first-operand left-operand)

;defines the second-operand as the same as the right-operand (for readability and clarity)
(define second-operand right-operand)

;define third-operand
(define third-operand
  (lambda (stmt)
    (if (null? (cdr(cdr(cdr stmt)))) '()
        (cadddr stmt))))

;gets right operand of a declaration expression
(define decl-right-operand
  (lambda (expression)
    (if (null? (cdr (cdr expression)))
        '()
        (right-operand expression))))

(define try-stmt cadr)
(define catch-stmt caddr)
(define full-finally-stmt cadddr)

;first statement of syntax tree
(define first-stmt
  (lambda (tree)
    (car tree)))

;list of all other statements of syntax tree after the first statement
(define other-stmts
  (lambda (stmt)
    (cdr stmt)))

;checks whether an expression has one operand
(define is-one-operand?
  (lambda (expr)
    (null? (cdr (cdr expr)))))

;combines combines the parts of an expression into the appropriate (operator left-operand right-operand) format
(define value-assemble-expression
  (lambda (operator left-operand right-operand)
    (cons operator (cons left-operand (list right-operand)))))

;add layer on top of state
(define state-add-on-layer cons)

;defines the top layer as car
(define top-layer car)

;defines the first binding pair as car
(define first-pair car)

;defines the var name as car
(define var-name car)

;returns the state excluding the first binding pair in the top layer of the state
(define rest-of-state
  (lambda (state)
    (cons (cdr (top-layer state)) (cdr state))))

;other pairs in layer
(define rest-of-pairs cdr)

;returns a new binding pair with a var and val
(define new-binding-pair
  (lambda (var val)
    (cons var (list (box val)))))

;returns the var name of the first binding pair in the top layer of the state
(define top-first-var-name
  (lambda (state)
    (var-name (first-pair (top-layer state)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; Testing ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define test
  (lambda (num class-name)
    (interpret (string-append (string-append "tests/test" num) ".txt") class-name)))

;main interpreter call
(interpret testFile 'B)

(test "1" 'A) ;15
(test "2" 'A) ;12
(test "3" 'A) ;125
(test "4" 'A) ;36
(test "5" 'A) ;54
(test "6" 'A) ;110
(test "7" 'C) ;26
(test "8" 'Square) ;117
(test "9" 'Square) ;32
(test "10" 'List) ;15
(test "11" 'List) ;123456
(test "12" 'List) ;5285
(test "13" 'C) ;-716
