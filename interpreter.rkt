;;;; ***************************************************
;;;; Group 15
;;;; Tyler Avery (tma58)
;;;; Thomas Bornhorst (thb34)
;;;; CSDS 345 Spring 2023
;;;; Simple Language Interpreter Project, Part 2
;;;; ***************************************************

#lang racket
(require "functionParser.rkt")

(define testFile "mainTest.txt")
(define testFile2 "tests/test6.txt")

(parser testFile2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;main interpreter functionality;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;main interpret call that parses file and calls the main interpreter functionality
(define interpret
  (lambda (filename)
    (interpreter-main (parser filename) (state-init))))

(define error-break (lambda(v) (error 'breakoutsideloop "Break statement not inside while loop")))
(define error-continue (lambda (v) (error 'continueoutsideloop  "Continue statement not inside while loop")))
(define error-throw (lambda (v) (error 'throwwithoutcatch "Throw statement without catch")))

;starts interpreter
(define interpreter-main
  (lambda (tree state)
    (state-get-definitions tree state (lambda (s) (interpreter-start (closure-function-body (value-get-var 'main s)) (state-add-layer s))))))

;outer layer - global variable declarations & function definitions
(define state-get-definitions
  (lambda (tree state return)
    (cond
      [(null? tree) (return state)]
      [(null? (first-stmt tree)) (return state)]
      [(eq? (operator (first-stmt tree)) 'function) (state-get-definitions (other-stmts tree) (state-define-function (first-stmt tree) state) return)]
      [(eq? (operator (first-stmt tree)) 'var) (state-get-definitions (other-stmts tree) (state-declaration-global (first-stmt tree) state) return)]
      [else (state-get-definitions (other-stmts tree) state return)])))

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
      [(eq? (operator (first-stmt tree)) 'var) (interpret-statement (other-stmts tree) (state-declaration (first-stmt tree) state) main-return break continue throw)]
      [(eq? (operator (first-stmt tree)) 'funcall) (state-function-call (first-stmt tree) state (lambda (v) (interpret-statement (other-stmts tree) state main-return break continue throw)))]
      [(eq? (operator (first-stmt tree)) 'if) (interpret-statement (other-stmts tree) (state-if (first-stmt tree) state main-return break continue throw) main-return break continue throw)]
      [(eq? (operator (first-stmt tree)) 'while) (interpret-statement (other-stmts tree) (call/cc (lambda (new-break)
                                                            (state-while (first-stmt tree) state main-return new-break continue throw))) main-return break continue throw)]
      [(eq? (operator (first-stmt tree)) 'return) (return-exit (first-stmt tree) state main-return)]
      [(eq? (operator (first-stmt tree)) '=) (interpret-statement (other-stmts tree) (state-assignmnet (first-stmt tree) state) main-return break continue throw)]
      [(eq? (operator (first-stmt tree)) 'begin) (interpret-statement (other-stmts tree) (state-pop-layer (interpret-statement (other-stmts (first-stmt tree)) (state-add-layer state) main-return break continue throw)) main-return break continue throw)]
      [(eq? (operator (first-stmt tree)) 'break) (break (state-pop-layer state))]
      [(eq? (operator (first-stmt tree)) 'continue) (continue state)]
      [(eq? (operator (first-stmt tree)) 'try) (interpret-statement (other-stmts tree) (state-try (first-stmt tree) state main-return break continue throw) main-return break continue throw)]
      [(eq? (operator (first-stmt tree)) 'throw) (throw (state-var-declaration 'throw (first-operand (first-stmt tree)) state))]
      [(eq? (operator (first-stmt tree)) 'finally) (interpret-statement (cadr (first-stmt tree)) state main-return break continue throw)]
      [else (error 'norelop "No relevant statements found")])))

(define function-name cadr)
(define formal-parameters caddr)
(define function-body cadddr)

(define state-define-function
  (lambda (stmt state)
    (state-var-declaration (function-name stmt) (value-make-closure stmt state) state)))

(define value-make-closure
  (lambda (stmt state)
    (cons (formal-parameters stmt) (cons (function-body stmt) (list (lambda (current-state) (state-function-environment (function-name stmt) current-state)))))))

;everything on define-state and lower (check for name of function)
(define state-function-environment
  (lambda (function-name call-state)
    (if (var-in-layer? function-name (top-layer call-state)) call-state (state-function-environment function-name (cdr call-state)))))

(define var-in-layer?
  (lambda (var layer)
    (cond
      [(null? layer) #f]
      [(eq? var (car (car layer))) #t]
      [else (var-in-layer? var (cdr layer))])))

(define function-call-name cadr)

;only modifications to the state will be through global variables so don't need to actually return the state
(define state-function-call
  (lambda (stmt state return)
    (return (value-function-call stmt state))))

;get closure then call function to deal with processing the rest of the function
(define value-function-call
  (lambda (stmt state)
    (value-function-call-with-closure stmt (value-get-var (function-call-name stmt) state) state)))

(define actual-parameters cddr)
(define closure-formal-parameters car)
(define closure-function-body cadr)
(define get-function-environment caddr)

;Get fstate -> bind parameters -> run function body
;TODO: Fix that #t will become 'true here on return?
(define value-function-call-with-closure
  (lambda (stmt closure state)
    (call/cc
     (lambda (ret)
    (interpret-statement (closure-function-body closure) (state-bind-parameters (closure-formal-parameters closure) (actual-parameters stmt) (state-add-layer ((get-function-environment closure) state)) state) ret error-break error-continue error-throw)))))

;bind actual parameters to formal parameters
(define state-bind-parameters
  (lambda (formal-parameters actual-parameters fstate state)
    (if (null? actual-parameters) fstate
        (value-process-expression (car actual-parameters) state (lambda (v) (state-bind-parameters (cdr formal-parameters) (cdr actual-parameters) (state-var-declaration (car formal-parameters) v fstate) state))))))

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
    (if (value-check-condition stmt state)
        (interpret-statement (list (second-operand stmt)) state main-return break continue throw)
        (if (not (null? (third-operand stmt)))
            (interpret-statement (list (third-operand stmt)) state main-return break continue throw)
            state))))

;implementation of a while loop
(define state-while
  (lambda (stmt state main-return break continue throw)
    (if (value-check-condition stmt state)
        (state-while stmt (call/cc (lambda (new-continue) (interpret-statement (list (second-operand stmt)) state main-return break new-continue throw))) main-return break continue throw)
        state)))

;function that defines variables as used in the interpreter
(define state-declaration
  (lambda (expr state)
    (value-process-expression (decl-right-operand expr) state (lambda (v) (state-var-declaration (left-operand expr) v state)))))

(define state-declaration-global
  (lambda (expr state)
    (value-process-expression (decl-right-operand expr) state (lambda (v) (state-var-declaration-global (left-operand expr) v state)))))

;sets the value of a variable
(define state-assignmnet
  (lambda (expr state)
    (value-process-expression (right-operand expr) state (lambda (v) (state-update-var (left-operand expr) v state)))))

;returns the result of an expression
(define return-exit
  (lambda (expr state return)
    (value-process-expression (first-operand expr) state (lambda (v) (return (value-convert-return v))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;variables - functions that directly interact with the state;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;checks whether a given variable is declared
(define var-is-declared?
  (lambda (var state)
    (cond
      [(null? state) #f]
      [(null? (top-layer state)) (var-is-declared? var (cdr state))]
      [(eq? var (top-first-var-name state)) #t] ; car state = top-layer, car top-layer = first-binding-pair, car first-binding-pair = var name
      [else (var-is-declared? var (rest-of-state state))])))

;declares a variable
(define state-var-declaration-global
  (lambda (var val state)
    (if (var-is-declared? var state)
        (error 'vardeclaredtwice "Variable already declared")
        (cons (cons (new-binding-pair var val) (top-layer state)) (cdr state)))))

(define state-var-declaration
  (lambda (var val state)
    (if (var-is-declared-not-global? var state)
        (error 'vardeclaredtwice "Variable already declared")
        (cons (cons (new-binding-pair var val) (top-layer state)) (cdr state)))))

(define var-is-declared-not-global?
  (lambda (var state)
    (cond
      [(null? state) #f]
      [(null? (cdr state)) #f] ; big change from normal one
      [(null? (top-layer state)) (var-is-declared-not-global? var (cdr state))]
      [(eq? var (top-first-var-name state)) #t] ; car state = top-layer, car top-layer = first-binding-pair, car first-binding-pair = var name
      [else (var-is-declared-not-global? var (rest-of-state state))])))

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

;proccesses a given expression
(define value-process-expression
  (lambda (expr state return)
    (cond
      [(null? expr) (return '())]
      [(not (list? expr)) (return (value-single-expr expr state))] ; if just a value
      [(eq? (operator expr) 'funcall) (return (value-function-call expr state))]
      [(is-one-operand? expr) (value-single-operand-expr expr state return)] ; if only one operand
      [(list? (left-operand expr)) (if (list? (right-operand expr)) ; if left-operand is a list (nested expression), recurse through it - if right-operand is a list, recurse through it
                                       (value-process-expression (left-operand expr) state
                                                                 (lambda (v1) (value-process-expression (right-operand expr) state
                                                                                                        (lambda (v2) (value-process-expression
                                                                                                                      (value-assemble-expression (operator expr) v1 v2) state return))))) ; both operands are lists
                                      (value-process-expression (left-operand expr) state
                                                                (lambda (v1) (value-process-expression (value-assemble-expression (operator expr) v1 (right-operand expr)) state return))))] ; only left-operand is a list
      [(list? (right-operand expr)) (value-process-expression (right-operand expr) state
                                                              (lambda (v2) (value-process-expression (value-assemble-expression (operator expr) (left-operand expr) v2) state return)))] ; only right-operand is a list
      [else (return (value-integer-operations (value-assemble-expression (operator expr) ; (essentially the base case) if neither are lists, make sure each operand is a value and pass to integer/conditional operations functions
                                                                         (value-single-expr (left-operand expr) state)
                                                                         (value-single-expr (right-operand expr) state)) state))])))

;processes the result of an expression with one operand
(define value-single-operand-expr
  (lambda (expr state return)
    (cond
      [(eq? (operator expr) '!) (value-process-expression (first-operand expr) state (lambda (v) (return (not v))))]
      [(eq? (operator expr) '-) (value-process-expression (first-operand expr) state (lambda (v) (return (* v -1))))])))

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

;abstraction for if and while functions
(define value-check-condition
  (lambda (stmt state)
    (value-check-condition-errors (value-process-expression (first-operand stmt) state (lambda (v) v)))))

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
  (lambda (num)
    (interpret (string-append (string-append "tests/test" num) ".txt"))))

;main interpreter call
;(interpret testFile)
#||#
(test "1") ;10
(test "2") ;14
(test "3") ;45
(test "4") ;55
(test "5") ;1
(test "6") ;115
(test "7") ;true
(test "8") ;20
(test "9") ;24
(test "10") ;2
(test "11") ;35
(test "12") ;error - custom error message?
(test "13") ;90
(test "14") ;69
(test "15") ;87
(test "16") ;64
(test "17") ;error
(test "18") ;125
(test "19") ;100
(test "20") ;2000400
