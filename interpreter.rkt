;;;; ***************************************************
;;;; Group 15
;;;; Tyler Avery (tma58)
;;;; Thomas Bornhorst (thb34)
;;;; CSDS 345 Spring 2023
;;;; Simple Language Interpreter Project, Part 2
;;;; ***************************************************

#lang racket
(require "simpleParser.rkt")

(define testFile "mainTest.txt")
(define testFile2 "tests/test17.txt")

(parser testFile2)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;main interpreter functionality;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define interpreter-main
  (lambda (tree state)
    (call/cc
     (lambda (ret)
       (interpret-statement tree state ret (lambda(v) (error 'breakoutsideloop "Break statement not inside while loop")) (lambda (v) (error 'continueoutsideloop  "Continue statement not inside while loop")) (lambda (v) (error 'throwwithoutcatch "Throw statement without catch")))))))

;define behavior for various keywords of statements
(define interpret-statement
  (lambda (tree state main-return break continue throw)
    (cond
      [(null? tree) state]
      [(eq? (operator (first-stmt tree)) 'var) (state-declaration (first-stmt tree) state (lambda (s) (interpret-statement (other-stmts tree) s main-return break continue throw)))]
      [(eq? (operator (first-stmt tree)) 'if) (state-if (first-stmt tree) state main-return break continue throw (lambda (s) (interpret-statement (other-stmts tree) s main-return break continue throw)))]
      [(eq? (operator (first-stmt tree)) 'while) (interpret-statement (other-stmts tree) (call/cc (lambda (new-break)
                                                            (state-while (first-stmt tree) state main-return new-break continue throw))) main-return break continue throw)]
      [(eq? (operator (first-stmt tree)) 'return) (return-exit (first-stmt tree) state main-return)]
      [(eq? (operator (first-stmt tree)) '=) (state-assignmnet (first-stmt tree) state (lambda (s) (interpret-statement (other-stmts tree) s main-return break continue throw)))]
      [(eq? (operator (first-stmt tree)) 'begin) (interpret-statement (other-stmts tree) (state-pop-layer (interpret-statement (other-stmts (first-stmt tree)) (state-add-layer state) main-return break continue throw)) main-return break continue throw)]
      [(eq? (operator (first-stmt tree)) 'break) (break (state-pop-layer state))]
      [(eq? (operator (first-stmt tree)) 'continue) (continue state)]
      [(eq? (operator (first-stmt tree)) 'try) (state-try (first-stmt tree) state main-return break continue throw (lambda (s) (interpret-statement (other-stmts tree) s main-return break continue throw)))]
      [(eq? (operator (first-stmt tree)) 'throw) (throw (first-operand (first-stmt tree)))]
      [(eq? (operator (first-stmt tree)) 'catch) (state-assignmnet (first-stmt tree) state (lambda (s) (interpret-statement (other-stmts tree) s main-return break continue throw)))]
      [(eq? (operator (first-stmt tree)) 'finally) (state-assignmnet (first-stmt tree) state (lambda (s) (interpret-statement (other-stmts tree) s main-return break continue throw)))]
      [else (error 'norelop "No relevant statements found")])))

(define try-stmt cadr)
(define catch-stmt caddr)
(define full-finally-stmt cadddr)
(define finally-stmt
  (lambda (stmt)
    (cadr (cadddr stmt))))

(define state-try
  (lambda (stmt state main-return break continue throw return)
    ((state-try-helper stmt state main-return break continue throw (lambda (s) (if (null? (full-finally-stmt stmt)) (return s) (return (interpret-statement (finally-stmt stmt) s main-return break continue throw))))))))

(define state-try-helper
  (lambda (stmt state main-return break continue throw return)
    (return (state-catch (catch-stmt stmt) (call/cc (lambda (new-throw) (return (interpret-statement (try-stmt stmt) (state-add-layer state) main-return break continue new-throw)))) state main-return break continue throw))))

(define state-catch
  (lambda (stmt e state main-return break continue throw)
    (interpret-statement (caddr stmt) (state-var-declaration (car (cadr stmt)) e state) main-return break continue throw)))

;implementation of an if statement
(define state-if
  (lambda (stmt state main-return break continue throw return)
    (if (value-process-expression (first-operand stmt) state (lambda (v) v))
        (return (interpret-statement (list (second-operand stmt)) state main-return break continue throw))
        (if (not (null? (third-operand stmt)))
            (return (interpret-statement (list (third-operand stmt)) state main-return break continue throw))
            (return state)))))

;implementation of a while loop
(define state-while
  (lambda (stmt state main-return break continue throw)
    (state-while stmt (call/cc (lambda (new-continue) (state-while-cps stmt state main-return break new-continue throw (lambda (s) (break s))))) main-return break continue throw)))

;implementation of a while loop
(define state-while-cps
  (lambda (stmt state main-return break continue throw return)
    (if (value-process-expression (first-operand stmt) state (lambda (v) v))
        (state-while-cps stmt (interpret-statement (list (second-operand stmt)) state main-return break continue throw) main-return break continue throw return) (return state))))

;function that defines variables as used in the interpreter
(define state-declaration
  (lambda (expr state return)
    (value-process-expression (decl-right-operand expr) state (lambda (v) (return (state-var-declaration (left-operand expr) v state))))))

;sets the value of a variable
(define state-assignmnet
  (lambda (expr state return)
    (value-process-expression (right-operand expr) state (lambda (v) (return (state-update-var (left-operand expr) v state))))))

;returns the result of an expression
(define return-exit
  (lambda (expr state return)
    (value-process-expression (first-operand expr) state (lambda (v) (return (value-convert-return v))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;variables - functions that directly interact with the state;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define top-layer car)
(define first-pair car)
(define var-name car)
(define rest-of-state
  (lambda (state)
    (cons (cdr (top-layer state)) (cdr state))))
(define new-binding-pair
  (lambda (var val)
    (cons var (list val))))
(define top-first-var-name
  (lambda (state)
    (var-name (first-pair (top-layer state)))))

(define state-pop-layer
  (lambda (state)
    (cdr state)))
    

;checks whether a given variable is declared
(define var-is-declared?
  (lambda (var state)
    (cond
      [(null? state) #f]
      [(null? (top-layer state)) (var-is-declared? var (cdr state))]
      [(eq? var (top-first-var-name state)) #t] ; car state = top-layer, car top-layer = first-binding-pair, car first-binding-pair = var name
      [else (var-is-declared? var (rest-of-state state))])))

;declares a variable
(define state-var-declaration
  (lambda (var val state)
    (if (var-is-declared? var state)
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
      [(eq? var (top-first-var-name state)) (return (cons (cons (new-binding-pair var val) (cdr (top-layer state))) (cdr state)))]
      [else (state-update-var-CPS var val (rest-of-state state) (lambda (v) (return (cons (cons (first-pair (top-layer state)) (top-layer v)) (cdr v)))))])))

;gets value of variable
(define value-get-var
  (lambda (var state)
    (cond
      [(null? state) (error 'varnotdeclared "Variable not yet declared")]
      [(null? (top-layer state)) (value-get-var var (cdr state))]
      [(eq? var (top-first-var-name state)) (cadr (first-pair (top-layer state)))]
      [else (value-get-var var (rest-of-state state))])))

;return the starting state
(define state-init
  (lambda ()
    (list '())))

(define state-add-layer
  (lambda (state)
    (cons '() state)))

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
      [else val])))

;proccesses a given expression
(define value-process-expression
  (lambda (expr state return)
    (cond
      [(null? expr) (return '())]
      [(not (list? expr)) (return (value-single-expr expr state))] ; if just a value
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

;helper functions to abstract the details and get the denotational code above to read better
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

;main interpreter call
;(interpreter-main (parser testFile) (state-init))

;Testing

(define test
  (lambda (num)
    (interpreter-main (parser (string-append (string-append "tests/test" num) ".txt")) (state-init))))

#|
(test "1") ;20
(test "2") ;164
(test "3") ;32
(test "4") ;2
;(test "5") ;error
(test "6") ;25
(test "7") ;21
(test "8") ;6
(test "9") ;-1
(test "10") ;789
;(test "11") ;error
;(test "12") ;error
;(test "13") ;error
(test "14") ;12
|#
;(test "15") ;125
;(test "16") ;110
(test "17") ;2000400
(test "18") ;101
(test "19") ;error