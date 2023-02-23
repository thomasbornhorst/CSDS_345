; Group 27: Tyler Avery, Thomas Bornhorst

#lang racket
(require "simpleParser.rkt")

(define testFile "mainTest.txt")

(parser testFile)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;main parser functionality;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;define behavior for various keywords
(define parserMain
  (lambda (tree state)
    (cond
      [(null? tree) state]
      [(eq? (operator (car tree)) 'var) (variableType (car tree) state (lambda (s) (parserMain (cdr tree) s)))]
      [(eq? (operator (car tree)) 'if) (ifStatement (car tree) state (lambda (s) (parserMain (cdr tree) s)))]
      [(eq? (operator (car tree)) 'while) (whileLoop (car tree) state (lambda (s) (parserMain (cdr tree) s)))]
      [(eq? (operator (car tree)) 'return) (returnOutput (car tree) state)]
      [(eq? (operator (car tree)) '=) (variableValue (car tree) state (lambda (s) (parserMain (cdr tree) s)))]
      [else (error 'norelop "No relevant statements found")])))

;implementation of an if statement
(define ifStatement
  (lambda (stmt state return)
    (if (processExpression (leftoperand stmt) state (lambda (v) v))
        (return (parserMain (list (rightoperand stmt)) state))
        (if (not (null? (thirdoperand stmt)))
            (return (parserMain (list (thirdoperand stmt)) state))
            (return state)))))

;implementation of a while loop
(define whileLoop
  (lambda (stmt state return)
    (if (processExpression (leftoperand stmt) state (lambda (v) v))
        (whileLoop stmt (parserMain (list (rightoperand stmt)) state) return) (return state))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;variables;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;function that defines variables as used in the interpreter
(define variableType
  (lambda (expr state return)
    (processExpression (declrightoperand expr) state (lambda (v) (return (declareVar (leftoperand expr) v state))))))

;sets the value of a variable
(define variableValue
  (lambda (expr state return)
    (processExpression (rightoperand expr) state (lambda (v) (return (updateVar (leftoperand expr) v state))))))

;checks whether a given variable is declared
(define isDeclared
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((eq? var (car(car state))) #t)
      (else (isDeclared var (cdr state))))))

;declares a variable
(define declareVar
  (lambda (var val state)
    (if (isDeclared var state)
        (error 'vardeclaredtwice "Variable already declared")
        (cons (cons var (list val)) state))))

;updates the value of an already defined variable
(define updateVar
  (lambda (var val state)
    (updateVarCPS var val state (lambda (v) v))))

;gets value of variable
(define getVal
  (lambda (var state)
    (cond
      [(null? state) (error 'varnotdeclared "Variable not yet declared")]
      [(eq? var (car(car state))) (cadr (car state))]
      [else (getVal var (cdr state))])))

;updates the value of an already defined variable with CPS
(define updateVarCPS
  (lambda (var val state return)
    (cond
      [(null? state) (error 'varnotdeclared "Variable not yet declared")]
      [(eq? var (car (car state))) (return (cons (cons var (list val)) (cdr state)))]
      [else (updateVarCPS var val (cdr state) (lambda (v) (cons (car state) v)))])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;symbol definitions;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;defines behavior for integer operations
(define integerType
  (lambda (expr state)
    (cond
      [(or (null? (leftoperand expr)) (null? (rightoperand expr))) (error 'varnotassgn "Variable not yet assigned")]
      [(eq? (operator expr) '+) (+         (leftoperand expr) (rightoperand expr))]
      [(eq? (operator expr) '-) (-         (leftoperand expr) (rightoperand expr))]
      [(eq? (operator expr) '*) (*         (leftoperand expr) (rightoperand expr))]
      [(eq? (operator expr) '/) (quotient  (leftoperand expr) (rightoperand expr))]
      [(eq? (operator expr) '%) (remainder (leftoperand expr) (rightoperand expr))]
      [else (conditionalOperator expr state)])))

;defines behavior for conditional operators
;! op included in processExpression checks
(define conditionalOperator
  (lambda (condition state)
    (cond
      ((eq? (operator condition) '==) (eq?    (leftoperand condition) (rightoperand condition)))
      ((eq? (operator condition) '!=) (not (eq?    (leftoperand condition) (rightoperand condition))))
      ((eq? (operator condition) '<) (<    (leftoperand condition) (rightoperand condition)))
      ((eq? (operator condition) '>) (>    (leftoperand condition) (rightoperand condition)))
      ((eq? (operator condition) '<=) (<=    (leftoperand condition) (rightoperand condition)))
      ((eq? (operator condition) '>=) (>=    (leftoperand condition) (rightoperand condition)))
      ((eq? (operator condition) '&&) (and    (leftoperand condition) (rightoperand condition)))
      ((eq? (operator condition) '||) (or    (leftoperand condition) (rightoperand condition)))
      (else (error 'unknownop "Bad Operator")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;expression processing;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;returns the result of an expression
(define returnOutput
  (lambda (expr state)
    (processExpression (leftoperand expr) state (lambda (v) (convertReturn v)))))

;returns 'true' and 'false' rather than '#t' and '#f"
(define convertReturn
  (lambda (val)
    (cond
      [(eq? val #t) 'true]
      [(eq? val #f) 'false]
      [else val])))

;proccesses a given expression
(define processExpression
  (lambda (expr state return)
    (cond
      [(null? expr) (return '())]
      [(not (list? expr)) (return (singleExprVal expr state))]
      [(isOneOperand? expr) (singleOperandExpr expr state return)]
      [(list? (leftoperand expr)) (if (list? (rightoperand expr)) (processExpression (leftoperand expr) state (lambda (v1) (processExpression (rightoperand expr) state (lambda (v2) (processExpression (assembleExpression (operator expr) v1 v2) state return))))) ; change this lambda to just "return"?
                                      (processExpression (leftoperand expr) state (lambda (v1) (processExpression (assembleExpression (operator expr) v1 (rightoperand expr)) state return))))]
      [(list? (rightoperand expr)) (processExpression (rightoperand expr) state (lambda (v2) (processExpression (assembleExpression (operator expr) (leftoperand expr) v2) state return)))]
      [else (return (integerType (assembleExpression (operator expr) (singleExprVal (leftoperand expr) state) (singleExprVal (rightoperand expr) state)) state))])))

;processes the result of an expression with one operand
(define singleOperandExpr
  (lambda (expr state return)
    (cond
      [(eq? (operator expr) '!) (processExpression (leftoperand expr) state (lambda (v) (return (not v))))]
      [(eq? (operator expr) '-) (processExpression (leftoperand expr) state (lambda (v) (return (* v -1))))])))

;returns the value of an expression with one element
(define singleExprVal
  (lambda (expr state)
    (cond
      [(eq? expr 'false) #f]
      [(eq? expr 'true) #t]
      [(eq? expr #f) #f]
      [(eq? expr #t) #t]
      [(number? expr) expr]
      [else (getVal expr state)])))

;helper functions to abstract the details and get the denotational code above to read better
(define operator
  (lambda (expression)
    (car expression)))

;defines the left operand as the cadr of the expression
(define leftoperand cadr)

;define rightoperand as the caddr of the expression
(define rightoperand caddr)

;define thirdoperand
(define thirdoperand
  (lambda (stmt)
    (if (null? (cdr(cdr(cdr stmt)))) '()
        (cadddr stmt))))

;declares the right operand of the expression
(define declrightoperand
  (lambda (expression)
    (if (null? (cdr (cdr expression)))
        '()
        (rightoperand expression))))

;checks whether an expression has one operand
(define isOneOperand?
  (lambda (expr)
    (null? (cdr (cdr expr)))))

;combines combines the parts of an expression into the "state" format
(define assembleExpression
  (lambda (operator leftoperand rightoperand)
    (cons operator (cons leftoperand (list rightoperand)))))

(parserMain (parser testFile) '())