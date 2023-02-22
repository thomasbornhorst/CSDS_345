#lang racket
(require "simpleParser.rkt")

(define testFile "mainTest.txt")

(parser testFile)

; TODO: Add if and while - To do that, first need to add Mconditional
; 'true vs #t

(define parserMain
  (lambda (tree state)
    (cond
      ((null? tree) state)
      ((eq? (car (car tree)) 'var) (Mvar (car tree) state (lambda (s) (parserMain (cdr tree) s))))
      ((eq? (car (car tree)) 'if) (Mif (car tree) state (lambda (s) (parserMain (cdr tree) s))))
      ((eq? (car (car tree)) 'while) (Mwhile (car tree) state (lambda (s) (parserMain (cdr tree) s))))
      ((eq? (car (car tree)) 'return) (Mreturn (car tree) state))
      ((eq? (car (car tree)) '=) (Mval (car tree) state (lambda (s) (parserMain (cdr tree) s))))
      (else (error 'norelop "No relevant operations found")))))

 ; the following comparison operators are implemented: ==, !=, <, >, <=. >=, and the following boolean operators: &&, ||, !

(define Mvar
  (lambda (expr state return)
    (Mexpr (declrightoperand expr) state (lambda (v) (return (declareVar (leftoperand expr) v state))))))

(define Mval
  (lambda (expr state return)
    (if (eq? (operator expr) '=)
         (Mexpr (rightoperand expr) state (lambda (v) (return (updateVar (leftoperand expr) v state))))
        (return state))))

(define Mreturn
  (lambda (expr state)
    (cond
      ((list? (leftoperand expr)) (Mexpr (leftoperand expr) state (lambda (v) v)))
      ((number? (leftoperand expr)) (leftoperand expr))
      (else (getVal (leftoperand expr) state)))))

(define Mif
  (lambda (stmt state return)
    (if (Mexpr (leftoperand stmt) state (lambda (v) v))
        (return (parserMain (list (rightoperand stmt)) state))
        (if (not (null? (thirdoperand stmt)))
            (return (parserMain (list (thirdoperand stmt)) state))
            (return state)))))

(define Mwhile
  (lambda (stmt state return)
    (if (Mexpr (leftoperand stmt) state (lambda (v) v))
        (Mwhile stmt (parserMain (list (rightoperand stmt)) state) return) (return state))))

(define Mexpr
  (lambda (expr state return)
    (cond
      [(null? expr) (return '())]
      [(not (list? expr)) (return (singleExprVal expr state))]
      [(eq? (operator expr) '!) (return (not (Mexpr (leftoperand expr) state)))]
      [(list? (leftoperand expr)) (if (list? (rightoperand expr)) (Mexpr (leftoperand expr) state (lambda (v1) (Mexpr (rightoperand expr) state (lambda (v2) (Mexpr (combineExprParts (operator expr) v1 v2) state return))))) ; change this lambda to just "return"?
                                      (Mexpr (leftoperand expr) state (lambda (v1) (Mexpr (combineExprParts (operator expr) v1 (rightoperand expr)) state return))))]
      [(list? (rightoperand expr)) (Mexpr (rightoperand expr) state (lambda (v2) (Mexpr (combineExprParts (operator expr) (leftoperand expr) v2) state return)))]
      [else (return (Minteger (combineExprParts (operator expr) (singleExprVal (leftoperand expr) state) (singleExprVal (rightoperand expr) state)) state))])))

(define singleExprVal
  (lambda (expr state)
    (cond
      [(eq? expr 'false) #f]
      [(eq? expr 'true) #t]
      [(eq? expr #f) #f]
      [(eq? expr #t) #t]
      [(number? expr) expr]
      [else (getVal expr state)])))

; Actually, the interpreter will be in prefix form
; (* (+ 4 5) (- 10 6))
(define Minteger
  (lambda (expr state)
    (cond
      ((eq? (operator expr) '+) (+         (leftoperand expr) (rightoperand expr)))
      ((eq? (operator expr) '-) (-         (leftoperand expr) (rightoperand expr)))
      ((eq? (operator expr) '*) (*         (leftoperand expr) (rightoperand expr)))
      ((eq? (operator expr) '/) (quotient  (leftoperand expr) (rightoperand expr)))
      ((eq? (operator expr) '%) (remainder (leftoperand expr) (rightoperand expr)))
      (else (Mcondition expr state)))))

; TODO: figure out issue with &&
; ! op included in Mexpr checks
(define Mcondition
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

; helper functions to abstract the details and get the denotational code above to read better
(define operator
  (lambda (expression)
    (car expression)))

(define leftoperand cadr)

(define rightoperand caddr)

(define thirdoperand
  (lambda (stmt)
    (if (null? (cdr(cdr(cdr stmt)))) '()
        (cadddr stmt))))

(define declrightoperand
  (lambda (expression)
    (if (null? (cdr (cdr expression)))
        '()
        (rightoperand expression))))

(define combineExprParts
  (lambda (operator leftoperand rightoperand)
    (cons operator (cons leftoperand (list rightoperand)))))

(define isDeclared?
  (lambda (var state)
    (cond
      ((null? state) #f)
      ((eq? var (car(car state))) #t)
      (else (isDeclared? var (cdr state))))))

(define declareVar
  (lambda (var val state)
    (cons (cons var (list val)) state)))

(define newStateEntry ; Fix? or just don't use?
  (lambda (var val)
    ((cons var (list val)))))

(define updateVar
  (lambda (var val state)
    (updateVarCPS var val state (lambda (v) v))))

(define getVal
  (lambda (var state)
    (cond
      ((null? state) (error 'noval "No value found")) ; add name of var to error stmt?
      ((eq? var (car(car state))) (cadr (car state)))
      (else (getVal var (cdr state))))))

(define updateVarCPS
  (lambda (var val state return)
    (cond
      ((null? state) (error 'varnotdeclared "Variable not yet declared"))
      ((eq? var (car (car state))) (return (cons (cons var (list val)) (cdr state)))) ; abstract the var eq part?
      (else (updateVarCPS var val (cdr state) (lambda (v) (cons (car state) v)))))))

(define returnVal ; implement this as just calling getVal? - remove this method entirely and just call getVal from parserMain?
  (lambda (state)
    (cond
      ((null? state) (error 'noreturnstmnt "No return value found"))
      ((eq? 'return (car(car state))) (cadr (car state)))
      (else (returnVal (cdr state))))))

(parserMain (parser testFile) '())