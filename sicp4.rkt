#lang racket
(define (eval exp env)
  (cond((self-evaluating? exp)exp)
       ((variable? exp) (lookup-variable-value exp env))
       ((quoted? exp)(text-of-quotation exp))
       ((assignment? exp) (eval-assignment exp env))
       ((defination? exp) (eval-definition exp env))
       ((if? exp) (eval-if exp env))
       ((lambda? exp)
        (make-procedure (lamada-parameters exp)
                        (lambda-body exp)
                        env))
       ((bedin? exp)
        (eval-sequence (begin-actions exp)env))
       ((cond? exp) (eval(cond->if exp)env))
       ((application? exp)
        (apply(eval (operatptor exp)env)
              (list-of-values(operands exp)env)))
       (else
        (error "Unknow expession type -- EVAL" exp))))

(define(apply procedure arguments)
  (cond ((primitive-procedure? procedure arguments))
        ((apply-primitive-procedure procedure arguments))
        ((compounds-procedures? produre)
         (eval-sequence
          (procedure-body procedure)
          (extend-environment
           (procedure-parameters procedure)
           arguments
           (procedure-enviroment procedure))))
        (else
         (error
          "Unknow procedure type -- APPLY" procdure))))

(define (list-of-values exps env)
  (if(no-operands? exps)
     '()
     (conds (eval(first-operand exps)env)
            (list-of-values(rest-operands exps) env))))

(define (eval-if exp env)
  (if(true? (eval (if-predicate exp) env))
     (eval (if-condequent exp)env)
     (eval (if-alternative exp) env)))

(define (eval-assignment exp env)
  (set-variable-value!(assignment-variable exp)
                      (eval (assignment-value exp) env)
                      env)
  'ok)

(define (eval-definition exp env)
  (define-variable! (definition-variable exp)
    (eval(definition-value exp)env)
    env)
  'ok)