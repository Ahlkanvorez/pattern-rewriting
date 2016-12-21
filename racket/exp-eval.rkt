;;;
;; Author: Robert Mitchell <robert.mitchell36@gmail.com>
;; Edit log:
;; - 1/24/2015
;; - 10/24/2016 (added comment format; added comments)
;;;

;;;; Comment format:
;;    ;;;; Input signature -> output signature
;;    ;; Description of function
;;    ;;; Example-usage (optional name of testing function)
;;    ;;  ;; > (example-invocation)
;;    ;;  ;; actual-output from that invocation.
;;    (optional testing function definition prior to actual function definition)

;; Dependencies

#lang racket

(require "lisp.rkt")

(provide (all-defined-out))

;; Expression evaluation

;;;; '((k1 lambda1) (k2 lambda2) ...) '((k1 v1) (k2 v2) ...) '(an expression) -> a scalar.
;; This function takes an association list of function names (if they take two operands),
;; or function patterns (if they take one operand), which map to lambdas that perform the
;; represented operation. Additionally, this function takes an assoc list of variable names
;; to their values, and an expression to evaluate using the above information.
;;; Example-usage (exp-eval-test)
;;  ;; > (exp-eval (append basic-operators
;;  ;;                     basic-functions
;;  ;;                     trig-functions)
;;  ;;             '((a 25) (b 50) (c 3))
;;  ;;             '(/ (+ (* (- c) (sin a)) (sqrt b)) 5))
  (define (exp-eval-test)
    (let ((result (exp-eval (append basic-operators
                                    basic-functions
                                    trig-functions)
                            '((a 25) (b 50) (c 3))
                            '(/ (+ (* (- c) (sin a)) (sqrt b)) 5)))
          (correctAnswer 1.493624612431759))
      (if (equal? result correctAnswer)
          #t
          (list 'got result 'expected correctAnswer))))
(define (exp-eval functions variables exp)
  (cond ((null? exp) exp)
        ((number? exp) exp)
        ((symbol? exp) (cadr (assoc exp variables)))
        ((list? exp)
         (let ((left (if (not (null? (lhs exp)))
                         (exp-eval functions variables (lhs exp))
                         `()))
               (right (if (not (null? (rhs exp)))
                          (exp-eval functions variables (rhs exp))
                          `())))
           (cond ((null? left)  ((cadr (assoc (list (car exp) 'u) functions)) right))
                 ((null? right) ((cadr (assoc (list (car exp) 'u) functions)) left))
                 (else          ((cadr (assoc (car exp) functions)) left right)))))
        (else `())))

;;;; '((k1 v1) (k2 v2) ...) '(an expression) -> a scalar value.
;; This function takes an association list of variable symbols with values, and an
;; expression, then evaluates that expression using the following rewrite rules all
;; defined in this file: basic-operators, basic-functions, trig-functions. The evaluation
;; is done by calling exp-eval with the aforementioned operands. For more information,
;; see the documentation for exp-eval.
;;; Example-usage (exp-easy-eval-test)
;;  ;; > (exp-easy-eval '((a 25) (b 50) (c 3))
;;  ;;                  '(/ (+ (* (- c) (sin a)) (sqrt b)) 5)
;;  ;; 1.493624612431759
  (define (exp-easy-eval-test)
    (let ((result (exp-easy-eval '((a 25) (b 50) (c 3))
                                 '(/ (+ (* (- c) (sin a)) (sqrt b)) 5)))
          (correctAnswer 1.493624612431759))
      (if (equal? result correctAnswer)
          #t
          (list 'got result 'expected correctAnswer))))
(define (exp-easy-eval variables exp)
  (exp-eval (append basic-operators
                    basic-functions
                    trig-functions)
            variables
            exp))

;; Rewrite rules to replace a symbolic version of a basic arithmetic operation, such as
;; addition, negation, etc, with a lambda performing the represented function.
(define basic-operators
  (list
   (list '+ 	(lambda (x y)	(+ x y)))
   (list '-	(lambda (x y)	(- x y)))
   (list '(- u) (lambda (x)	(- x)))
   (list '* 	(lambda (x y)	(* x y)))
   (list '/ 	(lambda (x y)	(/ x y)))
   ))

;; Rewrite rules to replace a symbolic version of a basic function, such as power functions,
;; exponential functions, and logarithmic functions, with a lambda performing the
;; represented function.
(define basic-functions
  (list
   (list '(sqrt u)	(lambda (x)	(sqrt x)))
   (list '(square u)	(lambda (x)	(* x x)))
   (list '(exp u)	(lambda (x)	(exp x)))
   (list 'expt	        (lambda (x y)	(expt x y)))
   (list '(log u)	(lambda (x) 	(log x)))
   ))

;; Rewrite rules to replace a symbolic version of a trigonometric function with a
;; lambda performing the represented function.
(define trig-functions
  (list
   (list '(cos u)	(lambda (x) (cos x)))
   (list '(sin u)	(lambda (x) (sin x)))
   (list '(tan u)	(lambda (x) (tan x)))
   (list '(sec u)	(lambda (x) (/ 1.0 (cos x))))
   (list '(csc u)	(lambda (x) (/ 1.0 (sin x))))
   (list '(cot u)	(lambda (x) (/ 1.0 (tan x))))
   (list '(versin u)	(lambda (x) (- 1 (cos x))))
   (list '(vercosin u)	(lambda (x) (+ 1 (cos x))))
   (list '(coversin u)	(lambda (x) (- 1 (sin x))))
   (list '(covercos u)	(lambda (x) (+ 1 (sin x))))
   ))
