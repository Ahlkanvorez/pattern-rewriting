;;;
;; Author: Robert Mitchell <robert.mitchell36@gmail.com>
;; Edit log:
;; - 1/24/2015
;; - 10/24/2016 (added comment format; added comments)
;;;

;;;; Comment format:
;;    ;;;; Input signature -> output signature
;;    ;; Description of function
;;    ;; Example-usage (optional name of testing function)
;;    ;;  ;; > (example-invocation)
;;    ;;  ;; actual-output from that invocation.
;;    (optional testing function definition prior to actual function definition)

;; Dependencies

#lang racket

(require "lisp.rkt")

(provide (all-defined-out))

;; Expression evaluation

;;;;
;; TODO: Comment
;;;
(define (exp-eval functions variables exp)
  (cond ((number? exp) exp)
        ((symbol? exp) (cadr (assoc exp variables)))
        (else
         (let ((left (if (not (null? (lhs exp)))
                         (exp-eval functions variables (lhs exp))
                         `()))
               (right (if (not (null? (rhs exp)))
                          (exp-eval functions variables (rhs exp))
                          `()))
               (operator (cadr (assoc (list (car exp) 'u) functions))))
           (cond ((null? left)  (operator right))
                 ((null? right) (operator left))
                 (else (operator left right)))))))

;;;;
;; TODO: Comment
;;;
(define (exp-easy-eval variables exp)
  (exp-eval (append basic-operators
                    basic-functions
                    trig-functions
                    vector-operators)
            variables
            exp))

;; TODO: Comment
(define basic-operators
  (list
   (list '+ 	(lambda (x y)	(+ x y)))
   (list '- 	(lambda (x y)	(- x y)))
   (list '(- u)	(lambda (x)	(- x)))
   (list '* 	(lambda (x y)	(* x y)))
   (list '/ 	(lambda (x y)	(/ x y)))
   ))

;; TODO: Comment
(define basic-functions
  (list
   (list '(sqrt u)	(lambda (x)	(sqrt x)))
   (list '(square u)	(lambda (x)	(* x x)))
   (list '(exp u)	(lambda (x)	(exp x)))
   (list 'expt		(lambda (x y)	(expt x y)))
   (list '(log u)	(lambda (x) 	(log x)))
   ))

;; TODO: Comment
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

;; TODO: Comment
(define vector-operators
  (list
   ; TODO: Add support for more functions -- including curvature.
   ))

;; TODO: Comment
(define numerical-differentiation
  (list
   ))
