;;;
;; Author: Robert Mitchell <robert.mitchell36@gmail.com>
;; Edit log:
;; - 1/24/2015 (Prototype)
;; - 1/25/2015 (New algorithm - BFS)
;; - 1/27/2015 (Comments)
;; - 9/5/2015 (Simplified and partially evaluated answers from solve)
;; - 10/24/2016 (Corrected major bugs in 'simplify' and 'solve'; added comment
;;     format; added some comments)
;;;

;;;; Comment format:
;;    ;;;; Input signature -> output signature
;;    ;; Description of function
;;    ;;; Example-usage (optional name of testing function)
;;    ;;  ;; > (example-invocation)
;;    ;;  ;; actual-output from that invocation.
;;    (optional testing function definition prior to actual function definition)

#lang racket

(require "lisp.rkt")
(require "graphs.rkt")
(require "pat.rkt")
(require "exp-eval.rkt")

(provide (all-defined-out))

;; Symbolic algbra manipulation

;;;; (((a1 b1) (a2 b2)) exp1 var) -> exp2
;; Given an association list of rewrite rules, a quoted expression, and a quoted
;; variable to solve for, this function will return '() if it cannot find a
;; solution, or the solution in the form '(= var exp3) where exp3 is the quoted
;; expression that var is equal to.
;;; Example-Usage (solve-test)
;;  ;; > (solve (append rewrite-rules-functions
;;  ;;                  rewrite-rules-basic-algebra
;;  ;;                  rewrite-rules-solve-equation)
;;  ;;          '(= (exp (square (* 5 (log (sin x?))))) 42)
;;  ;;          'x?))
;;  ;;'((= (exp (square (* 5 (log (sin x?))))) 42)
;;  ;;  (= (square (* 5 (log (sin x?)))) (log 42))
;;  ;;  (= (* 5 (log (sin x?))) (sqrt (log 42)))
;;  ;;  (= (log (sin x?)) (/ (sqrt (log 42)) 5))
;;  ;;  (= (sin x?) (exp (/ (sqrt (log 42)) 5)))
;;  ;;  (= x? (arcsin (exp (/ (sqrt (log 42)) 5)))))
  (define (solve-test)
    (let ((result (solve (append rewrite-rules-functions
                                 rewrite-rules-basic-algebra
                                 rewrite-rules-solve-equation)
                         '(= (exp (square (* 5 (log (sin x?))))) 42)
                         'x?)))
      (if (eq? 'x? (lhs (last result)))
          #t
          result)))
(define (solve rules exp var)
  (define (either-solution? exp)
    (or (solution? (car exp))
        (solution? (flip (car exp)))))
  (define (solution? exp)
    (and (equal? #f (member var (eq-values (rhs exp))))
         (equal? var (lhs exp))))
  (define (extract-solution exp visited)
    (reverse
     (if (solution? (car exp))
         exp
         (cons (flip (car exp)) (cdr exp)))))
  (define (alter-expression exp)
    (define (alter exp)
      (define (alter-function x)
        (let ((altered (transform-part x exp)))
          (if (equal? exp altered)
              '()
              altered)))
      (filter (lambda (x) (not (null? x))) (map alter-function rules)))
    (define (add-path x)
      (cons x exp))
    (append (map add-path (alter (car exp)))
            (map add-path (alter (flip (car exp))))))
  (define (remember exp visited)
    (cons (car exp) visited))
  (define (unknown? known)
    (define (unknown-iter known exp)
      (cond ((null? known) #t)
            ((equal? (car known) exp) #f)
            (else (unknown-iter (cdr known) exp))))
    (lambda (x) (unknown-iter known x)))
  (bfs (list exp)
       either-solution?
       extract-solution
       unknown?
       alter-expression
       remember))

;;;; ((f1 f2 ... fN) (rule1 rule2 ... ruleM) exp) -> exp
;; Note: the functions which can optionally be provided here are patterned after
;; those in the exp-eval.rkt file, used for evaluating expressions. See that
;; file for examples.
;; Given an expression, this function performs simplification operations on both
;; the symbolic subexpressions, by way of algebraic operations, and the
;; functional expressions, by way of algebraic operations, and lastly numerical
;; computations where no symbols remain ambiguous in a subexpression, returning
;; the result of the simplification as a new expression, all according to the
;; default patterns.
;;; Example-usage (simplify-test)
;; ;; > (simplify '()
;; ;;             rewrite-rules-basic-algebra
;; ;;             '(* (+ (* (- 5 5) x?) 3) (sqrt (/ y? 5))))
;; ;; '(* 3 (sqrt (/ y? 5)))
(define (simplify-test)
  (let ((result (simplify '()
                          rewrite-rules-basic-algebra
                          '(* (+ (* (- 5 5) x?) 3) (sqrt (/ y? 5)))))
        (target '(* 3 (sqrt (/ y? 5)))))
    (if (equal? target result)
        #t
        (list 'got result 'expected target))))
(define (simplify functions rules exp)
  (define (eval-simplify expr)
    ; If the expression is a list, then treat it as a full expression tree
    ; simplifying each sub-tree and recombining the results.
    (if (list? expr)
        ; If there are no variables in the expression, evaluate it.
        (if (empty? (expr-vars expr))
            (if (empty? functions)
                (exp-easy-eval '() expr)
                (exp-eval functions '() expr))
            ; Otherwise, recursively simplify each sub-tree, and recombine the
            ; results.
            (let ((left (eval-simplify (lhs expr)))
                  (right (eval-simplify (rhs expr))))
              ; Trees with one child are treated as always having a left child.
              (if (null? right)
                  (list (car expr) left)
                  (list (car expr) left right))))
        ; Otherwise, return the expression as is: scalars cannot be simplified.
        expr))
  (define (simplify-iter expr last seen)
    (if (or (equal? last expr) (member expr seen))
        expr
        (simplify-iter (simplify-aux expr) expr (cons last seen))))
  (define (simplify-aux expr)
    (eval-simplify (if (empty? (expr-vars expr))
                       expr
                       (transform rules expr))))
  ; Transform the expression according to the given rules before reduction.
  (simplify-iter (simplify-aux exp) exp '()))

;;;; (exp) -> exp
;; Given an expression, this function performs simplification operations on both
;; the symbolic subexpressions, by way of algebraic operations, and the
;; functional expressions, by way of algebraic operations, and lastly numerical
;; computations where no symbols remain ambiguous in a subexpression, returning
;; the result of the simplification as a new expression, all according to the
;; default patterns.
;;; Example-usage (easy-simplify-test)
;; ;; > (easy-simplify '(* (+ (* (- 5 5) x?) 3) (sqrt (/ y? 5))))
;; ;; '(* 3 (sqrt (/ y? 5)))
(define (easy-simplify-test)
  (let ((result (easy-simplify '(* (+ (* (- 5 5) x?) 3) (sqrt (/ y? 5)))))
        (target '(* 3 (sqrt (/ y? 5)))))
    (if (equal? result target)
        #t
        (list 'got result 'expected target))))
(define (easy-simplify exp)
  (simplify '()
            (append rewrite-rules-functions
                    rewrite-rules-basic-algebra
                    rewrite-rules-differential-calculus)
            exp))

;;;; (exp1 var) -> exp2
;; Given a quoted expression, and a quoted variable to solve for, this function
;; will return '() if it cannot find a solution, or the solution in the form
;; '(= var exp3) where exp3 is the quoted expression that var is equal to. This
;; will use the default solving rules.
;;; Example Usage (easy-solve-test)
;;  ;; > (easy-solve '(= (exp (square (* (log (sin x?))))) 42) 'x?)
;;  ;; '((= (exp (square (* 5 (log (sin x?))))) 42)
;;  ;;  (= (square (* 5 (log (sin x?)))) (log 42))
;;  ;;  (= (* 5 (log (sin x?))) (sqrt (log 42)))
;;  ;;  (= (log (sin x?)) (/ (sqrt (log 42)) 5))
;;  ;;  (= (sin x?) (exp (/ (sqrt (log 42)) 5)))
;;  ;;  (= x? (arcsin (exp (/ (sqrt (log 42)) 5)))))
  (define (easy-solve-test)
    (let ((result (easy-solve '(= (exp (square (* 5 (log (sin x?))))) 42) 'x?)))
      (if (eq? 'x? (lhs (last result)))
          #t
          result)))
(define (easy-solve exp var)
  (solve (append rewrite-rules-functions
                 rewrite-rules-basic-algebra
                 rewrite-rules-solve-equation)
         exp var))

;;;; (exp) -> (a? b? c? ... z?)
;; Given a quoted expression, this function will return a list containing all of
;; the symbols which represent values in the  expression tree, or '() if there
;; are none; meaning that symbols which represent functions or operators will
;; not be listed in the result.
;;; Example Usage (eq-values-test)
;;  ;; > (eq-values '(= (exp (square (* 5 (log (sin x?))))) (+ a? 42)))
;;  ;; '(x? a?)
  (define (eq-values-test)
    (let ((vals (eq-values '(= (exp (square (* 5 (log (sin x?))))) (+ a? 42)))))
      (if (equal? '(x? a?) vals)
          #t
          vals)))
(define (eq-values exp)
  (cond ((null? exp) '())
        ((symbol? exp) (list exp))
        ((list? exp) (append (eq-values (lhs exp))
                             (eq-values (rhs exp))))
        (else '())))

;;;; (exp1 exp2 ... expN) -> '()
;; NOTE: This function is not truly functional: it has the side-effect of output
;; to the console, and has no natural return value.
;; This function outputs each expression on a new-line, as a convenience method
;; for printing each step in the process of solving an equation.
;;; Example Usage [no test provided].
;;  ;; > (print-work
;;  ;;     (easy-solve '(= (exp (square (* 5 (log (sin x?))))) (+ a? 42)) 'x?))
(define (print-work steps)
  (newline)
  (if (not (null? steps))
      (display (car steps))
      '())
  (if (null? steps)
      (newline)
      (print-work (cdr steps))))


;; Some basic and rather arbitrary rules for solving equations at the root level
;; across an '= sign.
(define rewrite-rules-solve-equation
  '(
    ; Forms for reducing a variable in count
    ((= y? (+ x? x?))
     (= y? (* 2 x?)))
    ((= y? (- x? x?))
     (= y? 0))
    ((= y? (* x? x?))
     (= y? (square x?)))
    ((= y? (* x? 0))
     (= y? 0))
    ((= y? (/ x? x?))
     (= y? 1))

    ((+ x? x?)
     (* 2 x?))
    ((* 2 x?)
     (+ x? x?))
    ((- x? x?)
     0)
    ((* x? x?)
     (square x?))
    ((square x?)
     (* x? x?))
    ((* x? 0)
     0)
    ((/ x? x?)
     1)

    ; Product of sums
    ((= y? (* (+ a? b?) (+ c? d?)))
     (= y? (+ (square a?) (+ (square b?) (* 2 (* a? b?))))))

    ; Completing the square
    ((= y? (+ (* a? (square x?)) (+ (* b? x?) c?)))
     (= (square (+ x? (/ b? (* 2 a?))))
        (+ (/ (- y? c?) a?) (square (/ b? (* 2 a?))))))
    ((= y? (+ (square x?) (+ (* b? x?) c?)))
     (= (square (+ x? (/ b? 2))) (+ (- y? c?) (square (/ b? 2)))))
    ((= y? (+ (square x?) (* b? x?)))
     (= (square (+ x? (/ b? 2))) (+ y? (square (/ b? 2)))))
    ((= y? (+ (square x?) (+ x? c?)))
     (= (square (+ x? (/ 1 2))) (+ (- y? c?) (square (/ 1 2)))))
    ((= y? (+ (square x?) x?))
     (= (square (+ x? (/ 1 2))) (+ y? (square (/ 1 2)))))

    ; Associativity
    ((= x? (+ y? (+ z? w?)))
     (= x? (+ (+ y? z?) w?)))
    ((= x? (+ (+ y? z?) w?))
     (= x? (+ y? (+ z? w?))))

    ((= x? (* y? (* z? w?)))
     (= x? (* (* y? z?) w?)))
    ((= x? (* (* y? z?) w?))
     (= x? (* y? (* z? w?))))

    ; Commutivity
    ((= x? (+ y? z?))
     (= x? (+ z? y?)))
    ((= x? (* y? z?))
     (= x? (* z? y?)))

    ((= x? (+ y? z?))
     (= y? (- x? z?)))
    ((= x? (+ y? z?))
     (= z? (- x? y?)))

    ; Distributivity
    ((= x? (* y? (+ z? w?)))
     (= x? (+ (* y? z?) (* y? w?))))
    ((= x? (* y? (- z? w?)))
     (= x? (- (* y? z?) (* y? w?))))

    ; Solving Subtraction
    ((= x? (- y? z?))
     (= y? (+ x? z?)))
    ((= x? (- y? z?))
     (= z? (- y? x?)))
    ((= x? (- y?))
     (= y? (- x?)))

    ; Solving Multiplication
    ((= x? (* y? z?))
     (= y? (/ x? z?)))
    ((= x? (* y? z?))
     (= z? (/ x? y?)))

    ; Solving Division
    ((= x? (/ y? z?))
     (= y? (* x? z?)))
    ((= x? (/ y? z?))
     (= z? (/ y? x?)))

    ; Inverse Functions
    ((= x? (expt y? 2))
     (= x? (square y?)))
    ((= x? (square y?))
     (= y? (sqrt x?)))
    ((= x? (sqrt y?))
     (= y? (square x?)))

    ((= x? (log y?))
     (= y? (exp x?)))
    ((= x? (exp y?))
     (= y? (log x?)))

    ((= x? (cos y?))
     (= y? (arccos x?)))
    ((= x? (sin y?))
     (= y? (arcsin x?)))
    ((= x? (tan y?))
     (= y? (arctan x?)))
    ((= x? (arccos y?))
     (= y? (cos x?)))
    ((= x? (arcsin y?))
     (= y? (sin x?)))
    ((= x? (arctan y?))
     (= y? (tan x?)))

    ; Differentiation/Integration
    ((= x? (deriv (int y? z?) z?))
     (= x? y?))
    ((= x? (int (deriv y? z?) z?))
     (= x? y?))

    ((= x? (deriv (+ y? z?) w?))
     (= (deriv y? w?) (- x? (deriv z? w?))))
    ((= x? (deriv (+ y? z?) w?))
     (= (deriv z? w?) (- x? (deriv y? w?))))

    ((= x? (deriv y? z?))
     (= y? (int x? z?)))
    ((= x? (int y? z?))
     (= y? (deriv x? z?)))

    ; TODO: Add more equations

    ))

;; Some basic rules for rewriting algebraic expressions according to the rules
;; of an abelian group.
(define rewrite-rules-basic-algebra
  '(
    ; Commutative law for addition
    ((+ x? ?y) (+ y? x?))

    ; Associative law for addition
    ((+ x? (+ y? z?)) (+ (+ x? y?) z?))

    ; Identity element for addition
    ((+ x? 0) x?)
    ((+ 0 x?) x?)

    ; Inverses for addition
    ((+ x? (- x?)) 0)
    ((+ (- x?) x?) 0)

    ; Distribution of multiplication over addition
    ((* x? (+ y? z?)) (+ (* x? y?) (* x? z?)))

    ; Commutative law for multiplication
    ((* x? y?) (* y? x?))

    ; Associative law for multiplication
    ((* x? (* y? z?)) (* (* x? y?) z?))

    ; Identity element for multiplication
    ((* x? 1) x?)
    ((* 1 x?) x?)

    ; Inverses for multiplication
    ((* x? (/ 1 x?)) 1)
    ((* (/ 1 x?) x?) 1)

    ; Multiplication by 0 -- shorthand for the combination of distributivity of
    ; multiplication over addition and of the additive identity a = a + 0
    ((* x? 0) 0)
    ((* 0 x?) 0)

    ))

;; Some basic rules for solving derivatives
(define rewrite-rules-differential-calculus
  '(
    ((deriv (+ f? g?) x?)
     (+ (deriv f? x?) (deriv g? x?)))
    ((deriv (- f? g?) x?)
     (- (deriv f? x?) (deriv g? x?)))

     ; Product Rule
    ((deriv (* f? g?) x?)
     (+ (* (deriv f? x?) g?) (* f? (deriv g? x?))))

     ; Quotient Rule
    ((deriv (/ f? g?) x?)
     (/ (- (* (deriv f? x?) g?) (* f? (deriv g? x?))) (square g?)))

    ; Exponent Rule
    ((deriv (expt x? n?) u?)
     (* n? (expt x? (- n? 1))))

    ; Chain rule
    ((deriv (f? (g? x?)) u?)
     (* (deriv (f? (g? x?)) u?) (deriv (g? x?) u?)))

    ; Exponential Rule
    ((deriv (exp x?) u?)
     (* (deriv x? u?) (exp x?)))

    ((deriv (sin x?) u?)
     (* (cos x?) (deriv x? u?)))
    ((deriv (cos x?) u?)
     (* (- (sin x?)) (deriv x? u?)))
    ((deriv (tan x?) u?)
     (square (sec x?)))
    ((deriv (sec x?) u?)
     (* (sec x?) (tan x?)))
    ((deriv (csc x?) u?)
     (- (* (csc x?) (cot x?))))
    ((deriv (cot x?) u?)
     (- (square (csc x?))))
    ((deriv (arcsin x?) u?)
     (/ 1 (sqrt (- 1 (square x?)))))
    ((deriv (arccos x?) u?)
     (/ (- 1) (sqrt (- 1 (square x?)))))
    ((deriv (arctan x?) u?)
     (/ (+ 1 (square x?))))
    ; TODO: finish adding derivative rules.

    ((deriv (log x?) u?)
     (/ (deriv x? u?) x?))

    ((deriv x? x?)
     1)
    ((deriv c?c u?)
     0)

    ))

;; Some basic rules for solving functional expressions.
(define rewrite-rules-functions
  '(

    ((exp (log x?))
     x?)
    ((log (exp x?))
     x?)

    ((sqrt (square x?))
     (abs x?))
    ((square (sqrt x?))
     (abs x?))

    ((abs (- x?))
     x?)

    ((* 2 (* (sin x?) (cos x?)))
     (sin (* 2 x?)))
    ((- (square (cos x?)) (square (sin x?)))
     (cos (* 2 x?)))
    ((+ (square (cos x?)) (square (sin x?)))
     1)

    ))
