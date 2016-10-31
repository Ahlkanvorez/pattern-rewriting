;;;
;; Author: Robert Mitchell <robert.mitchell36@gmail.com>
;; Edit log:
;; - 1/24/2015 (Prototype)
;; - 1/25/2015 (New algorithm - BFS)
;; - 1/27/2015 (Comments)
;; - 9/5/2015 (Simplified and partially evaluated answers from solve)
;; - 10/24/2016 (Corrected major bugs in 'simplify' and 'solve'; added comment format; added some comments)
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
;; Given an association list of rewrite rules, a quoted expression, and a quoted variable to
;; solve for, this function will return '() if it cannot find a solution, or the solution
;; in the form '(= var exp3) where exp3 is the quoted expression that var is equal to.
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
    (solve (append rewrite-rules-functions
                   rewrite-rules-basic-algebra
                   rewrite-rules-solve-equation)
           '(= (exp (square (* 5 (log (sin x?))))) 42)
           'x?))
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
  (bfs (list exp) either-solution? extract-solution unknown? alter-expression remember))

;;;;
;; TODO: Comment
;;;
(define (simplify functions rules exp)
  (define (eval-simplify expr)
    (if (list? expr)
        (if (empty? (expr-vars expr))
            (if (empty? functions)
                (exp-easy-eval '() expr)
                (exp-eval functions '() expr))
            (list (car expr)
                  (eval-simplify (lhs expr))
                  (eval-simplify (rhs expr))))
        expr))
  (eval-simplify (if (empty? (expr-vars exp))
                     exp
                     (transform rules exp))))

;;;;
;; TODO: Comment
;;;
(define (easy-simplify exp)
  (simplify '()
            (append rewrite-rules-functions
                    rewrite-rules-basic-algebra
                    rewrite-rules-differential-calculus)
            exp))

;;;; (exp1 var) -> exp2
;; Given a quoted expression, and a quoted variable to solve for, this function will return '()
;; if it cannot find a solution, or the solution in the form '(= var exp3) where exp3 is the
;; quoted expression that var is equal to. This will use the default solving rules.
;;; TODO: Comment example usage.
(define (easy-solve exp var)
  (let ((solution (solve (append rewrite-rules-solve-equation
                                 rewrite-rules-basic-algebra)
                         exp var)))
    (append solution
            (list (easy-simplify (last solution))))))

;;;;
;; TODO: Comment
;;;
(define (eq-values exp)
  (cond ((null? exp) '())
        ((symbol? exp) (list exp))
        ((list? exp) (append (eq-values (lhs exp))
                             (eq-values (rhs exp))))
        (else '())))

;;;;
;; TODO: Comment
;;;
(define (print-work steps)
  (newline)
  (if (not (null? steps))
      (display (car steps))
      '())
  (if (null? steps)
      (newline)
      (print-work (cdr steps))))


;; TODO: Comment
(define rewrite-rules-solve-equation
  '(
    ; Forms for reducing a variable in count
    ((= y? (+ x? x?)) (= y? (* 2 x?)))
    ((= y? (- x? x?)) (= y? 0))
    ((= y? (* x? x?)) (= y? (square x?)))
    ((= y? (* x? 0)) (= y? 0))
    ((= y? (/ x? x?)) (= y? 1))

    ((+ x? x?) (* 2 x?))
    ((* 2 x?) (+ x? x?))
    ((- x? x?) 0)
    ((* x? x?) (square x?))
    ((square x?) (* x? x?))
    ((* x? 0) 0)
    ((/ x? x?) 1)

    ; Product of sums
    ((= y? (* (+ a? b?) (+ c? d?))) (= y? (+ (square a?) (+ (square b?) (* 2 (* a? b?))))))

    ; Completing the square
    ((= y? (+ (* a? (square x?)) (+ (* b? x?) c?)))
     (= (square (+ x? (/ b? (* 2 a?)))) (+ (/ (- y? c?) a?) (square (/ b? (* 2 a?))))))
    ((= y? (+ (square x?) (+ (* b? x?) c?)))
     (= (square (+ x? (/ b? 2))) (+ (- y? c?) (square (/ b? 2)))))
    ((= y? (+ (square x?) (* b? x?)))
     (= (square (+ x? (/ b? 2))) (+ y? (square (/ b? 2)))))
    ((= y? (+ (square x?) (+ x? c?)))
     (= (square (+ x? (/ 1 2))) (+ (- y? c?) (square (/ 1 2)))))
    ((= y? (+ (square x?) x?))
     (= (square (+ x? (/ 1 2))) (+ y? (square (/ 1 2)))))

    ; Associativity
    ((= x? (+ y? (+ z? w?))) (= x? (+ (+ y? z?) w?)))
    ((= x? (+ (+ y? z?) w?)) (= x? (+ y? (+ z? w?))))

    ((= x? (* y? (* z? w?))) (= x? (* (* y? z?) w?)))
    ((= x? (* (* y? z?) w?)) (= x? (* y? (* z? w?))))

    ; Commutivity
    ((= x? (+ y? z?)) (= x? (+ z? y?)))
    ((= x? (* y? z?)) (= x? (* z? y?)))

    ((= x? (+ y? z?)) (= y? (- x? z?)))
    ((= x? (+ y? z?)) (= z? (- x? y?)))

    ; Distributivity
    ((= x? (* y? (+ z? w?))) (= x? (+ (* y? z?) (* y? w?))))
    ((= x? (* y? (- z? w?))) (= x? (- (* y? z?) (* y? w?))))

    ; Solving Subtraction
    ((= x? (- y? z?)) (= y? (+ x? z?)))
    ((= x? (- y? z?)) (= z? (- y? x?)))
    ((= x? (- y?)) (= y? (- x?)))

    ; Solving Multiplication
    ((= x? (* y? z?)) (= y? (/ x? z?)))
    ((= x? (* y? z?)) (= z? (/ x? y?)))

    ; Solving Division
    ((= x? (/ y? z?)) (= y? (* x? z?)))
    ((= x? (/ y? z?)) (= z? (/ y? x?)))

    ; Inverse Functions
    ((= x? (expt y? 2)) (= x? (square y?)))
    ((= x? (square y?)) (= y? (sqrt x?)))
    ((= x? (sqrt y?)) (= y? (square x?)))

    ((= x? (log y?)) (= y? (exp x?)))
    ((= x? (exp y?)) (= y? (log x?)))

    ((= x? (cos y?)) (= y? (arccos x?)))
    ((= x? (sin y?)) (= y? (arcsin x?)))
    ((= x? (tan y?)) (= y? (arctan x?)))
    ((= x? (arccos y?)) (= y? (cos x?)))
    ((= x? (arcsin y?)) (= y? (sin x?)))
    ((= x? (arctan y?)) (= y? (tan x?)))

    ; Differentiation/Integration
    ((= x? (deriv (int y? z?) z?)) (= x? y?))
    ((= x? (int (deriv y? z?) z?)) (= x? y?))

    ((= x? (deriv (+ y? z?) w?)) (= (deriv y? w?) (- x? (deriv z? w?))))
    ((= x? (deriv (+ y? z?) w?)) (= (deriv z? w?) (- x? (deriv y? w?))))

    ((= x? (deriv y? z?)) (= y? (int x? z?)))
    ((= x? (int y? z?)) (= y? (deriv x? z?)))

    ; TODO: Add more equations

    ))

;; TODO: Comment
(define rewrite-rules-basic-algebra
  '(
    ((+ x? ?y) (+ y? x?))                           ; Commutative law for addition
    ((+ x? (+ y? z?)) (+ (+ x? y?) z?))             ; Associative law for addition
    ((+ x? 0) x?)                                   ; Identity element for addition
    ((+ 0 x?) x?)                                   ; --
    ((+ x? (- x?)) 0)                               ; Inverses for addition
    ((+ (- x?) x?) 0)                               ; Inverses for addition

    ((* x? (+ y? z?)) (+ (* x? y?) (* x? z?)))      ; Distribution of multiplication over addition

    ((* x? y?) (* y? x?))                           ; Commutative law for multiplication
    ((* x? (* y? z?)) (* (* x? y?) z?))             ; Associative law for multiplication
    ((* x? 1) x?)                                   ; Identity element for multiplication
    ((* 1 x?) x?)                                   ; --
    ((* x? (/ 1 x?)) 1)                             ; Inverses for multiplication
    ((* (/ 1 x?) x?) 1)                             ; Inverses for multiplication
    ((* x? 0) 0)                                    ;
    ((* 0 x?) 0)                                    ;

    ))

;; TODO: Comment
(define rewrite-rules-differential-calculus
  '(
    ((deriv (+ f? g?) x?) (+ (deriv f? x?) (deriv g? x?)))
    ((deriv (- f? g?) x?) (- (deriv f? x?) (deriv g? x?)))
    ((deriv (* f? g?) x?) (+ (* (deriv f? x?) g?) (* f? (deriv g? x?))))                    ; Product Rule
    ((deriv (/ f? g?) x?) (/ (- (* (deriv f? x?) g?) (* f? (deriv g? x?))) (square g?)))    ; Quotient Rule
    ((deriv (expt x? n?) u?) (* n? (expt x? (- n? 1))))                                     ; Exponent Rule
    ((deriv (f? (g? x?)) u?) (* (deriv (f? (g? x?)) u?) (deriv (g? x?) u?)))                ; Chain rule

    ((deriv (exp x?) u?) (* (deriv x? u?) (exp x?)))                                        ; Exponential Rule

    ((deriv (sin x?) u?) (* (cos x?) (deriv x? u?)))
    ((deriv (cos x?) u?) (* (- (sin x?)) (deriv x? u?)))
    ((deriv (tan x?) u?) (square (sec x?)))
    ((deriv (sec x?) u?) (* (sec x?) (tan x?)))
    ((deriv (csc x?) u?) (- (* (csc x?) (cot x?))))
    ((deriv (cot x?) u?) (- (square (csc x?))))
    ((deriv (arcsin x?) u?) (/ 1 (sqrt (- 1 (square x?)))))
    ((deriv (arccos x?) u?) (/ (- 1) (sqrt (- 1 (square x?)))))
    ((deriv (arctan x?) u?) (/ (+ 1 (square x?))))
    ; TODO: finish adding derivative rules.

    ((deriv (log x?) u?) (/ (deriv x? u?) x?))

    ((deriv x? x?) 1)
    ((deriv c?c u?) 0)

    ))

;; TODO: Comment
(define rewrite-rules-functions
  '(

    ((exp (log x?)) x?)
    ((log (exp x?)) x?)

    ((sqrt (square x?)) (abs x?))
    ((square (sqrt x?)) (abs x?))

    ((abs (- x?)) x?)

    ((* 2 (* (sin x?) (cos x?))) (sin (* 2 x?)))
    ((- (square (cos x?)) (square (sin x?))) (cos (* 2 x?)))
    ((+ (square (cos x?)) (square (sin x?))) 1)
    ))
