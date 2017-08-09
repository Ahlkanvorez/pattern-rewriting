;; Some example usages of various functions, so that I don't have to fumble and
;; make embarrassing typos when trying to demonstrate code in person.

;; Usage: Once Racket has been installed, load a racket REPL using the
;;   following command:
;; $ racket
;; Then, enter the demo file by entering the following into the REPL:
;; > (enter! "demo.rkt")
;; After that, each demo can be viewed by executing any of the following lines:
;; > (demo1)
;; > (demo2)
;; > (demo3)
;; > (demo4)
;; > (demo5)

#lang racket

(require "symalg.rkt")
(require "pat.rkt")

(provide (all-defined-out))

(define (infix-to-prefix expr)
    (transform-exhaustive
        `(((^ base? exponent?)
           (expt base? exponent?))
          ((a? deriv b?)
           (deriv a? b?)))
        (transform-recursive
            `((lhs? op? rhs?)
              (op? lhs? rhs?))
            expr)))

(define (prefix-to-infix expr)
    (transform-exhaustive
        `(((base? expt exponent?)
            (base? ^ exponent?))
          ((a? deriv b?)
           (deriv a? b?)))
        (transform-recursive
            `((op? lhs? rhs?)
              (lhs? op? rhs?))
            expr)))

(define (simplify equation)
    (easy-simplify equation))

(define (solve input)
    (let ((equation (first input))
          (variable (third input)))
        (display (list `solving equation `for variable))
        (newline)
        (let* ((solution (easy-solve (infix-to-prefix equation) variable)))
            (print-work (map prefix-to-infix solution))
            (display (prefix-to-infix (easy-simplify (last solution))))
            (newline))))

;; Demonstrate some simple symbolic manipulation
(define (demo1)
    (let ((infix `((sqrt 2) = ((12 ^ 9) * (square x?)))))
        (display `(Rewriting ,infix using prefix-notation))
        (newline)
        (display (infix-to-prefix infix))
        (newline) (newline)
        (display `(Rewriting ,(infix-to-prefix infix) using infix-notation))
        (newline)
        (display (prefix-to-infix (infix-to-prefix infix)))
        (newline)))

;; Demonstrate solving & simplifying some equations.
(define (demo2)
    (solve `(((2 + x?) = 0) for x?)))

(define (demo3)
    (solve `(((square c?) = ((square a?) + (square b?))) for a?)))

(define (demo4)
    (solve `(((log (cos x?)) = (log (sin pi))) for x?)))

(define (demo5)
    (solve `(((square (x? + y?)) = (x? + y?)) for x?)))
