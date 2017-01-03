;;;
;; Author: Robert Mitchell <robert.mitchell36@gmail.com>
;; Edit log:
;; - 1/24/2015 (prototype)
;;;

;;;; Comment format:
;;    ;;;; Input signature -> output signature
;;    ;; Description of function
;;    ;; Example-usage (optional name of testing function)
;;    ;;  ;; > (example-invocation)
;;    ;;  ;; actual-output from that invocation.
;;    (optional testing function definition prior to actual function definition)

#lang racket

(require "lisp.rkt")
(require "pat.rkt")

(provide latex-format lisp-format lisp->latex)

;; Latex Formatter

;;;; string -> symbol
;; Given a string, this function returns a symbol of that string with a slash ('\') for a suffix.
;;; Example-usage (slash-test)
;;  ;; > (slash "slash-me")
;;  ;; '|\slash-me|
(define (slash-test)
  (let ((result (slash "slash-me"))
        (target '|\slash-me|))
    (if (equal? result target)
        #t
        `(got ,result expected ,target))))
(define (slash c)
  (string->symbol (list->string (cons #\\ (string->list c)))))

;;;; expression -> expression
;; Given an expression, this function returns a new expression where all symbols in the given
;; expression are replaced with their latex equivalents. Note: This function and lisp-format are
;; inverses.
;;; Example-usage (latex-format-test)
;;  ;; > (latex-format '(sqrt (sin (3 * x))))
;;  ;; '("sqrt" ("sin" (3 "*" "x")))
(define (latex-format-test)
  (let ((result (latex-format '(sqrt (sin (3 * x)))))
        (target '("sqrt" ("sin" (3 "*" "x")))))
    (if (equal? result target)
        #t
        `(got ,result expected ,target))))
(define (latex-format x)
  (cond ((symbol? x) (symbol->string x))
        ((list? x) (map latex-format x))
        (else x)))

;;;; expression -> expression
;; Given an expression, this function returns a new expression where all the latex equivalents
;; are replaced with lisp symbols. Note: This function and latex-format are inverses.
;;; Example-usage (lisp-format-test)
;;  ;; > (lisp-format '("sqrt" ("sin" (3 "*" "x"))))
;;  ;; '(sqrt (sin (3 * x)))
(define (lisp-format-test)
  (let ((result (lisp-format '("sqrt" ("sin" (3 "*" "x")))))
        (target '(sqrt (sin (3 * x)))))
    (if (equal? result target)
        #t
        `(got ,result expected ,target))))
(define (lisp-format x)
  (cond ((string? x) (string->symbol x))
        ((list? x) (map lisp-format x))
        (else x)))

;;;; expression -> list
;; Given an expression, this function returns that expression formatted as a latex mathematical
;; expression, bracketed between '$$'.
;;; Example-usage (lisp->latex-test)
;;  ;; > (lisp->latex '(sqrt (sin (* 3 x))))
;;  ;; '("$$" "\\sqrt" "{" ("\\sin" "{" (3 "*" "x") "}") "}" "$$")
(define (lisp->latex-test)
  (let ((result (lisp->latex '(sqrt (sin (* 3 x)))))
        (target '("$$" "\\sqrt" "{" ("\\sin" "{" (3 "*" "x") "}") "}" "$$")))
    (if (equal? result target)
        #t
        `(got ,result expected ,target))))
(define (lisp->latex exp)
  (cons "$$" (append (map latex-format (transform-exhaustive lisp-to-latex exp)) (list "$$"))))

;;;; a list of patterns.
;; This list contains patterns mapping lisp expressions to latex equivalents, formatted with
;; slashes where appropriate.
;;;
(define lisp-to-latex
  `(
    ((= x? y?) (x? = y?))
    ((+ x? y?) (x? + y?))
    ((- x? y?) (x? - y?))
    ((* x? y?) (x? * y?))
    ((/ x? y?) (,(slash "frac") \{ x? \} \{ y? \}))

    ((sqrt x?) (,(slash "sqrt") \{ x? \}))
    ((square x?) (x? ^ 2))
    ((expt x? a?) (x? ^ a?))

    ((exp x?) (,(slash "exp") \{ x? \}))
    ((log x?) (,(slash "log") \{ x? \}))

    ((cos x?) (,(slash "cos") \{ x? \}))
    ((sin x?) (,(slash "sin") \{ x? \}))
    ((tan x?) (,(slash "tan") \{ x? \}))
    ((arccos x?) (,(slash "arccos") \{ x? \}))
    ((arcsin x?) (,(slash "arcsin") \{ x? \}))
    ((arctan x?) (,(slash "arctan") \{ x? \}))

    ;; TODO: Add more patterns to allow answers from (solve ...) outputted as latex.

    ))
