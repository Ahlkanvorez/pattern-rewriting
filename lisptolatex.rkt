;;;
;; Author: Robert Mitchell <robert.mitchell36@gmail.com>
;; Edit log:
;; - 1/24/2015 (prototype)
;; -
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

;;;;
;; TODO: Comment
;;;
(define (slash c)
	(string->symbol (list->string (cons #\\ (string->list c)))))

;;;;
;; TODO: Comment
;;;
(define (latex-format x)
	(cond ((symbol? x) (symbol->string x))
		  ((list? x) (map latex-format x))
		  (else x)))

;;;;
;; TODO: Comment
;;;
(define (lisp-format x)
	(cond ((string? x) (string->symbol x))
		  ((list? x) (map lisp-format x))
		  (else x)))

;;;;
;; TODO: Comment
;;;
(define (lisp->latex exp)
	(cons "$$" (append (map latex-format (transform-exhaustive lisp-to-latex exp)) (list "$$"))))

;(define (latex->lisp exp)
;	(transform latex-to-lisp (map lisp-format exp)))

;;;;
;; TODO: Comment
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

; (define latex-to-lisp (reverse (assoc-inverse lisp-to-latex)))
