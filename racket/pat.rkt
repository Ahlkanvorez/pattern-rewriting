;;;
;; Author: Robert Mitchell <robert.mitchell36@gmail.com>
;; Edit log:
;; - 1/24/2015, 1/25/2015 (prototype)
;; - 10/24/2016 (added comment format; added comments)
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

(provide (all-defined-out))

;; Pattern rewrite code

;;;; 'symbol -> '(#\s #\y #\m #\b #\o #\l)
;; Taking any symbol as input, a list of symbols, which consist of the
;;  letters in the original symbol, is returned.
;;; Example usage (symbol->list-test)
;;  ;; > (symbol->list 'symbol)
;;  ;; '(#\s #\y #\m #\b #\o #\l)
    (define (symbol->list-test)
      (equal? (symbol->list 'symbol)
              '(#\s #\y #\m #\b #\o #\l)))
(define (symbol->list x)
  (string->list (symbol->string x)))

;;;; '((k1 v1) (k2 v2) ...) '(lists and symbols) -> '(list of replaced symbols)
;; Taking a dictionary (list of key-value pairs), and an original list of
;;  symbols or more lists, the symbols which exist as keys in the dictionary
;;  are replaced with their associated values, and the resulting list is
;;  returned; which is a new list, as this is a constructive function.
;;; Example usage (substitute-test)
;;  ;; > (substitute '((Hello Farewell) (friend sir)) '(Hello friend)
;;  ;; '(Farewell sir)
    (define (substitute-test)
      (equal? (substitute '((Nihil nothing)
                            (sub under)
                            (sole (the sun))
                            (novum (is new)))
                          '(Nihil sub sole novum)) ; Ecclesiastes
              '(nothing under (the sun) (is new))))
(define (substitute dict a)
  (define (substitutor-for dict)
    (lambda (x)
      (let ((pair (assoc x dict)))
        (if (equal? pair #f)
            (if (list? x)
                (map (substitutor-for dict) x)
                x)
            (cadr pair)))))
  (if (list? a)
      (map (substitutor-for dict) a)
      a))

; Patterns of the form '((x? y?) (z? w?) (a? b?))
;;;; '((a? b?) (x? y?) (z? w?)) '(a list of lists and symbols)
;;;;    -> '(a new list transformed according to the given patterns)
;; Given a list of patterns, which allows for matching structural form instead
;; of simply testing equality of symbols, and a list of symbols and lists, the
;; original list is transformed according to the structural patterns provided,
;; and the new list is returned. This transforms the entire expression tree
;; by considering every branch and node, not simply the root.
;;; Example-usage: (transform-test)
;;  ;; > (transform '(((if condition? result?)
;;  ;;                 (result? unless (not condition?))))
;;  ;;              '(if (you are hungry) (eat some food)))
;;  ;; '((eat some food) unless (not (you are hungry)))
    (define (transform-test)
      (transform '(((if condition? result?)
                    (result? unless (not condition?))))
                 '(if (you are hungry) (eat some food))))
(define (transform patterns a)
  (if (null? patterns)
      a
      (transform (cdr patterns) (transform-part (car patterns) a))))

;;;; (patterns expression) -> expression
;; This function continues to transform the list using the transform function
;; until the list does not change, or until the resulting transformation has
;; been seen previously.
;;; Example-usage: (transform-exhaustive-test)
;;  ;; > (transform-exhaustive '(((S (N (S x?))) (N x?)) ((N (N x?)) (I x?))
;;  ;;                           ((S (I x?)) (S x?)) ((N (I x?) (N x?))))
;;  ;;                         '(S (S (S (N (S (N (N (S (S (N x?)))))))))))
;;  ;; 0
(define (transform-exhaustive-test)
  (let ((result (transform-exhaustive
                  '(((S (N (S x?))) (N x?)) ((N (N x?)) (I x?))
                      ((S (I x?)) (S x?)) ((N (I x?) (N x?))))
                  '(S (S (S (N (S (N (N (S (S (N x?))))))))))))
        (target '(I x?)))
    (if (equal? result target)
        #t
        `(got ,result expected ,target))))
(define (transform-exhaustive patterns a)
  (define (transform-exhaustive-iter a visited)
    (let ((result (transform patterns a)))
      (if (or (equal? result a) (member result visited))
          result
          (transform-exhaustive-iter result (cons result visited)))))
  (transform-exhaustive-iter a (list a)))

;;;; (pattern expression) -> expression
;; This function applies the given pattern, if applicable, to the given
;; expression, by matching either the whole expression, or each branch of the
;; expression tree. If one part of the tree cannot be changed, but another can
;; be changed, the unchangeable part is left as it was.
;;; Example-usage (transform-part-test)
;;  ;; > (transform-part
;;  ;;      '((leaf?) (branch (leaf?) (leaf?)))
;;  ;;      '(branch (branch (leaf)
;;  ;;                       (branch (leaf) (leaf)))
;;  ;;               (branch (branch (leaf))))
;;  ;; '(branch
;;  ;;   (branch
;;  ;;    (branch (leaf) (leaf))
;;  ;;    branch
;;  ;;    ((branch (leaf) (leaf)))
;;  ;;    ((branch (leaf) (leaf))))
;;  ;;   branch
;;  ;;   ((branch (branch (leaf))))
;;  ;;   ((branch (branch (leaf)))))
(define (transform-part-test)
  (let ((result (transform-part
                  '((leaf?) (branch (leaf?) (leaf?)))
                  '(branch (branch (leaf)
                                   (branch (leaf) (leaf)))
                           (branch (branch (leaf))))))
        (target '(branch
                  (branch
                   (branch (leaf) (leaf))
                   branch
                   ((branch (leaf) (leaf)))
                   ((branch (leaf) (leaf))))
                  branch
                  ((branch (branch (leaf))))
                  ((branch (branch (leaf)))))))
    (if (equal? result target)
        #t
        `(got ,result expected ,target))))
(define (transform-part pattern a)
  (cond ((null? a) '())
        ((can-bind? (car pattern) a)
         (transform-total pattern a))
        ((can-bind? (car pattern) (car a))
         (cons (transform-total pattern (car a))
               (transform-part pattern (cdr a))))
        ((list? (car a))
         (cons (transform-part pattern (car a))
               (transform-part pattern (cdr a))))
        (else (cons (car a) (transform-part pattern (cdr a))))))

;;;; (pattern expression) -> expression
;; Given a pattern and an expression, this function attempts to match the
;; pattern to the entire expression at once. if the pattern is null, or the
;; expression is null, then null is returned. If the pattern does not match the
;; expression, then the original expression is returned unaltered.
;;; Example-usage (transform-total-test)
;;  ;; > (transform-total '((square x?) (* x? x?))
;;  ;;                    '(square (sqrt (log 25))))
;;  ;; '(* (sqrt (log 25)) (sqrt (log 25)))
(define (transform-total-test)
  (let ((result (transform-total '((square x?) (* x? x?))
                                 '(square (sqrt (log 25)))))
        (target '(* (sqrt (log 25)) (sqrt (log 25)))))
    (if (equal? result target)
        #t
        `(got ,result expected ,target))))
(define (transform-total pattern a)
  (cond ((or (null? pattern) (null? a)) a)
        ((can-bind? (car pattern) a)
         (let ((binds (bindings (car pattern) a)))
           (cadr (substitute binds pattern))))
        (else a)))

(define (transform-recursive pattern a)
  (cond ((or (null? pattern) (null? a))
          a)
        ((list? a)
          (transform-total
            pattern
            (map (lambda (x) (transform-recursive pattern x))
                 a)))
        (else
          (transform-total pattern a))))

;;;; (symbol) -> boolean
;; Given a symbol, this function returns true if the symbol is formatted
;; properly to be used as a variable in binding for pattern rewriting, and false
;; otherwise.
;;; Example-usage (bind-var?-test)
;;  ;; > (map bind-var? '(x? x 5))
;;  ;; '(#t #f #f)
(define (bind-var?-test)
  (let ((result (map bind-var? '(x? x 5)))
        (target '(#t #f #f)))
    (if (equal? result target)
        #t
        `(got ,result expected ,target))))
(define (bind-var? x)
  (define (var? x)
    (equal? (car (reverse (symbol->list x))) #\?))
  (and (symbol? x)
       (var? x)))

;;;; (symbol scalar) -> boolean
;; Given a symbol and a scalar, this function returns whether the symbol is a
;; valid variable for matching constants, and whether the scalar given is a
;; valid constant. A symbol is particularly used for matching constants if
;; instead of having the suffix '?' it has the suffix '?c', and a scalar is a
;; valid constant if it is neither a symbol (which could be a variable) nor a
;; list (which would be better called a vector).
;;; Example-usage (bind-const?-test)
;;  ;; > (map (lambda (args) (apply bind-const? args))
;;  ;;        '((x?c 5) (x?c (a b c)) (x? 5)))
;;  ;; '(#t #f #f)
(define (bind-const?-test)
  (let ((result (map (lambda (args) (apply bind-const? args))
                     '((x?c 5) (x?c (a b c)) (x? 5))))
        (target '(#t #f #f)))
    (if (equal? result target)
        #t
        `(got ,result expected ,target))))
(define (bind-const? x a)
  (define (constant-var? x)
    (and (symbol? x)
         (let ((y (reverse (symbol->list x))))
           (and (equal? (car y) #\c)
                (equal? (cadr y) #\?)))))
  (define (constant? a)
    (and (not (symbol? a))
         (not (list? a))))
  (and (constant? a)
       (constant-var? x)))

;;;; (expr) -> list
;; Given an expression, this function returns a list of all the variables in the
;; expression tree, whether they be values on leaves or branches. The expression
;; tree is assumed to be structured such that each node has at most two child
;; branches.
;;; Example-usage (expr-vars-test)
;;  ;; > (expr-vars '(f? (g? x? y?) (h? a? (i? b?))))
;;  ;; '(f? g? x? y? h? a? i? b?)
(define (expr-vars-test)
  (let ((result (expr-vars '(f? (g? x? y?) (h? a? (i? b?)))))
        (target '(f? g? x? y? h? a? i? b?)))
    (if (equal? result target)
        #t
        `(got ,result expected ,target))))
(define (expr-vars expr)
  (if (null? expr)
      '()
      (if (list? expr)
          (append (expr-vars (first expr))
                  (expr-vars (lhs expr))
                  (expr-vars (rhs expr)))
          (if (bind-var? expr)
              (list expr)
              '()))))

;;;; (pattern expr) -> boolean
;; This function returns true if the given pattern can bind to the given
;; expression, and false otherwise. Note that a bind must be a complete match,
;; not only a match at some sub-level of the expression tree.
;;; Example-usage (can-bind?-test)
;;  ;; > (can-bind? '(subject? adverb? reporting-verb?
;;  ;;                  (subject-accusative? subordinate-clause? infinitive?))
;;  ;;              '((A hoplite) confidently said
;;  ;;                (Thermopylae (by Greek troops) (would be protected))))
;;  ;; #t
(define (can-bind?-test)
  (let ((result
          (can-bind?
            '(subject? adverb? reporting-verb?
                (subject-accusative? subordinate-clause? infinitive?))
            '((A hoplite) confidently said (Thermopylae (by Greek troops)
              (would be protected)))))
        (target #t))
    (if (equal? result target)
        #t
        `(got ,result expected ,target))))
(define (can-bind? pattern a)
  (define (can-bind-aux? pattern a)
    (or (equal? pattern a)
        (and (list? pattern) (list? a) (not (null? pattern)) (not (null? a))
             (if (and (list? (car pattern)) (list? (car a)))
                 (can-bind-aux? (car pattern) (car a))
                 (or (bind-var? (car pattern))
                     (bind-const? (car pattern) (car a))
                     (equal? (car pattern) (car a))))
             (can-bind-aux? (cdr pattern) (cdr a)))))
  (and (can-bind-aux? pattern a)
       (not (null? (bindings pattern a)))))

;;;; (pattern expression) -> list
;; Given a pattern and an expression, if the pattern matches the expression,
;; this function returns the bindings mapping the variables in the pattern to
;; their associated expressions within the expression tree.
;;; Example-usage (bindings-test)
;;  ;; > (bindings '(subject? adverb? reporting-verb?
;;  ;;                (subject-accusative? subordinate-clause? infinitive?))
;;  ;;             '((A hoplite) confidently said (Thermopylae (by Greek troops)
;;  ;;               (would be protected))))
;;  ;; '((subject? (A hoplite))
;;  ;;   (adverb? confidently)
;;  ;;   (reporting-verb? said)
;;  ;;   (subject-accusative? Thermopylae)
;;  ;;   (subordinate-clause? (by Greek troops))
;;  ;;   (infinitive? (would be protected)))
(define (bindings-test)
  (let ((result
          (bindings
              '(subject? adverb? reporting-verb?
                  (subject-accusative? subordinate-clause? infinitive?))
              '((A hoplite) confidently said (Thermopylae (by Greek troops)
                  (would be protected)))))
        (target '((subject? (A hoplite))
                  (adverb? confidently)
                  (reporting-verb? said)
                  (subject-accusative? Thermopylae)
                  (subordinate-clause? (by Greek troops))
                  (infinitive? (would be protected)))))
    (if (equal? result target)
        #t
        `(got ,result expected ,target))))
(define (bindings pattern a)
  (define (bindings-aux pattern a)
    (cond ((null? a) '())
          ((or (bind-var? (car pattern))
               (bind-const? (car pattern) (car a)))
           (acons (car pattern)
                  (list (car a))
                  (bindings-aux (cdr pattern) (cdr a))))
          ((list? (car pattern))
           (append (bindings-aux (car pattern) (car a))
                   (bindings-aux (cdr pattern) (cdr a))))
          (else (bindings-aux (cdr pattern) (cdr a)))))
  (define (consistent-binding? binds)
    (define (all-values-equal? x a)
      (cond ((null? a) #t)
            ((equal? #f (assoc (car x) a)) #t)
            (else
             (and (equal? (cadr x) (cadr (assoc (car x) a)))
                  (all-values-equal? x (assoc-remove (car x) a))))))
    (or (null? binds)
        (and (all-values-equal? (car binds) (cdr binds))
             (consistent-binding? (cdr binds)))))
  (let ((binds (bindings-aux pattern a)))
    (if (consistent-binding? binds)
        binds
        '())))
