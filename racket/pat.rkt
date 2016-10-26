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
      (equal? (substitute '((Nihil nothing) (sub under) (sole (the sun)) (novum (is new)))
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
;;;; '((a? b?) (x? y?) (z? w?)) '(a list of lists and symbols) -> '(a new list transformed according to the given patterns)
;; Given a list of patterns, which allows for matching structural form instead of simply
;;  testing equality of symbols, and a list of symbols and lists, the original list is transformed
;;  according to the structural patterns provided, and the new list is returned.
;;; Example usage: (transform-test)
;;  ;; > (transform '(((if condition? result?) (result? unless (not condition?))))
;;  ;;              '(if (you are hungry) (eat some food)))
;;  ;; '((eat some food) unless (not (you are hungry)))
    (define (transform-test)
      (transform '(((if condition? result?) (result? unless (not condition?))))
                 '(if (you are hungry) (eat some food))))
(define (transform patterns a)
  (if (null? patterns)
      a
      (transform (cdr patterns) (transform-part (car patterns) a))))

;;;;
;; TODO: Comment
;;;
(define (transform-exhaustive patterns a)
  (let ((result (transform patterns a)))
    (if (equal? result a)
        result
        (transform-exhaustive patterns result))))

; Pattern of the form '(x? y?)
;;;;
;; TODO: Comment
;;;
(define (transform-part pattern a)
  (cond ((null? a) '())
        ((can-bind? (car pattern) a)
         (transform-total pattern a))
        ((can-bind? (car pattern) (car a))
         (cons (transform-total pattern (car a)) (transform-part pattern (cdr a))))
        ((list? (car a))
         (cons (transform-part pattern (car a))
               (transform-part pattern (cdr a))))
        (else (cons (car a) (transform-part pattern (cdr a))))))

;;;;
;; TODO: Comment
;;;
(define (transform-total pattern a)
  (cond ((or (null? pattern) (null? a)) a)
        ((can-bind? (car pattern) a)
         (let ((binds (bindings (car pattern) a)))
           (cadr (substitute binds pattern))))
        (else a)))

;;;;
;; TODO: Comment
;;;
(define (bind-var? x)
  (define (var? x)
    (equal? (car (reverse (symbol->list x))) #\?))
  (and (symbol? x)
       (var? x)))

;;;;
;; TODO: Comment
;;;
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

;;;;
;; TODO: Comment
;;;
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

;;;;
;; TODO: Comment
;;;
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

;; Given a pattern as a list, and an expression a,
;; this returns an association list of variables, and
;; constants, where variables match any structure and
;; constants match non-symbol-non-list items.
;;;;
;; TODO: Comment
;;;
(define (bindings pattern a)
  (define (bindings-aux pattern a)
    (cond ((null? a) '())
          ((or (bind-var? (car pattern))
               (bind-const? (car pattern) (car a)))
           (acons (car pattern) (list (car a)) (bindings-aux (cdr pattern) (cdr a))))
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
