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

;; Dependencies

#lang racket

; (require srfi/13)

(provide (all-defined-out))

;; Map-Reduce functions

;;;;
;; TODO: Comment
;;;
(define (range a b)
  (define (range-iter a b accum)
    (if (> a b)
        (reverse accum)
        (range-iter (+ a 1) b (cons a accum))))
  (range-iter a b '()))

;;;;
;; TODO: Comment
;;;
(define (map f a)
  (define (consf x accum)
    (cons (f x) accum))
  (reverse (reduce consf '() a)))

;;;;
;; TODO: Comment
;;;
(define (concatmap f a)
  (reduce append '() (map f a)))

;;;;
;; TODO: Comment
;;;
(define (group-common f g)
  (lambda (a accum)
    (if (not (equal? #f (assoc (f a) accum)))
        (acons (f a) (list (cons (g a) (cadr (assoc (f a) accum)))) (assoc-remove (f a) accum))
        (acons (f a) (list (g a)) accum))))

;;;;
;; TODO: Comment
;;;
(define (reduce f s a)
  (define (reduce-iter a accum)
    (if (null? a)
        accum
        (reduce-iter (cdr a) (f (car a) accum))))
  (reduce-iter a s))

;;;;
;; TODO: Comment
;;;
(define (repeat n)
  (lambda (x)
    (map (lambda (y) x) (range 1 n))))

;;;;
;; TODO: Comment
;;;
(define (sum a)
  (reduce + 0 a))

;;;;
;; TODO: Comment
;;;
(define (product a)
  (reduce * 1 a))

;;;;
;; TODO: Comment
;;;
(define (sequence f a b)
  (map f (range a b)))

;;;;
;; TODO: Comment
;;;
(define (series f a b)
  (sum (sequence f a b)))

;;;;
;; TODO: Comment
;;;
(define (filter f a)
  (define (filter-of f)
    (lambda (x accum)
      (if (f x)
          (cons x accum)
          accum)))
  (reverse (reduce (filter-of f) '() a)))

;;;;
;; TODO: Comment
;;;
(define (all f a)
  (reduce (lambda (a b) (and a b)) #t (map f a)))

;;;;
;; TODO: Comment
;;;
(define (any f a)
  (reduce (lambda (a b) (or a b)) #f (map f a)))

;;;;
;; TODO: Comment
;;;
(define (compose a)
  (define (compose-pair f g)
    (lambda (x)
      (f (g x))))
  (define (identity x)
    x)
  (reduce compose-pair identity a))

;;;;
;; TODO: Comment
;;;
(define (square x)
  (* x x))

;;;;
;; TODO: Comment
;;;
(define (halve a)
  (define (halve-iter end right left)
    (if (or (null? end) (null? (cdr end)))
        (list (reverse left) right)
        (halve-iter (cddr end) (cdr right) (cons (car right) left))))
  (halve-iter a a '()))

;;;;
;; TODO: Comment
;;;
(define (merge-sort a)
  (define (merge a)
    (define (merge-iter x y accum)
      (cond ((null? x) (append (reverse y) accum))
            ((null? y) (append (reverse x) accum))
            ((< (car x) (car y)) (merge-iter (cdr x) y (cons (car x) accum)))
            (else (merge-iter x (cdr y) (cons (car y) accum)))))
    (reverse (merge-iter (car a) (cadr a) '())))
  (if (or (null? a) (null? (cdr a)))
      a
      (merge (map merge-sort (halve a)))))

;;;;
;; TODO: Comment
;;;
(define (values tree)
  (cond ((null? tree) '())
        (else (cons (car tree)
                    (append (values (lhs tree))
                            (values (rhs tree)))))))

;;;;
;; TODO: Comment
;;;
(define (scar x)
  (if (null? x)
      x
      (car x)))

;;;;
;; TODO: Comment
;;;
(define (scdr x)
  (if (null? x)
      x
      (cdr x)))

;;;;
;; TODO: Comment
;;;
(define (lhs tree)
  (scar (scdr tree)))

;;;;
;; TODO: Comment
;;;
(define (rhs tree)
  (scar (scdr (scdr tree))))

;;;;
;; TODO: Comment
;;;
(define (flip tree)
  (list (car tree) (rhs tree) (lhs tree)))

;; Given a key k0, a value in a list '(v0), and an association list of the form
;;  '((k1 v1) (k2 v2) ... (kn vn))
;; this returns the association list
;;  '((k0 v0) (k1 v1) (k2 v2) ... (kn vn))
;;;;
;; TODO: Comment
;;;
(define (acons x y a)
  (cons (cons x y) a))

;; Given a key kj, and an association list of the form
;;   '((k1 v1) (k2 v2) ... (kj vj) ... (kn vn))
;; this returns the association list with every element except (kj vj)
;;;;
;; TODO: Comment
;;;
(define (assoc-remove x a)
  (define (assoc-remove-iter x a accum)
    (cond ((null? a) (reverse accum))
          ((equal? (caar a) x) (append (reverse accum) (cdr a)))
          (else (assoc-remove-iter x (cdr a) (cons (car a) accum)))))
  (assoc-remove-iter x a '()))

;;;;
;; TODO: Comment
;;;
(define (string-contains? source target)
  (not (equal? #f (string-contains? source target))))

;;;;
;; TODO: Comment
;;;
(define (string-cmp a b)
  (cond ((equal? a b) 0)
        ((string<? a b) -1)
        (else 1)))

;;;;
;; TODO: Comment
;;;
(define (id x) x)
