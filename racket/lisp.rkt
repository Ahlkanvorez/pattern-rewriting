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

;;;; (a b) -> [a, b)
;; Precondition: a <= b
;; Returns a list of integers representing the mathematical object [a, b); viz. a list
;; of every integer from a inclusive to b exclusive, from least to greatest.
;;; Example-usage (range-test)
;;  ;; > (range -3 12)
;;  ;; '(-3 -2 -1 0 1 2 3 4 5 6 7 8 9 10 11)
(define (range-test)
  (let ((result (range -3 12))
        (target '(-3 -2 -1 0 1 2 3 4 5 6 7 8 9 10 11)))
    (if (equal? result target)
        #t
        (list 'got result 'expected target))))
(define (range a b)
  (define (range-iter b accum)
    (if (< b a)
        accum
        (range-iter (- b 1) (cons b accum))))
  (range-iter (- b 1) '()))

;;;; (function list) -> list
;; Given a function and a list, this function returns a new list whose elements are the result of
;; applying the given function to each element of the original list.
;;; Example-usage (map-test)
;; ;; > (map square (range -4 5))
;; ;; '(16 9 4 1 0 1 4 9 16)
(define (map-test)
  (let ((result (map square (range -4 5)))
        (target '(16 9 4 1 0 1 4 9 16)))
    (if (equal? result target)
        #t
        (list 'got result 'expected target))))
(define (map f a)
  (define (consf x accum)
    (cons (f x) accum))
  (reverse (reduce consf '() a)))

;;;; (function list) -> (list)
;; Performs a map operation on the given list, then appends every sublist in the list. This
;; function assumes that each element in the list will be a list after the map operation.
;;; Example-usage (concatmap-test)
;;  ;; > (concatmap (lambda (n) (range 1 (+ 1 n))) (range 1 4))
;;  ;; '(1 2 3 1 2 1)
(define (concatmap-test)
  (let ((result (concatmap (lambda (n) (range 1 (+ 1 n))) (range 1 4)))
        (target '(1 2 3 1 2 1)))
    (if (equal? result target)
        #t
        (list 'got result 'expected target))))
(define (concatmap f a)
  (reduce append '() (map f a)))

;;;; (pair -> anything, pair -> anything) -> ((element list) -> list)
;; Given two functions which alter an assoc pair in some way, this function returns a new
;; function for use in conjunction with 'reduce' which performs operations using the two given functions
;; on pairs in an assoc list with common keys. This is a generalization of the case where the first
;; functional argument is 'first', and the second functional argument is 'second'.
;;; Example-usage (group-common-test)
;;  ;; > (reduce (group-common first second) '() '((Indo-European Latin) (Indo-European Greek)
;;  ;;                                             (Jam Grape) (Jam Plum) (Indo-European Sanskrit)))
;;  ;; '((Indo-European (Sanskrit Greek . Latin)) (Jam (Plum . Grape)))
(define (group-common-test)
  (let ((result (reduce (group-common first second) '() '((Indo-European Latin) (Indo-European Greek)
                                                          (Jam Grape) (Jam Plum) (Indo-European Sanskrit))))
        (target '((Indo-European (Sanskrit Greek . Latin)) (Jam (Plum . Grape)))))
    (if (equal? result target)
        #t
        `(got ,result expected ,target))))
(define (group-common f g)
  (lambda (a accum)
    (if (assoc (f a) accum)
        (acons (f a) (list (cons (g a) (cadr (assoc (f a) accum))))
               (assoc-remove (f a) accum))
        (acons (f a) (list (g a))
               accum))))

;;;; (function initial-value values) -> value
;; Given a function which combines two elements of data, an initial data value, and a list of data
;; elements, this function uses the given function to combine all of the elements of the given list,
;; in FIFO order.
;;; Example-usage (reduce-test)
;;  ;; > (reduce (lambda (a b) (list b '< a)) 0 (range 1 5))
;;  ;; '((((0 < 1) < 2) < 3) < 4)
(define (reduce-test)
  (let ((result (reduce (lambda (a b) (list b '< a)) 0 (range 1 5)))
        (target '((((0 < 1) < 2) < 3) < 4)))
    (if (equal? result target)
        #t
        (list 'got result 'expected target))))
(define (reduce f s a)
  (define (reduce-iter a accum)
    (if (null? a)
        accum
        (reduce-iter (cdr a) (f (car a) accum))))
  (reduce-iter a s))

;;;; (natural-number) -> (f : X -> [X])
;; Given a natural number, this function returns a function which will take one input and return a list which
;; contains that input repeated as many times as the given natural number.
;;; Example-usage (repeat-test)
;;  ;; > (map (repeat 3) (range 1 4))
;;  ;; '((1 1 1) (2 2 2) (3 3 3))
(define (repeat-test)
  (let ((result (map (repeat 3) (range 1 4)))
        (target '((1 1 1) (2 2 2) (3 3 3))))
    (if (equal? result target)
        #t
        (list 'got result 'expected target))))
(define (repeat n)
  (lambda (x)
    (map (lambda (y) x) (range 0 n))))

;;;; (numbers) -> number
;; Given a list of numbers, this function adds them all and returns the sum.
;;; Example-usage (sum-test)
;;  ;; > (sum (range 1 10))
;;  ;; 45
(define (sum-test)
  (let ((result (sum (range 1 10)))
        (target 45))
    (if (= result target)
        #t
        (list 'got result 'expected target))))
(define (sum a)
  (reduce + 0 a))

;;;; (numbers) -> number
;; Given a list of numbers, this function multiplies them all and returns the product.
;;; Example-usage (product-test)
(define (product-test)
  (let ((result (product (range 1 10)))
        (target 362880))
    (if (= result target)
        #t
        (list 'got result 'expected target))))
(define (product a)
  (reduce * 1 a))

;;;; (natural-number) -> natural-number
;; Given a natural-number, this function returns the factorial of that number, viz. the product of the numbers
;; from 1 to that number inclusive.
;;; Example-usage (factorial-test)
;;  ;; > (map factorial (range 1 7))
;;  ;; '(1 2 6 24 120 720)
(define (factorial-test)
  (let ((result (map factorial (range 1 7)))
        (target '(1 2 6 24 120 720)))
    (if (equal? result target)
        #t
        (list 'got result 'expected target))))
(define (factorial n)
  (product (range 1 (+ n 1))))

;;;; (function integer integer) -> list
;; Given a function which takes an integer as an input, and two integers a & b where a <= b, this function
;; returns a list of the image of that function on the range [a, b), viz. f[a, b).
;;; Example-usage (sequence-test)
;;  ;; (sequence (lambda (n) (* 3 (square n))) 0 10)
;;  ;; '(0 3 12 27 48 75 108 147 192 243)
(define (sequence-test)
  (let ((result (sequence (lambda (n) (* 3 (square n))) 0 10))
        (target '(0 3 12 27 48 75 108 147 192 243)))
    (if (equal? result target)
        #t
        (list 'got result 'expected target))))
(define (sequence f a b)
  (map f (range a b)))

;;;; (function integer integer) -> number
;; This function returns the sum of the sequence resulting from the given function and two integers.
;;; Example-usage (series-test)
;;  ;; > (series (lambda (n) (/ (expt 2 n) (factorial n))) 0 10)
;;  ;; 20947/2835
(define (series-test)
  (let ((result (series (lambda (n) (/ (expt 2 n) (factorial n))) 0 10))
        (target 20947/2835))
    (if (= result target)
        #t
        (list 'got result 'expected target))))
(define (series f a b)
  (sum (sequence f a b)))

;;;; (function list) -> list
;; Given a function which returns either true or false given any input, and a list of elements which can each
;; be given to that function as inputs, this function filters out from that list all elements which the given
;; function maps to false, returning a list of only those elements which the given function maps to true.
;; In terms of sets, the result of this function is the intersection of the original set O with the fiber of
;; true on f; viz. O intersect f^-1{true}.
;;; Example-usage (filter-test)
;;  ;; > (filter even? (range 1 20))
;;  ;; '(2 4 6 8 10 12 14 16 18)
(define (filter-test)
  (let ((result (filter even? (range 1 20)))
        (target '(2 4 6 8 10 12 14 16 18)))
    (if (equal? result target)
        #t
        (list 'got result 'expected target))))
(define (filter f a)
  (define (filter-iter lst accum)
    (cond ((null? lst)   (reverse accum))
          ((f (car lst)) (filter-iter (cdr lst) (cons (car lst) accum)))
          (else          (filter-iter (cdr lst) accum))))
  (filter-iter a '()))

;;;; (function list) -> boolean
;; Given a function which returns either true or false given any input, and a list of inputs, this function
;; returns true if and only if the given function maps every element of the input list to true. That is to say,
;; this function returns true iff f(A) = {true}, where A is the given list.
;;; Example-usage (all-test)
;;  ;; > (all even? '(2 4 6 8 10 14 12 16 3 18 20))
;;  ;; #f
(define (all-test)
  (let ((result (all even? '(2 4 6 8 10 14 12 16 3 18 20)))
        (target #f))
    (if (equal? result target)
        #t
        (list 'got result 'expected target))))
(define (all f a)
  (reduce (lambda (a b) (and a b)) #t (map f a)))

;;;; (function list) -> boolean
;; Given a function which returns either true or false given any input, and a list of inputs, this function
;; returns true if and only if there exists at least one element in the input list which the given function
;; maps to true. That is to say, this function returns true iff true \in f(A), where A is the given list.
;;; Example-usage (any-test)
;;  ;; > (any odd? '(2 4 6 8 10 14 12 16 3 18 20))
;;  ;; #t
(define (any-test)
  (let ((result (any odd? '(2 4 6 8 10 14 12 16 3 18 20)))
        (target #t))
    (if (equal? result target)
        #t
        (list 'got result 'expected target))))
(define (any f a)
  (reduce (lambda (a b) (or a b)) #f (map f a)))

;;;; (functions) -> function
;; Given a list of functions, this function returns the function representing the composition of all
;; those functions.
;;; Example-usage (compose-test)
;;  ;; > ((compose (list square log exp sqrt)) 1)
;;  ;; 1
(define (compose-test)
  (let ((result ((compose (list square log exp sqrt)) 1))
        (target 1))
    (if (= result target)
        #t
        (list 'got result 'expected target))))
(define (compose a)
  (define (compose-pair f g)
    (lambda (x)
      (f (g x))))
  (define (identity x)
    x)
  (reduce compose-pair identity (reverse a)))

;;;; (number) -> number
;; Given a number, this function returns the product of that number with itself.
;;; Example-usage (square-test)
;;  ;; > (square 4)
;;  ;; 16
(define (square-test)
  (let ((result (square 4))
        (target 16))
    (if (= result target)
        #t
        (list 'got result 'expected target))))
(define (square x)
  (* x x))

;;;; (list) -> (list list)
;; Given a list, this function returns a list of two lists, each containing one half of the original
;; list. If the list has an odd number of elements, the second half will be the longer half.
;;; Example-usage (halve-test)
;;  ;; > (halve (range 1 4))
;;  ;; '((1) (2 3))
(define (halve-test)
  (let ((result (halve (range 1 4)))
        (target '((1) (2 3))))
    (if (equal? result target)
        #t
        (list 'got result 'expected target))))
(define (halve a)
  (define (halve-iter end right left)
    (if (or (null? end) (null? (cdr end)))
        (list (reverse left) right)
        (halve-iter (cddr end) (cdr right) (cons (car right) left))))
  (halve-iter a a '()))

;;;; (list) -> list
;; Given a list of numbers, this function returns that list sorted using the merge-sort algorithm.
;;; Example-usage (merge-sort-test)
;;  ;; > (merge-sort '(1 6 5 3 0 4 9 7 3 6)
;;  ;; '(0 1 3 3 4 5 6 6 7 9)
(define (merge-sort-test)
  (let ((result (merge-sort '(1 6 5 3 0 4 9 7 3 6)))
        (target '(0 1 3 3 4 5 6 6 7 9)))
    (if (equal? result target)
        #t
        (list 'got result 'expected target))))
(define (merge-sort a)
  (define (merge a)
    (define (merge-iter x y accum)
      (cond ((null? x) (append (reverse y) accum))
            ((null? y) (append (reverse x) accum))
            ((< (car x) (car y)) (merge-iter (cdr x) y (cons (car x) accum)))
            (else                (merge-iter x (cdr y) (cons (car y) accum)))))
    (reverse (merge-iter (car a) (cadr a) '())))
  (if (or (null? a) (null? (cdr a)))
      a
      (merge (map merge-sort (halve a)))))

;;;; (tree) -> list
;; Given a tree with at most two children at each node, this function returns a
;; list of the values of the nodes in the tree.
;;; Example-usage (values-test)
;;  ;; > (values '(a (b) (c (d (d2)) (e (f) (g)))))
;;  ;; '(a b c d d2 e f g)
(define (values-test)
  (let ((result (values '(a (b) (c (d (d2)) (e (f) (g))))))
        (target '(a b c d d2 e f g)))
    (if (equal? result target)
        #t
        (list 'got result 'expected target))))
(define (values tree)
  (cond ((null? tree) '())
        (else (cons (car tree)
                    (append (values (lhs tree))
                            (values (rhs tree)))))))

;;;; (pair) -> item
;; This function is a safer version of car, which returns the first element of a pair.
;; If the given pair is null, instead of an error being thrown, null is returned.
;;; Example-usage
;;  ;; > (scar '())
;;  ;; '()
(define (scar-test)
  (let ((result (scar '()))
        (target '()))
    (if (equal? result target)
        #t
        (list 'got result 'expected target))))
(define (scar x)
  (if (null? x)
      x
      (car x)))

;;;; (pair) -> item
;; This function is a safer version of cdr, which returns the second element of a pair,
;; generally being another pair itself. If the given pair is null, this function returns
;; null instead of throwing an error.
;;; Example-usage (scdr-test)
;;  ;; > (scdr '())
;;  ;; '()
(define (scdr-test)
  (let ((result (scdr '()))
        (target '()))
    (if (equal? result target)
        #t
        (list 'got result 'expected target))))
(define (scdr x)
  (if (null? x)
      x
      (cdr x)))

;;;; (val left-child right-child) -> left-child
;; This function returns the left child of a tree, or null if the given tree is null.
;;; Example-usage (lhs-test)
;;  ;; > (lhs-test '(val left right)
;;  ;; 'left
(define (lhs-test)
  (let ((result (lhs '(val left right)))
        (target 'left))
    (if (equal? result target)
        #t
        (list 'got result 'expected target))))
(define (lhs tree)
  (scar (scdr tree)))

;;;; (val left-child right-child) -> right-child
;; This function returns the right child of a tree, or null if the given tree is null.
;;; Example-usage (rhs-test)
;;  ;; > (rhs-test '(val left right)
;;  ;; 'right
(define (rhs-test)
  (let ((result (rhs '(val left right)))
        (target 'right))
    (if (equal? result target)
        #t
        (list 'got result 'expected target))))
(define (rhs tree)
  (scar (scdr (scdr tree))))

;;;; (val left-child right-child) -> (val right-child left-child)
;; Given a tree, this function returns a new tree where the left and right children of
;; the given node are reversed, with the same value.
;;; Example-usage (flip-test)
;;  ;; > (flip '(val left right))
;;  ;; '(val right left)
(define (flip-test)
  (let ((result (flip '(val left right)))
        (target '(val right left)))
    (if (equal? result target)
        #t
        (list 'got result 'expected target))))
(define (flip tree)
  (list (scar tree) (rhs tree) (lhs tree)))

;;;; (key val lst) -> lst
;; Given a key, a value, and a list, this function adds the pair (key . value) to the
;; beginning of the given list, which if used to make an entire list, will create an
;; association list compatible with the assoc function.
;; More explicitly:
;; Given a key k0, a value in a list '(v0), and an association list of the form
;;  '((k1 v1) (k2 v2) ... (kn vn))
;; this returns the association list
;;  '((k0 v0) (k1 v1) (k2 v2) ... (kn vn))
;;; Example-usage (acons-test)
;;  ;; > (acons 'Rome 'Latin '((Athens . Greek)))
;;  ;; '((Rome . Latin) (Athens . Greek))
(define (acons-test)
  (let ((result (acons 'Rome 'Latin '((Athens . Greek))))
        (target '((Rome . Latin) (Athens . Greek))))
    (if (equal? result target)
        #t
        (list 'got result 'expected target))))
(define (acons x y a)
  (cons (cons x y) a))

;;;; (key assoc-list) -> assoc-list
;; Given a key in an association list, and that list, this function returns the given
;; list with all its elements except for the pair with the provided key. Note, that if
;; the given key does not appear in the list, the list is returned unchanged.
;; More explicitly:
;; Given a key kj, and an association list of the form
;;   '((k1 v1) (k2 v2) ... (kj vj) ... (kn vn))
;; this returns the association list with every element except (kj vj)
;;; Example-usage (assoc-remove-test)
;;  ;; > (assoc-remove 'America '((America . American) (Rome . Latin) (Athens . Greek)))
;;  ;; '((Rome . Latin) (Athens . Greek))
(define (assoc-remove-test)
  (let ((result (assoc-remove 'America '((America . American) (Rome . Latin) (Athens . Greek))))
        (target '((Rome . Latin) (Athens . Greek))))
    (if (equal? result target)
        #t
        `(got ,result expected ,target))))
(define (assoc-remove x a)
  (define (assoc-remove-iter x a accum)
    (cond ((null? a) (reverse accum))
          ((equal? (caar a) x) (append (reverse accum) (cdr a)))
          (else (assoc-remove-iter x (cdr a) (cons (car a) accum)))))
  (assoc-remove-iter x a '()))

;;;; (string string) -> integer
;; Given two strings, -1 is returned if the first is less than the second, by comparing
;; contained characters from first to last by ASCII value.
;;; Example-usage (string-cmp-test)
(define (string-cmp-test)
  (let ((result (string-cmp "first" "second"))
        (target -1))
    (if (= result target)
        #t
        `(got ,result expected ,target))))
(define (string-cmp a b)
  (cond ((equal? a b) 0)
        ((string<? a b) -1)
        (else 1)))

;;;; x -> x
;; Given any input, this function returns it unaltered.
;;; Example-usage (id-test)
(define (id-test)
  (let ((result (id `(asd ,(+ 2 3) 5)))
        (target '(asd 5 5)))
    (if (equal? result target)
        #t
        (list 'got result 'expected target))))
(define (id x) x)

;;;; list -> f : element -> list
;; Given a black-list, this function returns a function which will take an input item
;; and return true if the given item is not in the black-list.
;;; Example-usage (not-in-test)
;;  ;; > '(filter (not-in '(a b c d e f g)) '(a b c 1 2 3 x y z e f g))
;;  ;; '(1 2 3 x y z)
(define (not-in-test)
  (let ((result (filter (not-in '(a b c d e f g)) '(a b c 1 2 3 x y z e f g)))
        (target '(1 2 3 x y z)))
    (if (equal? result target)
        #t
        `(got ,result expected ,target))))
(define (not-in black-list)
  (lambda (elem)
    (false? (member elem black-list))))
