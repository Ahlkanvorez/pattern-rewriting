;;;
;; Author: Robert Mitchell <robert.mitchell36@gmail.com>
;; Edit log:
;; - 1/25/2015 (prototype)
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

(provide bfs dfs simple-bfs simple-dfs)

;;;; (val (val -> boolean) (val -> val) (val -> boolean) (val -> list) (val list -> list)) -> val
;; Given a value for the root node, a function which indicates whether the target value/node
;; has been found, a function which alters the target value before returning, a function which
;; indicates whether a child node is worth exploring, a function which given a value/node will
;; return a list of new values/nodes to consider, and a function which given the current node
;; and the list of previously visited nodes will record the current node in that list, this
;; function will search for the target value using the Breadth-First Search algorithm, which will
;; exhaust the new nodes given by each call of the 'children' function before calling it again on
;; the first value given in the list resulting from the call, until either no unseen values remain,
;; or the target is found.
;;; Example-usage (bfs-test)
;;; This extended example uses bfs and the expression-evaluation library to find the simplest form
;;; representing any integer using the Successor function (+ 1 x) and the Negation function (- x),
;;; relative to the number 0. The output is the proper, although not unique, way to represent an
;;; integer using such functions, starting from whatever reference integer is desired, with a list
;;; of the shortest number of operations to arrive at that integer.
;;; NOTE: This test requires exp-eval.rkt
;;  ;; > (let* ((S (lambda (x) (+ 1 x)))
;;  ;;          (N (lambda (x) (- x)))
;;  ;;          (evaluate (lambda (exp) (exp-eval (list `((S u) ,S) `((N u) ,N)) '() exp))))
;;  ;;     (bfs '((S 0))
;;  ;;          (lambda (x) (= (evaluate (car x)) -3))
;;  ;;          (lambda (x visited) (cons (evaluate (car x)) x))
;;  ;;          (lambda (lst)
;;  ;;            (lambda (x)
;;  ;;              ((not-in lst) (car x))))
;;  ;;          (lambda (x) (list (list `(S ,(car x)) x)
;;  ;;                            (list `(N ,(car x)) x)))
;;  ;;          (lambda (x lst) (cons (car x) lst))))
;;  ;; '(-3 (N (S (S (S 0)))) ((S (S (S 0))) ((S (S 0)) ((S 0)))))
(require "exp-eval.rkt")
(define (bfs-test)
  (let ((result (let* ((S (lambda (x) (+ 1 x)))
                       (N (lambda (x) (- x)))
                       (evaluate (lambda (exp) (exp-eval (list `((S u) ,S) `((N u) ,N)) '() exp))))
                  (bfs '((S 0))
                       (lambda (x) (= (evaluate (car x)) -3))
                       (lambda (x visited) (cons (evaluate (car x)) x))
                       (lambda (lst)
                         (lambda (x)
                           ((not-in lst) (car x))))
                       (lambda (x) (list (list `(S ,(car x)) x)
                                         (list `(N ,(car x)) x)))
                       (lambda (x lst) (cons (car x) lst)))))
        (target '(-3 (N (S (S (S 0)))) ((S (S (S 0))) ((S (S 0)) ((S 0)))))))
    (if (equal? result target)
        #t
        `(got ,result expected ,target))))
(define (bfs node target? alter-target good? children remember)
  (define (bfs-iter node queue visited)
    (if (target? node)
        (alter-target node visited)
        (let* ((kids (filter (good? visited) (children node)))
               (new-queue (append queue kids)))
          (if (null? new-queue)
              '()
              (bfs-iter (car new-queue) (cdr new-queue) (remember node visited))))))
  (bfs-iter node (filter (good? '()) (children node)) '()))

;;;; (val (val -> boolean) (val -> val) (val -> boolean) (val -> list)) -> val
;; Given a value for the root node, a function which indicates whether the target value/node
;; has been found, a function which alters the target value before returning, a function which
;; indicates whether a child node is worth exploring, a function which given a value/node will
;; return a list of new values/nodes to consider, and a function which given the current node
;; and the list of previously visited nodes will record the current node in that list, this
;; function will search for the target value using the Depth-First Search algorithm, which will
;; apply each function continuously until the options from that function are exhausted, before
;; considering the other functions starting from the new value reached by the first functions.
;; In other words, new values are added to a stack of values to check.
;;; Example-usage (dfs-test)
;;; This extended example finds the prime factorization of the given natural number, where the given
;;; natural number has no prime factors greater than 41 (although it could easily be adapted to
;;; support arbitrary natural numbers). Because the depth first algorithm is used, wasteful searches
;;; with primes that are not factors of the number are quickly discarded, and primes which are a
;;; factor are quickly used as many times as their multiplicity in the factorization.
;;  ;; > (dfs (list 35)
;;  ;;        (lambda (x) (= (car x) 1))
;;  ;;        (lambda (val lst) val)
;;  ;;        (lambda (visited)
;;  ;;          (lambda (x) (and (= 35 (product x)) ((not-in visited) x))))
;;  ;;        (lambda (x)
;;  ;;          (let ((rem (car x)))
;;  ;;            (map (lambda (p) (cons (quotient rem p) (cons p (cdr x))))
;;  ;;                 '(2 3 5 7 11 13 17 19 23 29 31 37 41)))))
;;  ;; '(1 7 5)
(define (dfs-test)
  (define (prime-factorization n)
    (dfs (list n)
        (lambda (x) (= (car x) 1))
        (lambda (val lst) val)
        (lambda (visited)
          (lambda (x) (and (= n (product x)) ((not-in visited) x))))
        (lambda (x)
          (let ((rem (car x)))
            (map (lambda (p) (cons (quotient rem p) (cons p (cdr x))))
                 '(2 3 5 7 11 13 17 19 23 29 31 37 41))))))
  (let ((result (map prime-factorization (range 1 42)))
        (target '((1)  (1 2)  (1 3)  (1 2 2)  (1 5)  (1 3 2)  (1 7)  (1 2 2 2)  (1 3 3)  (1 5 2)  (1 11)  (1 3 2 2)  (1 13)  (1 7 2)  (1 5 3)  (1 2 2 2 2)  (1 17)  (1 3 3 2)  (1 19)  (1 5 2 2)  (1 7 3)  (1 11 2)  (1 23)  (1 3 2 2 2)  (1 5 5)  (1 13 2)  (1 3 3 3)  (1 7 2 2)  (1 29)  (1 5 3 2)  (1 31)  (1 2 2 2 2 2)  (1 11 3)  (1 17 2)  (1 7 5)  (1 3 3 2 2)  (1 37)  (1 19 2)  (1 13 3)  (1 5 2 2 2)  (1 41))))
    (if (equal? result target)
        #t
        `(got ,result expected ,target))))
(define (dfs node target? alter-target good? children)
  (define (dfs-iter node stack visited)
    (if (target? node)
        (alter-target node visited)
        (let* ((kids (filter (good? visited) (children node)))
               (new-stack (append kids stack)))
          (if (null? new-stack)
              '()
              (dfs-iter (car new-stack) (cdr new-stack) (cons node visited))))))
  (dfs-iter node '() '()))

;;;; (val (val -> boolean) (val -> list)) -> val
;; Given a value for the root node, a function which indicates whether the target value/node
;; has been found, and a function which given a value/node will return a list of new values/nodes
;; to consider, this function will search for the target value using the Depth-First Search
;; algorithm, which will apply each function continuously until the options from that function
;; are exhausted, before considering the other functions starting from the new value reached by
;; the first functions. In other words, new values are added to a stack of values to check.
;;; Example-usage (simple-dfs-test)
;;  ;; > (simple-dfs 100 (lambda (x) (= x 5)) (lambda (x) (list (quotient x 5) (quotient x 2))))
;;  ;; '5
(define (simple-dfs-test)
  (let ((result (simple-dfs 100 (lambda (x) (= x 5)) (lambda (x) (list (quotient x 5) (quotient x 2)))))
        (target 5))
    (if (= result target)
        #t
        `(got ,result expected ,target))))
(define (simple-dfs node target? children)
  (define (simple-dfs-iter node stack visited)
    (if (target? node)
        node
        (let ((new-stack (append (filter (not-in visited) (children node)) stack)))
          (if (null? new-stack)
              '()
              (simple-dfs-iter (scar new-stack) (scdr new-stack) (cons node visited))))))
  (simple-dfs-iter node '() '()))

;;;; (val (val -> boolean) (val -> list)) -> val
;; Given a value for the root node, a function which indicates whether the target value/node
;; has been found, and a function which given a value/node will return a list of new values/nodes
;; to consider, this function will search for the target value using the Breadth-First Search
;; algorithm, which will exhaust the new nodes given by each call of the 'children' function
;; before calling it again on the first value given in the list resulting from the call, until
;; either no unseen values remain, or the target is found. In other words, new values are added
;; to a queue of values to check.
;;; Example-usage (simple-bfs-test)
;;  ;; > (simple-bfs 1 (lambda (x) (and (< 40 x) (< x 50))) (lambda (x) (list (* x 2) (* x 3))))
;;  ;; 48
(define (simple-bfs-test)
  (let ((result (simple-bfs 1 (lambda (x) (and (< 40 x) (< x 50))) (lambda (x) (list (* x 2) (* x 3)))))
        (target 48))
    (if (equal? result target)
        #t
        `(got ,result expected ,target))))
(define (simple-bfs node target? children)
  (define (simple-bfs-iter node queue visited)
    (if (target? node)
        node
        (let ((new-queue (append queue (filter (not-in visited) (children node)))))
          (if (null? new-queue)
              '()
              (simple-bfs-iter (scar new-queue) (scdr new-queue) (cons node visited))))))
  (simple-bfs-iter node '() '()))
