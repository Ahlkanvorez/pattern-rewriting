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

(provide bfs dfs)

;;;;
;; TODO: Comment
;;;
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

;;;;
;; TODO: Comment
;;;
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
