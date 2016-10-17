#lang racket

(require typed-stack)


(provide make-stack-frame start-session current-environment push-stack-frame set-current-lens get-current-lens)

(define (make-stack-frame path authors-panel publishers-panel)
  (define stack-frame (make-hash))
  (hash-set! stack-frame "path" path)
  (hash-set! stack-frame "authors-panel" authors-panel)
  (hash-set! stack-frame "publishers-panel" publishers-panel)
  (hash-set! stack-frame "authors-results" (make-hash))
  (hash-set! stack-frame "publishers-results" (make-hash))
  
  stack-frame)



(define current-lens '())
(define current-session '(null))

(define (start-session environment)
  (unless (eq? environment (current-environment))
    (set! current-session (list environment (make-stack)))))


(define (current-environment)
  (first current-session))

(define (push-stack-frame frame)
  (push! (second current-session) frame))


(define (set-current-lens lens)
  (set! current-lens lens))

(define (get-current-lens)
  current-lens)