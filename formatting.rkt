#lang racket


(provide indent build-line)

(define (indent indent-level s)
  (string-append (make-string (* 4 indent-level) #\ ) s))

(define (build-line key value show-comma is-wrapper)
  (define s (if (and (not is-wrapper) (symbol? key) (string? value)) "\"" ""))
  (define comma (if show-comma "," ""))
  
  (if (symbol? key)
      (format "\"~a\" : ~a~a~a~a" (symbol->string key) s value s comma)
      (format "~a~a" value comma)
      ))
